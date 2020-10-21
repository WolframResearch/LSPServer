BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the client.\
 StartServer[logDir] logs traffic to logDir."

RunServerDiagnostic


expandContent

handleContent



$Debug

$Debug2

$DebugBracketMatcher


$PreExpandContentQueue

$ContentQueue

$OpenFilesMap

$CancelMap

$hrefIdCounter


$ImplicitTokens


$BracketMatcher

$BracketMatcherDisplayInsertionText

$BracketMatcherUseDesignColors


$ConfidenceLevel


$HTMLSnippets


$ML4CodeTimeLimit


Begin["`Private`"]

Needs["LSPServer`BracketMismatches`"]
Needs["LSPServer`CodeAction`"]
Needs["LSPServer`Color`"]
Needs["LSPServer`Definitions`"]
Needs["LSPServer`Diagnostics`"]
Needs["LSPServer`DocumentSymbol`"]
Needs["LSPServer`Formatting`"]
Needs["LSPServer`Hover`"]
Needs["LSPServer`ImplicitTokens`"]
Needs["LSPServer`Library`"]
Needs["LSPServer`References`"]
Needs["LSPServer`ServerDiagnostics`"]
Needs["LSPServer`Utils`"]
Needs["LSPServer`Workspace`"]

Needs["CodeFormatter`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`ImplicitTokens`"]
Needs["CodeInspector`BracketMismatches`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

Needs["PacletManager`"] (* for PacletInformation *)


(*
TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "LongNames"]
*)
location = "Location" /. PacletInformation["LSPServer"];

processedSymbolsFile = FileNameJoin[{location, "Resources", "Generated", "processedSymbols.mx"}];

Get[processedSymbolsFile]


(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]



$DefaultConfidenceLevel = 0.95

$CodeActionLiteralSupport = False

$ImplicitTokens = False

(*
if $BracketMatcher, then load ML4Code` and use ML bracket matching tech
*)
$BracketMatcher = False

$BracketMatcherUseDesignColors = True

$BracketMatcherDisplayInsertionText = False

(*
Bracket suggestions from ML4Code can take O(n^2) time in the size of the chunk, so make sure to
have a time limit 
*)
$ML4CodeTimeLimit = 0.4


$ExecuteCommandProvider = <|
  "commands" -> {
    "enable_bracket_matcher_debug_mode",
    "disable_bracket_matcher_debug_mode",
    "enable_bracket_matcher_design_colors",
    "disable_bracket_matcher_design_colors",
    "enable_bracket_matcher_display_insertion_text",
    "disable_bracket_matcher_display_insertion_text"
  } |>

$HTMLSnippets = False




(*
lint objects may be printed to log files and we do not want to include ANSI control codes
*)
CodeInspector`Format`Private`$UseANSI = False


(*
The counter that is used for creating unique hrefs
*)
$hrefIdCounter = 0



$ErrorCodes = <|
  (*
  Defined by JSON RPC
  *)
  "ParseError" -> -32700,
  "InvalidRequest" -> -32600,
  "MethodNotFound" -> -32601,
  "InvalidParams" -> -32602,
  "InternalError" -> -32603,
  "serverErrorStart" -> -32099,
  "serverErrorEnd" -> -32000,
  "ServerNotInitialized" -> -32002,
  "UnknownErrorCode" -> -32001,

  (*
  Defined by the protocol.
  *)
  "RequestCancelled" -> -32800,
  "ContentModified" -> -32801
|>


$TextDocumentSyncKind = <|
  "None" -> 0,
  "Full" -> 1,
  "Incremental" -> 2
|>



$ContentQueue = {}

$OpenFilesMap = <||>

$CancelMap = <||>



Options[StartServer] = {
	ConfidenceLevel :> Automatic
}

(*
setup the REPL to handle traffic from client
*)
StartServer[logDir_String:"", OptionsPattern[]] :=
Catch[
Catch[
Module[{logFile, res, bytes, bytess, logFileStream,
  logFileName, logFileCounter,
  lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion,
  content, contents,
  errStr, ferror},

  $ConfidenceLevelOption = OptionValue[ConfidenceLevel];


  $MessagePrePrint =.;

  (*
  Ensure that no messages are printed to stdout
  *)
  $Messages = Streams["stderr"];

  (*
  Ensure that no Print output is printed to stdout

  There may have been messages printed from doing Needs["LSPServer`"], and we can't do anything about those
  But they will be detected when doing RunServerDiagnostic[] 
  *)
  $Output = Streams["stderr"];

  (*
  res = RunServerDiagnostic[];
  If[!TrueQ[res],
    Write[$Messages, "\n\n" //OutputForm];
    Write[$Messages, "RunDiagnostic[] did not return True: " //OutputForm, res //OutputForm];
    Write[$Messages, "KERNEL IS EXITING" //OutputForm];
    Write[$Messages, "\n\n" //OutputForm];
    Pause[1];Exit[1]
  ];
  *)

  $Debug = (logDir != "");

  If[$Debug,

    Quiet[CreateDirectory[logDir], {CreateDirectory::filex}];

    logFileCounter = 1;
    logFileName = "kernelLog-ppid" <> ToString[$ParentProcessID] <> "-pid" <> ToString[$ProcessID] <> "-";
    logFile = FileNameJoin[{logDir, logFileName <> ToString[logFileCounter] <> ".txt"}];

    While[True,
      If[FileExistsQ[logFile],
        logFileCounter++;
        logFile = FileNameJoin[{logDir, logFileName <> ToString[logFileCounter] <> ".txt"}]
        ,
        Break[]
      ]
    ];

    logFileStream = OpenWrite[logFile, CharacterEncoding -> "UTF8"];

    $Messages = $Messages ~Join~ { logFileStream }
  ];

  (*
  Previously tried setting CharacterEncoding -> "UTF-8", but seems to have no effect

  Maybe because stream is already open and being written to?

  TODO: look into any bug reports about setting CharacterEncoding for $Messages
  *)
  SetOptions[$Messages, PageWidth -> Infinity];

  (*
  There may be messages that we want to see

  TODO: investigate resetting the General::stop counter at the start of each eval loop
  *)
  Off[General::stop];


  log["$CommandLine: ", $CommandLine];
  log["\n\n"];

  log["$ProcessID: ", $ProcessID];
  log["\n\n"];

  If[!StringStartsQ[ToLowerCase[FileBaseName[$CommandLine[[1]]]], "wolframkernel"],
    log["WARNING: Command for Wolfram Language Server does not start with 'WolframKernel': ", $CommandLine[[1]]];
    log["\n\n"]
  ];
  If[!MemberQ[$CommandLine, "-noinit"],
    log["WARNING: -noinit is not in $CommandLine"];
    log["\n\n"]
  ];
  If[!MemberQ[$CommandLine, "-noprompt"],
    log["WARNING: -noprompt is not in $CommandLine"];
    log["\n\n"]
  ];
  If[!MemberQ[$CommandLine, "-nopaclet"],
    log["WARNING: -nopaclet is not in $CommandLine"];
    log["\n\n"]
  ];
  If[!MemberQ[$CommandLine, "-noicon"],
    log["WARNING: -noicon is not in $CommandLine"];
    log["\n\n"]
  ];


  lspServerVersion = Information[PacletObject["LSPServer"], "Version"];

  codeParserVersion = Information[PacletObject["CodeParser"], "Version"];

  codeInspectorVersion = Information[PacletObject["CodeInspector"], "Version"];

  codeFormatterVersion = Information[PacletObject["CodeFormatter"], "Version"];

  If[lspServerVersion =!= codeParserVersion,
    log["WARNING: LSPServer and CodeParser do not have the same version."];
    log["WARNING: LSPServer version: ", lspServerVersion];
    log["WARNING: CodeParser version: ", codeParserVersion];
    log["\n\n"]
  ];
  If[lspServerVersion =!= codeInspectorVersion,
    log["WARNING: LSPServer and CodeInspector do not have the same version."];
    log["WARNING: LSPServer version: ", lspServerVersion];
    log["WARNING: CodeInspector version: ", codeInspectorVersion];
    log["\n\n"]
  ];
  If[lspServerVersion =!= codeFormatterVersion,
    log["WARNING: LSPServer and CodeFormatter do not have the same version."];
    log["WARNING: LSPServer version: ", lspServerVersion];
    log["WARNING: CodeFormatter version: ", codeFormatterVersion];
    log["\n\n"]
  ];


  (*
  Some simple thing to warm-up
  *)
  CodeParse["1+1"];


  log["Starting server... (If this is the last line you see, then there may be a problem and the server is hanging.)"];
  log["\n\n"];


  StartBackgroundReaderThread[];


  (*
  loop over:
    read content
    evaluate
    write content
  *)
  While[True,

    TryQueue[];

    If[empty[$ContentQueue],
      Pause[0.1];
      Continue[]
    ];

    content = $ContentQueue[[1]];
    $ContentQueue = Rest[$ContentQueue];

    If[$Debug2,
      log["taking first from $ContentQueue: ", #["method"]&[content]];
      log["rest of $ContentQueue (up to 20): ", Take[#["method"]& /@ $ContentQueue, UpTo[20]]];
      log["..."]
    ];

    contents = LSPEvaluate[content];

    Check[
      bytess = ExportByteArray[#, "JSON"]& /@ contents

      ,
      log["\n\n"];
      log["message generated by contents: ", contents];
      log["\n\n"]
      ,
      {Export::jsonstrictencoding}
    ];

    (*
    write out each byte array in bytess
    *)
    Do[
    
      If[!ByteArrayQ[bytes],
        
        log["\n\n"];
        log["invalid bytes: ", bytes];
        log["\n\n"];

        exitHard[]
      ];

      (*
      Write the headers
      *)
      Do[
        If[$Debug2,
          log[""];
          log["C<--S  ", line]
        ];

        res = WriteLineToStdOut[line];
        If[res =!= 0,

          Switch[res,
            $LSPServerLibraryError["FWRITE_FAILED"] | $LSPServerLibraryError["FFLUSH_FAILED"],
              Which[
                GetStdOutFEOF[] != 0,
                  errStr = "fwrite EOF"
                ,
                (ferror = GetStdOutFError[]) != 0,
                  errStr = "fwrite error: " <> ToString[ferror]
                ,
                True,
                  errStr = "fwrite unknown error"
              ]
            ,
            _,
              errStr = "UNKNOWN ERROR: " <> ToString[res]
          ];

          log["\n\n"];
          log["StdOut error: ", errStr];
          log["\n\n"];

          If[TrueQ[$ServerState == "shutdown"],
            exitSemiGracefully[]
            ,
            exitHard[]
          ]
        ]
        ,
        {line, {"Content-Length: " <> ToString[Length[bytes]], ""}}
      ];

      (*
      Write the body
      *)
      If[$Debug2,
        log["C<--S  ", stringLineTake[FromCharacterCode[Normal[Take[bytes, UpTo[1000]]]], UpTo[20]]];
        log["...\n"]
      ];

      res = WriteBytesToStdOut[bytes];
      If[res =!= 0,

        Switch[res,
          $LSPServerLibraryError["FWRITE_FAILED"] | $LSPServerLibraryError["FFLUSH_FAILED"],
            Which[
              GetStdOutFEOF[] != 0,
                errStr = "fwrite EOF"
              ,
              (ferror = GetStdOutFError[]) != 0,
                errStr = "fwrite error: " <> ToString[ferror]
              ,
              True,
                errStr = "fwrite unknown error"
            ]
          ,
          _,
            errStr = "UNKNOWN ERROR: " <> ToString[res]
        ];

        log["\n\n"];
        log["StdOut error: ", errStr];
        log["\n\n"];

        If[TrueQ[$ServerState == "shutdown"],
          exitSemiGracefully[]
          ,
          exitHard[]
        ]
      ]
      ,
      {bytes, bytess}
    ](*Do bytess*)

  ](*While*)
]],(*Module*)
_,
(
  log["\n\n"];
  log["uncaught Throw: ", #1];
  log["\n\n"];
  
  exitHard[]

  )&
]


TryQueue[] :=
  Catch[
  Module[{bytes,
    queueSize, frontMessageSize,
    content,
    bytessIn, contentsIn,
    backgroundReaderThreadError, errStr, ferror,
    lastContentsIn},

    backgroundReaderThreadError = GetBackgroundReaderThreadError[];

    If[backgroundReaderThreadError != 0,

      Switch[backgroundReaderThreadError,
        $LSPServerLibraryError["FREAD_FAILED"],
          Which[
            GetStdInFEOF[] != 0,
              errStr = "fread EOF"
            ,
            (ferror = GetStdInFError[]) != 0,
              errStr = "fread error: " <> ToString[ferror]
            ,
            True,
              errStr = "fread unknown error"
          ]
        ,
        $LSPServerLibraryError["UNEXPECTED_LINEFEED"],
          errStr = "unexpected linefeed"
        ,
        $LSPServerLibraryError["EXPECTED_LINEFEED"],
          errStr = "expected linefeed"
        ,
        $LSPServerLibraryError["UNRECOGNIZED_HEADER"],
          errStr = "unrecognized header"
        ,
        _,
          errStr = "UNKNOWN ERROR: " <> ToString[backgroundReaderThreadError]
      ];

      log["\n\n"];
      log["Background Reader Thread Error: ", errStr];
      log["\n\n"];

      If[TrueQ[$ServerState == "shutdown"],
        exitSemiGracefully[]
        ,
        exitHard[]
      ]
    ];

    LockQueue[];

    queueSize = GetQueueSize[];

    If[queueSize == 0,
      
      UnlockQueue[];

      Throw[Null]
    ];

    If[$Debug2,
      log["\n\n"];
      log["messages in queue: ", queueSize];
      log["\n\n"]
    ];

    bytessIn = {};
    Do[
      
      frontMessageSize = GetFrontMessageSize[];

      bytes = PopQueue[frontMessageSize];

      AppendTo[bytessIn, bytes]
      ,
      queueSize
    ];

    UnlockQueue[];

    contentsIn = {};
    Do[
      If[FailureQ[bytesIn],
        log["\n\n"];
        log["invalid bytes from stdin: ", bytesIn];
        log["\n\n"];

        exitHard[]
      ];

      If[$Debug2,
        log["C-->S " <> ToString[Length[bytesIn]] <> " bytes"];
        log["C-->S " <> stringLineTake[FromCharacterCode[Normal[Take[bytesIn, UpTo[1000]]]], UpTo[20]]];
        log["...\n"]
      ];

      content = ImportByteArray[bytesIn, "RawJSON"];

      AppendTo[contentsIn, content]
      ,
      {bytesIn, bytessIn}
    ];

    bytessIn = {};

    If[!MatchQ[contentsIn, {_?AssociationQ ...}],
      log["\n\n"];
      log["Internal assert 1 failed: list of Associations: ", contentsIn];
      log["\n\n"];

      exitHard[]
    ];


    preScanForCancels[contentsIn];


    (*
    Now expand new contents
    *)

    If[$Debug2,
      log["before expandContent"]
    ];

    $PreExpandContentQueue = contentsIn;

    lastContentsIn = $PreExpandContentQueue;

    $PreExpandContentQueue = Flatten[MapIndexed[expandContent, $PreExpandContentQueue] /. expandContent[c_, _] :> {c}];

    If[$Debug2,
      log["$PreExpandContentQueue (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
      log["..."]
    ];

    While[$PreExpandContentQueue =!= lastContentsIn,

      If[$Debug2,
        log["expanded (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
        log["..."]
      ];

      lastContentsIn = $PreExpandContentQueue;

      $PreExpandContentQueue = Flatten[MapIndexed[expandContent, $PreExpandContentQueue] /. expandContent[c_, _] :> {c}];

      If[$Debug2,
        log["$PreExpandContentQueue (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
        log["..."]
      ]
    ];

    If[$Debug2,
      log["after expandContent"]
    ];

    contentsIn = $PreExpandContentQueue;

    $PreExpandContentQueue =.;


    If[!MatchQ[contentsIn, {_?AssociationQ ...}],
      log["\n\n"];
      log["Internal assert 2 failed: list of Associations: ", contentsIn];
      log["\n\n"];

      exitHard[]
    ];


    $ContentQueue = $ContentQueue ~Join~ contentsIn;

    If[$Debug2,
      log["appending to $ContentQueue"];
      log["$ContentQueue (up to 20): ", #["method"]& /@ Take[$ContentQueue, UpTo[20]]]
    ];

    Null
  ]]


preScanForCancels[contents:{_?AssociationQ ...}] :=
  Module[{cancels, params, id},

    cancels = Cases[contents, KeyValuePattern["method" -> "$/cancelRequest"]];

    Scan[
      Function[{content},
        params = content["params"];

        id = params["id"];

        $CancelMap[id] = True
      ], cancels];

    If[$Debug2,
      log["after preScanForCancels"];
      log["$CancelMap: ", $CancelMap]
    ]
  ]



(*
input: JSON RPC assoc

returns: a list of JSON RPC assocs
*)
LSPEvaluate[content_(*no Association here, allow everything*)] :=
Catch[
Module[{contents},

  (*
  (*  
  Figuring out what to with UTF-16 surrogates...

  Related bugs: 382744
  *)

  (*
  Coming in as JSON, so non-ASCII characters are using \uXXXX escaping
  So safe to treat bytes as ASCII
  *)
  str = FromCharacterCode[Normal[bytes], "ASCII"];

  escapes = StringCases[str, "\\u" ~~ ds : (_ ~~ _ ~~ _ ~~ _) :> ds];
  If[escapes != {},
    surrogates = Select[escapes, (
        (* high and low surrogates *)
        16^^d800 <= FromDigits[#, 16] <= 16^^dfff
      )&];
    If[surrogates != {},
      (*
      surrogates have been detected
      *)
      Null
    ]
  ];

  content = ImportString[str, "RawJSON"];
  *)

  Which[
    TrueQ[$ServerState == "shutdown"],
      contents = handleContentAfterShutdown[content]
    ,
    True,
      contents = handleContent[content]
  ];

  If[!MatchQ[contents, {_?AssociationQ ...}],
    log["\n\n"];
    log["invalid contents result (should match {_?AssociationQ ...}): ", contents];
    log["\n\n"];

    exitHard[]
  ];

  contents
]]



$didOpenMethods = {
  "textDocument/runDiagnostics",
  "textDocument/publishDiagnostics"
}


$didCloseMethods = {
  "textDocument/publishDiagnostics"
}


$didSaveMethods = {}


$didChangeMethods = {
  "textDocument/runDiagnostics",
  "textDocument/publishDiagnostics"
}


RegisterDidOpenMethods[meths_] := ($didOpenMethods = Join[$didOpenMethods, meths])

RegisterDidCloseMethods[meths_] := ($didCloseMethods = Join[$didCloseMethods, meths])

RegisterDidSaveMethods[meths_] := ($didSaveMethods = Join[$didSaveMethods, meths])

RegisterDidChangeMethods[meths_] := ($didChangeMethods = Join[$didChangeMethods, meths])



(*
content: JSON-RPC Association

returns: a list of associations (possibly empty), each association represents JSON-RPC
*)
handleContent[content:KeyValuePattern["method" -> "initialize"]] :=
Module[{id, params, capabilities, textDocument, codeAction, codeActionLiteralSupport, codeActionKind, valueSet,
  codeActionProviderValue, initializationOptions, implicitTokens,
  bracketMatcher, debugBracketMatcher, htmlSnippets, clientName},

  If[$Debug2,
    log["initialize: enter"];
    log["content: ", content]
  ];

  id = content["id"];
  params = content["params"];

  If[KeyExistsQ[params, "initializationOptions"],
    initializationOptions = params["initializationOptions"];

    (*

    "confidenceLevel" initialization option is deprecated

    Use ConfidenceLevel option for StartServer

    If[KeyExistsQ[initializationOptions, "confidenceLevel"],
      $ConfidenceLevelInitialization = initializationOptions["confidenceLevel"]
    ];
    *)
    
    If[KeyExistsQ[initializationOptions, "implicitTokens"],
      implicitTokens = initializationOptions["implicitTokens"];

      $ImplicitTokens = implicitTokens
    ];
    If[KeyExistsQ[initializationOptions, "bracketMatcher"],
      bracketMatcher = initializationOptions["bracketMatcher"];

      $BracketMatcher = bracketMatcher
    ];
    If[KeyExistsQ[initializationOptions, "debugBracketMatcher"],
      debugBracketMatcher = initializationOptions["debugBracketMatcher"];

      $DebugBracketMatcher = debugBracketMatcher
    ];
    If[KeyExistsQ[initializationOptions, "htmlSnippets"],
      htmlSnippets = initializationOptions["htmlSnippets"];

      $HTMLSnippets = htmlSnippets
    ]
  ];


  (*
  Only use confidenceLevel from initializationOptions if no ConfidenceLevel option was passed to StartServer[]
  *)
  Which[
    NumberQ[$ConfidenceLevelOption],
      $ConfidenceLevel = $ConfidenceLevelOption
    ,
    (* NumberQ[$ConfidenceLevelInitialization],
      $ConfidenceLevel = $ConfidenceLevelInitialization
    , *)
    True,
      $ConfidenceLevel = $DefaultConfidenceLevel
  ];

  If[$Debug2,
    log["$ConfidenceLevel: ", $ConfidenceLevel]
  ];


  $ColorProvider = True;

  If[KeyExistsQ[params, "clientName"],
    clientName = params["clientName"];

    (*
    There are multiple problems with Eclipse here:

    Eclipse, or possibly the LSP4E plugin, has strange behavior where 100s or 1000s of documentColor messages
    are sent to the server.

    So we need to disable colorProvider for Eclipse

    Also, Eclipse sends the NON-STANDARD clientName as identification

    VERY ANNOYING!!
    *)
    If[clientName == "Eclipse IDE",
      $ColorProvider = False
    ]
  ];

  If[$Debug2,
    log["$ColorProvider: ", $ColorProvider]
  ];

  
  capabilities = params["capabilities"];
  textDocument = capabilities["textDocument"];
  codeAction = textDocument["codeAction"];

  If[KeyExistsQ[codeAction, "codeActionLiteralSupport"],
    $CodeActionLiteralSupport = True;
    codeActionLiteralSupport = codeAction["codeActionLiteralSupport"];
    codeActionKind = codeActionLiteralSupport["codeActionKind"];
    valueSet = codeActionKind["valueSet"]
  ];

  If[$CodeActionLiteralSupport,
    codeActionProviderValue = <| "codeActionKinds" -> {"quickfix"} |>
    ,
    codeActionProviderValue = True
  ];

  If[$ImplicitTokens,

    RegisterDidOpenMethods[{
      "textDocument/runImplicitTokens",
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidCloseMethods[{
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidSaveMethods[{}];

    RegisterDidChangeMethods[{
      "textDocument/runImplicitTokens",
      "textDocument/publishImplicitTokens"
    }]
  ];

  If[$BracketMatcher,

    Needs["ML4Code`"];

    (*
    Some simple thing to warm-up
    *)
    ML4Code`SuggestBracketEdits["f["];

    RegisterDidOpenMethods[{
      "textDocument/runBracketMismatches",
      "textDocument/suggestBracketEdits",
      "textDocument/publishBracketMismatches"
    }];

    RegisterDidCloseMethods[{
      "textDocument/publishBracketMismatches"
    }];

    RegisterDidSaveMethods[{}];

    RegisterDidChangeMethods[{
      "textDocument/runBracketMismatches",
      "textDocument/suggestBracketEdits",
      "textDocument/publishBracketMismatches"
    }]
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id,
      "result" -> <| "capabilities"-> <| "referencesProvider" -> True,
                                         "textDocumentSync" -> <| "openClose" -> True,
                                                                  "save" -> <| "includeText" -> False |>,
                                                                  "change" -> $TextDocumentSyncKind["Full"]
                                                              |>,
                                         "codeActionProvider" -> codeActionProviderValue,
                                         "colorProvider" -> $ColorProvider,
                                         "hoverProvider" -> True,
                                         "definitionProvider" -> True,
                                         "documentFormattingProvider" -> True,
                                         "documentRangeFormattingProvider" -> True,
                                         "executeCommandProvider" -> $ExecuteCommandProvider,
                                         "documentSymbolProvider" -> True
                                     |>
                 |>
  |>}
]


handleContent[content:KeyValuePattern["method" -> "initialized"]] :=
  Module[{},

    If[$Debug2,
      log["initialized: enter"]
    ];

    {}
  ]

handleContent[content:KeyValuePattern["method" -> "shutdown"]] :=
Catch[
Module[{id},

  If[$Debug2,
    log["shutdown: enter"]
  ];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["$CancelMap: ", $CancelMap]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  $OpenFilesMap =.;

  $ServerState = "shutdown";

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}
]]

(*
Unexpected call to exit
*)
handleContent[content:KeyValuePattern["method" -> "exit"]] :=
  Module[{},

    If[$Debug2,
      log["exit: enter"]
    ];

    exitSemiGracefully[]
  ]


handleContent[content:KeyValuePattern["method" -> "$/cancelRequest"]] :=
Catch[
Module[{params, id},
  
  If[$Debug2,
    log["$/cancelRequest: enter"]
  ];

  params = content["params"];

  id = params["id"];

  If[!KeyExistsQ[$CancelMap, id],
    Throw[{}]
  ];

  log["cancel was not handled: ", id];

  $CancelMap[id] =.;

  If[$Debug2,
    log["$CancelMap: ", $CancelMap]
  ];

  {}
]]

(*
$ Notifications and Requests

Notification and requests whose methods start with "$/" are messages which are protocol
implementation dependent and might not be implementable in all clients or servers.
For example if the server implementation uses a single threaded synchronous programming
language then there is little a server can do to react to a "$/cancelRequest" notification.
If a server or client receives notifications starting with "$/" it is free to ignore the
notification.
If a server or client receives a requests starting with "$/" it must error the request with
error code MethodNotFound (e.g. -32601).
*)
handleContent[content:KeyValuePattern["method" -> meth_ /; StringMatchQ[meth, "$/" ~~ __]]] :=
Module[{id},

  If[$Debug2,
    log[meth <> ": enter"]
  ];

  If[KeyExistsQ[content, "id"],
    (*
    has id, so this is a request
    *)
    id = content["id"];
    {<| "jsonrpc" -> "2.0", "id" -> id,
      "error" -> <|
        "code" -> $ErrorCodes["MethodNotFound"],
        "message"->"Method Not Found" |> |>}
    ,
    (*
    does not have id, so this is a notification
    just ignore
    *)
    {}
  ]
]



handleContentAfterShutdown[content:KeyValuePattern["method" -> "exit"]] :=
  Module[{},

    If[$Debug2,
      log["exit after shutdown: enter"]
    ];

    exitGracefully[]
  ]

(*
Called if any requests or notifications come in after shutdown
*)
handleContentAfterShutdown[content_?AssociationQ] :=
  Module[{id},

    If[$Debug2,
      log["message after shutdown: enter: ", #["method"]&[content]]
    ];

    If[KeyExistsQ[content, "id"],
      (*
      has id, so this is a request
      *)
      id = content["id"];
      {<| "jsonrpc" -> "2.0", "id" -> id,
        "error" -> <|
          "code" -> $ErrorCodes["InvalidRequest"],
          "message"->"Invalid request" |> |>}
      ,
      (*
      does not have id, so this is a notification
      just ignore
      *)
      {}
    ]
  ]


expandContent[content:KeyValuePattern["method" -> "textDocument/didOpen"], pos_] :=
  Catch[
  Module[{params, doc, uri},

    If[$Debug2,
      log["textDocument/didOpen: enter expand"]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/didOpenFencepost", "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "params" -> params |>& /@ ({
        "textDocument/didOpenFencepost"
      } ~Join~ $didOpenMethods)
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/didOpenFencepost"]] :=
Catch[
Module[{params, doc, uri, text, entry},
  
  If[$Debug2,
    log["textDocument/didOpenFencepost: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  text = doc["text"];

  entry = <| "Text" -> text |>;

  $OpenFilesMap[uri] = entry;

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/concreteParse"]] :=
Catch[
Module[{params, doc, uri, cst, text, entry},

  If[$Debug2,
    log["textDocument/concreteParse: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = $OpenFilesMap[uri];

  cst = Lookup[entry, "CST", Null];

  If[cst =!= Null,
    Throw[{}]
  ];

  text = entry["Text"];

  If[$Debug2,
    log["text: ", stringLineTake[StringTake[ToString[text, InputForm], UpTo[1000]], UpTo[20]]];
    log["...\n"]
  ];
  
  If[$Debug2,
    log["before CodeConcreteParse"]
  ];

  cst = CodeConcreteParse[text];

  If[$Debug2,
    log["after CodeConcreteParse"]
  ];

  If[FailureQ[cst],

    (*
    It is possible that a file is open in an editor, the actual file system contents get deleted,
    but the editor still has a stale window open.
    Focusing on that window could trigger a textDocument/didOpen notification, but the file does not exist!
    TODO: is this a bug in Sublime / LSP package?
    *)
    If[MatchQ[cst, Failure["FindFileFailed", _]],
      Throw[{}]
    ];

    Throw[cst]
  ];

  cst[[1]] = File;

  entry["CST"] = cst;

  (*
  save time if the file has no tabs
  *)
  If[!StringContainsQ[text, "\t"],
    entry["CSTTabs"] = cst
  ];

  $OpenFilesMap[uri] = entry;

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/concreteTabsParse"]] :=
Catch[
Module[{params, doc, uri, text, entry, cstTabs},

  If[$Debug2,
    log["textDocument/concreteTabsParse: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = $OpenFilesMap[uri];

  cstTabs = Lookup[entry, "CSTTabs", Null];

  If[cstTabs =!= Null,
    Throw[{}]
  ];

  text = entry["Text"];

  (*
  Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)

  If[$Debug2,
    log["before CodeConcreteParse (TabWidth 4)"]
  ];

  cstTabs = CodeConcreteParse[text, "TabWidth" -> 4];

  If[$Debug2,
    log["after CodeConcreteParse (TabWidth 4)"]
  ];

  If[FailureQ[cstTabs],

    (*
    It is possible that a file is open in an editor, the actual file system contents get deleted,
    but the editor still has a stale window open.
    Focusing on that window could trigger a textDocument/didOpen notification, but the file does not exist!
    TODO: is this a bug in Sublime / LSP package?
    *)
    If[MatchQ[cstTabs, Failure["FindFileFailed", _]],
      Throw[{}]
    ];

    Throw[cstTabs]
  ];

  cstTabs[[1]] = File;

  entry["CSTTabs"] = cstTabs;

  $OpenFilesMap[uri] = entry;

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/aggregateParse"]] :=
Catch[
Module[{params, doc, uri, cst, text, entry, agg},

  If[$Debug2,
    log["textDocument/aggregateParse: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = $OpenFilesMap[uri];

  text = entry["Text"];

  agg = Lookup[entry, "Agg", Null];

  If[agg =!= Null,
    Throw[{}]
  ];

  cst = entry["CST"];
  
  If[$Debug2,
    log["before Aggregate"]
  ];

  agg = CodeParser`Abstract`Aggregate[cst];

  If[$Debug2,
    log["after Aggregate"]
  ];

  entry["Agg"] = agg;

  (*
  save time if the file has no tabs
  *)
  If[!StringContainsQ[text, "\t"],
    entry["AggTabs"] = agg
  ];

  $OpenFilesMap[uri] = entry;

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/aggregateTabsParse"]] :=
Catch[
Module[{params, doc, uri, entry, cstTabs, aggTabs},

  If[$Debug2,
    log["textDocument/aggregateTabsParse: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];

  entry = $OpenFilesMap[uri];

  aggTabs = Lookup[entry, "AggTabs", Null];

  If[aggTabs =!= Null,
    Throw[{}]
  ];

  cstTabs = entry["CSTTabs"];

  (*
  Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)

  If[$Debug2,
    log["before Aggregate"]
  ];

  aggTabs = CodeParser`Abstract`Aggregate[cstTabs];

  If[$Debug2,
    log["after Aggregate"]
  ];

  If[FailureQ[aggTabs],
    Throw[aggTabs]
  ];

  entry["AggTabs"] = aggTabs;

  $OpenFilesMap[uri] = entry;

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/abstractParse"]] :=
Catch[
Module[{params, doc, uri, entry, agg},

  If[$Debug2,
    log["textDocument/abstractParse: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];
  
  entry = $OpenFilesMap[uri];

  ast = Lookup[entry, "AST", Null];

  If[ast =!= Null,
    Throw[{}]
  ];

  agg = entry["Agg"];
  
  If[$Debug2,
    log["before Abstract"]
  ];

  ast = CodeParser`Abstract`Abstract[agg];

  If[$Debug2,
    log["after Abstract"]
  ];

  entry["AST"] = ast;

  $OpenFilesMap[uri] = entry;

  {}
]]



expandContent[content:KeyValuePattern["method" -> "textDocument/didClose"], pos_] :=
  Catch[
  Module[{params, doc, uri},

    If[$Debug2,
      log["textDocument/didClose: enter expand"]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/didCloseFencepost", "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "params" -> params |>& /@ ({
        "textDocument/didCloseFencepost"
      } ~Join~ $didCloseMethods)
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/didCloseFencepost"]] :=
Module[{params, doc, uri},

  If[$Debug2,
    log["textDocument/didCloseFencepost: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  $OpenFilesMap[uri] =.;

  {}
]



expandContent[content:KeyValuePattern["method" -> "textDocument/didSave"], pos_] :=
  Catch[
  Module[{params, doc, uri},

    If[$Debug2,
      log["textDocument/didSave: enter expand"]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/didSaveFencepost", "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "params" -> params |>& /@ ({
        "textDocument/didSaveFencepost"
      } ~Join~ $didSaveMethods)
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/didSaveFencepost"]] :=
  Module[{},

    If[$Debug2,
      log["textDocument/didSaveFencepost: enter"]
    ];

    {}
  ]



expandContent[content:KeyValuePattern["method" -> "textDocument/didChange"], pos_] :=
  Catch[
  Module[{params, doc, uri},

    If[$Debug2,
      log["textDocument/didChange: enter expand"]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/didChangeFencepost", "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "params" -> params |>& /@ ({
        "textDocument/didChangeFencepost"
      } ~Join~ $didChangeMethods)
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/didChangeFencepost"]] :=
Catch[
Module[{params, doc, uri, text, lastChange, entry},
  
  If[$Debug2,
    log["textDocument/didChangeFencepost: enter"]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  
  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{}]
  ];
  
  changes = params["contentChanges"];

  (*
  Currently only supporting full text, so always only apply the last change
  *)
  lastChange = changes[[-1]];

  text = lastChange["text"];

  entry = <| "Text" -> text |>;

  $OpenFilesMap[uri] = entry;

  {}
]]


exitGracefully[] := (
  log["\n\n"];
  log["KERNEL IS EXITING GRACEFULLY"];
  log["\n\n"];
  Pause[1];Exit[0]
)

exitSemiGracefully[] := (
  log["\n\n"];
  log["KERNEL IS EXITING SEMI-GRACEFULLY"];
  log["\n\n"];
  Pause[1];Exit[1]
)

exitHard[] := (
  log["\n\n"];
  log["KERNEL IS EXITING HARD"];
  log["\n\n"];
  Pause[1];Exit[1]
)


End[]

EndPackage[]
