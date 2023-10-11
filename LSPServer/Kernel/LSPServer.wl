(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the client.\
 StartServer[logDir] logs traffic to logDir."

RunServerDiagnostic

initializeLSPComm

expandContent

expandContentsAndAppendToContentQueue

LSPEvaluate
readEvalWriteLoop

ProcessScheduledJobs


exitHard
exitGracefully
exitSemiGracefully
shutdownLSPComm


handleContent

handleContentAfterShutdown



$Debug

$Debug3
(*  
level 0: Server start and exit log
level 1: Content handler entry and exit log to understand the flow of the handlers
level 2: Log inside a content handler for content handler debugging
level 3: Further detailed log
*)
$LogLevel = 0

$DebugBracketMatcher


$PreExpandContentQueue

$ContentQueue

$OpenFilesMap

$CancelMap

$hrefIdCounter

$ServerState


$AllowedImplicitTokens


$BracketMatcher

(*
$BracketMatcherDisplayInsertionText
*)

$BracketMatcherUseDesignColors


$ConfidenceLevel

$HierarchicalDocumentSymbolSupport

(*
$SemanticTokens is True if the client supports semantic tokens and the user has enabled them

If $SemanticTokens is False, then diagnostics are used as a fallback to indicate scoping issues such as unused variables and shadowed variables

*)
$SemanticTokens


$ML4CodeTimeLimit

$commProcess



$BracketMatcherDelayAfterLastChange
$DiagnosticsDelayAfterLastChange
$ImplicitTokensDelayAfterLastChange


$startupMessagesText


Begin["`Private`"]


(*
setup Startup Messages handling

There may be internal errors in LSPServer that emit messages during Needs["LSPServer`"]

These messages are exceptionally hard to handle because any code for handling has not yet been loaded

The messages may cause unexplained hangs in clients

So manually set $Messages to a tmp file and then handle the messages later
*)
$startupMessagesFile = OpenWrite[]

If[!FailureQ[$startupMessagesFile],
  $oldMessages = $Messages;
  $Messages = {$startupMessagesFile}
  ,
  $startupMessagesText = "OpenWrite[] failed while setting up Startup Messages handling"
]



Needs["LSPServer`BracketMismatches`"]
Needs["LSPServer`CodeAction`"]
Needs["LSPServer`Color`"]
Needs["LSPServer`Completion`"]
Needs["LSPServer`Definitions`"]
Needs["LSPServer`Diagnostics`"]
Needs["LSPServer`DocumentSymbol`"]
Needs["LSPServer`FoldingRange`"]
Needs["LSPServer`Formatting`"]
Needs["LSPServer`Hover`"]
Needs["LSPServer`ImplicitTokens`"]
Needs["LSPServer`Library`"]
Needs["LSPServer`ListenSocket`"]
Needs["LSPServer`References`"]
Needs["LSPServer`SelectionRange`"]
Needs["LSPServer`SemanticTokens`"]
Needs["LSPServer`ServerDiagnostics`"]
Needs["LSPServer`Socket`"]
Needs["LSPServer`StdIO`"]
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
TODO: when targeting 12.1 as a minimum, then use paclet["AssetLocation", "BuiltInFunctions"]
*)
location = "Location" /. PacletInformation["LSPServer"]

WolframLanguageSyntax`Generate`$builtinFunctions = Get[FileNameJoin[{location, "Resources", "Data", "BuiltinFunctions.wl"}]]
WolframLanguageSyntax`Generate`$options = Get[FileNameJoin[{location, "Resources", "Data", "Options.wl"}]]
WolframLanguageSyntax`Generate`$constants = Get[FileNameJoin[{location, "Resources", "Data", "Constants.wl"}]]
WolframLanguageSyntax`Generate`$experimentalSymbols = Get[FileNameJoin[{location, "Resources", "Data", "ExperimentalSymbols.wl"}]]
WolframLanguageSyntax`Generate`$obsoleteSymbols = Get[FileNameJoin[{location, "Resources", "Data", "ObsoleteSymbols.wl"}]]
WolframLanguageSyntax`Generate`$systemCharacters = Get[FileNameJoin[{location, "Resources", "Data", "SystemCharacters.wl"}]]
WolframLanguageSyntax`Generate`$systemLongNames = Get[FileNameJoin[{location, "Resources", "Data", "SystemLongNames.wl"}]]
WolframLanguageSyntax`Generate`$undocumentedSymbols = Get[FileNameJoin[{location, "Resources", "Data", "UndocumentedSymbols.wl"}]]


(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]


$DefaultConfidenceLevel = 0.50

$CodeActionLiteralSupport = False

$AllowedImplicitTokens = {}

(*
if $BracketMatcher, then load ML4Code` and use ML bracket matching tech
*)
$BracketMatcher = False

$BracketMatcherUseDesignColors = True


$SemanticTokens = False

$HierarchicalDocumentSymbolSupport = False


(*
$BracketMatcherDisplayInsertionText = False
*)

(*
Bracket suggestions from ML4Code can take O(n^2) time in the size of the chunk, so make sure to
have a time limit

Related issues: CODETOOLS-71
*)
$ML4CodeTimeLimit = 0.4


$ExecuteCommandProvider = <|
  "commands" -> {
    (*
    roundtrip_responsiveness_test is an undocumented, debug command
    *)
    "roundtrip_responsiveness_test",
    (*
    ping_pong_responsiveness_test is an undocumented, debug command
    *)
    "ping_pong_responsiveness_test",
    (*
    payload_responsiveness_test is an undocumented, debug command
    *)
    "payload_responsiveness_test"
  }
|>




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
  (* "jsonrpcReservedErrorRangeStart" -> -32099, *)
  "ServerNotInitialized" -> -32002,
  "UnknownErrorCode" -> -32001,
  (* "jsonrpcReservedErrorRangeEnd" -> -32000, *)
  (* "lspReservedErrorRangeStart" -> -32899, *)
  "ContentModified" -> -32801,
  "RequestCancelled" -> -32800
  (* "lspReservedErrorRangeEnd" -> -32800, *)
|>


$TextDocumentSyncKind = <|
  "None" -> 0,
  "Full" -> 1,
  "Incremental" -> 2
|>

$MessageType = <|
  "Error" -> 1,
  "Warning" -> 2,
  "Info" -> 3,
  "Log" -> 4
|>



$ContentQueue = {}

(*
An assoc of uri -> entry

entry is an assoc of various key/values such as "Text" -> text and "CST" -> cst

*)
$OpenFilesMap = <||>

(*
An assoc of id -> True|False
*)
$CancelMap = <||>



(*
Expands contents and appends to $ContentQueue

Returns Null
*)
expandContentsAndAppendToContentQueue[contentsIn_] :=
Module[{contents},

  contents = contentsIn;

  log[1, "**************************************** Message Cycle ****************************************** \n"];
  log[1, "$ContentQueue Methods(before expansion):> ", InputForm[#["method"]& /@ $ContentQueue]];
  log[1, "New message (before expansion):> ", InputForm[#["method"]& /@ contents]];

  If[!MatchQ[contents, {_?AssociationQ ...}],
    log[0, "\n\n"];
    log[0, "Internal assert 1 failed: list of Associations: ", contents];
    log[0, "\n\n"];

    exitHard[]
  ];

  preScanForCancels[contents];

  (*
  Now expand new contents
  *)

  contents = expandContents[contents];

  $ContentQueue = $ContentQueue ~Join~ contents;

  log[1, "$ContentQueue methods (after expansion & joining new content) :> ", InputForm[#["method"]& /@ $ContentQueue]];
  log[3, "$ContentQueue (after expansion & joining new content):> ", InputForm[$ContentQueue], "\n"];


  c = Cases[$ContentQueue, KeyValuePattern["method" -> "textDocument/completion"]];

  If[Length[c] > 0, 
    $ContentQueue = rearrangeQueue[$ContentQueue];
  ];
  
];


rearrangeQueue[queue_] := 
Module[{didChangeMessage, completionMessage, params, lastComPos, positionDCFP, selectedDCFPmsgs, deletePositions, q, id},

	(* Get all the completion and didChangeFencepost messages *)
  didChangeMessage = Cases[queue, KeyValuePattern["method" -> "textDocument/didChangeFencepost"]];
  completionMessage = Cases[queue, KeyValuePattern["method" -> "textDocument/completion"]];
  positionCompletion = First @ First @ Position[queue, #]& /@ completionMessage;
  
  (* Get all the didChangeFencepost messages before the last completion message in the queue *)
  lastComPos = First @ First @ Position[queue, Last[completionMessage]];
  positionDCFP = Select[(First @ First @ Position[queue,#]& /@ didChangeMessage), #< lastComPos&];
  selectedDCFPmsgs = queue[[#]]& /@ positionDCFP;
  
  (* using  DeleteElements, requires Mathematica 13.1+ *)
  (* 
    q = DeleteElements[queue, completionMessage];
    q = DeleteElements[q, selectedDCFPmsgs];
  *)

  (* using Delete for 12.0+ compatibility *)
  deletePositions = {#} & /@ Sort[Join[positionCompletion, positionDCFP]];
  q = Delete[queue, deletePositions];

  id = Last[completionMessage]["id"];
  params = Last[completionMessage]["params"];

  (* 
    We are not going to evaluate AST and CST inside completion handler to save time.
    So the changes introduced by the dcfp messages will be reflected in the AST and the CST
    by evaluating AST and CST just after the completion handle. 
  *)

  PrependTo[q,  (<| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse"
  })];

  (* 
      Just handle the last completion message in the queue and ignore the earlier
      ones as only that last one can give the token with most updated text.
  *)

  PrependTo[q, Last[completionMessage]];

  (* 
      Rearranged message queue after completion message prioritization contains 
        1. All the didChangeFencepost messages
        2. Most recent completion message
        3. concreteParse, aggregateParse and abstractParse messages
        4. Rest of the queue
  *)
  PrependTo[q, selectedDCFPmsgs] // Flatten

]


(*

Use 0.4 seconds, same as default value of spelling squiggly in FE

In[7]:= CurrentValue[$FrontEnd, {SpellingOptions, "AutoSpellCheckDelay"}]

Out[7]= 0.4
*)
$DiagnosticsDelayAfterLastChange = 0.4

$ImplicitTokensDelayAfterLastChange = 3.0

$BracketMatcherDelayAfterLastChange = 4.0



StartServer::notebooks = "LSPServer cannot be started inside of a notebook session."

Options[StartServer] = {
  ConfidenceLevel -> Automatic,
  CommunicationMethod -> "StdIO"
}

(*
setup the REPL to handle traffic from client
*)
StartServer[logDir_String:"", OptionsPattern[]] :=
Catch[
Catch[
Module[{logFile, logFileStream,
  logFileName, logFileCounter, oldLogFiles, now, quantity30days, dateStr, readEvalWriteCycle},

  $kernelStartTime = Now;

  If[$Notebooks,
    (*
    OK to return here without killing the kernel
    This is in a notebook session
    *)
    Message[StartServer::notebooks];
    Throw[$Failed]
  ];

  (*
  This is NOT a notebook session, so it is ok to kill the kernel
  *)

  $ConfidenceLevelOption = OptionValue[ConfidenceLevel];
  $commProcess = OptionValue[CommunicationMethod];


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

  $Debug = (logDir != "");

  If[$Debug,

    (
    (* :!CodeAnalysis::BeginBlock:: *)
    (* :!CodeAnalysis::Disable::BackwardsCompatibility:: *)
    Quiet[CreateDirectory[logDir], {CreateDirectory::eexist, CreateDirectory::filex}];
    (* :!CodeAnalysis::EndBlock:: *)
    );

    (*
    Cleanup existing log files
    *)
    oldLogFiles = FileNames["kernelLog*", logDir];
    now = Now;
    (*
    Was using ":" as a time separator
    But obviously cannot use ":" character in file names on Windows!!
    *)
    dateStr = DateString[now, {"Year", "-", "Month", "-", "Day", "_", "Hour24", "-", "Minute", "-", "Second"}];
    quantity30days = Quantity[30, "Days"];
    Do[
      (*
      Delete oldLogFile if not modified for 30 days
      *)
      If[(now - Information[File[oldLogFile]]["LastModificationDate"]) > quantity30days,
        DeleteFile[oldLogFile]
      ]
      ,
      {oldLogFile, oldLogFiles}
    ];

    logFileName = "kernelLog-" <> dateStr;
    logFile = FileNameJoin[{logDir, logFileName <> ".txt"}];

    logFileCounter = 1;
    While[True,
      If[FileExistsQ[logFile],
        logFile = FileNameJoin[{logDir, logFileName <> "-" <> ToString[logFileCounter] <> ".txt"}];
        logFileCounter++;
        ,
        Break[]
      ]
    ];

    logFileStream = OpenWrite[logFile, CharacterEncoding -> "UTF-8"];

    If[FailureQ[logFileStream],
      
      log[0, "\n\n"];
      log[0, "opening log file failed: ", logFileStream];
      log[0, "\n\n"];
      
      exitHard[]
    ];

    $Messages = $Messages ~Join~ { logFileStream };

    $Output = $Output ~Join~ { logFileStream }
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


  log[0, "$CommandLine: ", $CommandLine];
  log[0, "\n\n"];

  log[0, "$commProcess: ", $commProcess];
  log[0, "\n\n"];

  log[0, "$ProcessID: ", $ProcessID];
  log[0, "\n\n"];

  log[0, "$ParentProcessID: ", $ParentProcessID];
  log[0, "\n\n"];

  log[0, "Directory[]: ", Directory[]];
  log[0, "\n\n"];


  log[0, "Starting server... (If this is the last line you see, then StartServer[] may have been called in an unexpected way and the server is hanging.)"];
  log[0, "\n\n"];


  If[$startupMessagesText =!= "",
    log[0, "\n\n"];
    log[0, "There were messages when loading LSPServer` package: ", $startupMessagesText];
    log[0, "\n\n"];
    
    exitHard[]
  ];


  (*
  This is the first use of LSPServer library, so this is where the library is initialized.
  Handle any initialization failures or other errors.
  *)

  $initializedComm = initializeLSPComm[$commProcess];

  If[FailureQ[$initializedComm],
    log[0,"\n\n"];
    (*
    //InputForm to work-around bug 411375
    *)
    log[0, "Initialization failed: ", $initializedComm //InputForm];
    log[0, "\n\n"];
    
    exitHard[]
  ];

  readEvalWriteCycle = readEvalWriteLoop[$commProcess, $initializedComm];

  If[FailureQ[readEvalWriteCycle],
    log[0, "\n\n"];
    log[0, "Read-Eval-Write-Loop failed: ", readEvalWriteCycle];
    log[0, "\n\n"];
    
    exitHard[]
  ];

]],(*Module, 1-arg Catch*)
_,
(
  log[0, "\n\n"];
  log[0, "uncaught Throw: ", #1];
  log[0, "\n\n"];
  
  exitHard[]

  )&
]


preScanForCancels[contents:{_?AssociationQ ...}] :=
Module[{cancels, params, id},

  cancels = Cases[contents, KeyValuePattern["method" -> "$/cancelRequest"]];

  Scan[
    Function[{content},
      params = content["params"];

      id = params["id"];

      $CancelMap[id] = True
    ], cancels];

  
    log[2, "after preScanForCancels"];
    log[2, "$CancelMap: ", $CancelMap];
  
]


(*
Input: list of Associations
Returns: list of Associations
*)
expandContents[contentsIn_] :=
Module[{contents, lastContents},

  contents = contentsIn;
  (* 
  This log can be used to know time to handle a feature.
  Time taken for a feature (x feature timing) = (feature exit log timing - new message entry timing)
  As we are changing the message queue to prioritise cumpletion message,
  it is important to know the completion feature timing.
  *)
  If[$Debug2,
    log["New message (before expansion):> ", InputForm[#["method"]& /@ contents], "\n"]
  ];

  log[2, "before expandContent"];

  Block[{$PreExpandContentQueue},

    $PreExpandContentQueue = contents;

    lastContents = $PreExpandContentQueue;

    $PreExpandContentQueue = Flatten[MapIndexed[expandContent, $PreExpandContentQueue] /. expandContent[c_, _] :> {c}];

    log[2, "$PreExpandContentQueue (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
    log[2, "..."];

    While[$PreExpandContentQueue =!= lastContents,

      log[2, "expanded (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
      log[2, "..."];

      lastContents = $PreExpandContentQueue;

      $PreExpandContentQueue = Flatten[MapIndexed[expandContent, $PreExpandContentQueue] /. expandContent[c_, _] :> {c}];

      log[2, "$PreExpandContentQueue (up to 20): ", #["method"]& /@ Take[$PreExpandContentQueue, UpTo[20]]];
      log[2, "..."];
    ];

    log[2, "after expandContent"];
    
    contents = $PreExpandContentQueue;
  ];

  If[!MatchQ[contents, {_?AssociationQ ...}],
    log[0, "\n\n"];
    log[0, "Internal assert 2 failed: list of Associations: ", contents];
    log[0, "\n\n"];

    exitHard[]
  ];

  contents
]


ProcessScheduledJobs[] :=
Catch[
Module[{openFilesMapCopy, entryCopy, jobs, res, methods, contents, toRemove, job, toRemoveIndices, contentsToAdd},

  (*
  Do not process any scheduled jobs after shutdown
  *)
  If[$ServerState == "shutdown",
    Throw[Null]
  ];

  openFilesMapCopy = $OpenFilesMap;

  contents = {};
  KeyValueMap[
    Function[{uri, entry},
      jobs = Lookup[entry, "ScheduledJobs", {}];
      toRemoveIndices = {};
      Do[
        job = jobs[[j]];
        res = job[entry];
        {methods, toRemove} = res;

        contentsToAdd = <| "method" -> #, "params" -> <| "textDocument" -> <| "uri" -> uri |> |> |>& /@ methods;

        contents = contents ~Join~ contentsToAdd;

        If[toRemove,
          AppendTo[toRemoveIndices, {j}]
        ]
        ,
        {j, 1, Length[jobs]}
      ];
      If[!empty[toRemoveIndices],
        jobs = Delete[jobs, toRemoveIndices];
        entryCopy = entry;
        entryCopy["ScheduledJobs"] = jobs;
        $OpenFilesMap[uri] = entryCopy
      ]
    ]
    ,
    openFilesMapCopy
  ];

  If[!empty[contents],
  
    contents = expandContents[contents];

    $ContentQueue = $ContentQueue ~Join~ contents;
  ]
]]


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

  Related bugs: 382744, 397941

  Related issues: https://github.com/microsoft/language-server-protocol/issues/376
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

  If[MatchQ[contents, Failure["URINotFound", _]],

    (*
    This can happen under some circumstances

    A file is closed, and something like publishDiagnostics is after the close in the queue

    Do not kill the kernel for this
    *)

    log[1, "\n\n"];
    log[1, "Internal assert 3 failed: list of Associations: ", contents];
    log[1, "\n\n"];

    Throw[contents]
  ];

  If[!MatchQ[contents, {_?AssociationQ ...}],
    log[0, "\n\n"];
    log[0, "Internal assert 4 failed: list of Associations: ", contents];
    log[0, "\n\n"];

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


$didChangeMethods = {}

$didChangeScheduledJobs = {
  Function[{entry}, If[Now - entry["LastChange"] > Quantity[$DiagnosticsDelayAfterLastChange, "Seconds"],
    {{
      "textDocument/runDiagnostics",
      "textDocument/publishDiagnostics"
    }, True},
    {{}, False}]
  ]
}


RegisterDidOpenMethods[meths_] := ($didOpenMethods = Join[$didOpenMethods, meths])

RegisterDidCloseMethods[meths_] := ($didCloseMethods = Join[$didCloseMethods, meths])

RegisterDidSaveMethods[meths_] := ($didSaveMethods = Join[$didSaveMethods, meths])

RegisterDidChangeMethods[meths_] := ($didChangeMethods = Join[$didChangeMethods, meths])

RegisterDidOpenScheduledJobs[jobs_] := ($didOpenScheduledJobs = Join[$didOpenScheduledJobs, jobs])

RegisterDidCloseScheduledJobs[jobs_] := ($didCloseScheduledJobs = Join[$didCloseScheduledJobs, jobs])

RegisterDidSaveScheduledJobs[jobs_] := ($didSaveScheduledJobs = Join[$didSaveScheduledJobs, jobs])

RegisterDidChangeScheduledJobs[jobs_] := ($didChangeScheduledJobs = Join[$didChangeScheduledJobs, jobs])




(*
content: JSON-RPC Association

returns: a list of associations (possibly empty), each association represents JSON-RPC
*)
handleContent[content:KeyValuePattern["method" -> "initialize"]] :=
Module[{id, params, capabilities, textDocument, codeAction, codeActionLiteralSupport, codeActionKind, valueSet,
  codeActionProviderValue, initializationOptions, implicitTokens,
  bracketMatcher, debugBracketMatcher, clientName, semanticTokensProviderValue, semanticTokens, contents,
  documentSymbol, hierarchicalDocumentSymbolSupport},

  log[1, "initialize: Enter"];

  id = content["id"];
  params = content["params"];

  If[KeyExistsQ[params, "initializationOptions"],

    initializationOptions = params["initializationOptions"];

    log[2, "initializationOptions: ", initializationOptions];

    (*
    initializationOptions may be Null, such as from Jupyter Lab LSP
    *)
    If[AssociationQ[initializationOptions],

      (*

      "confidenceLevel" initialization option is deprecated

      Use ConfidenceLevel option for StartServer

      If[KeyExistsQ[initializationOptions, "confidenceLevel"],
        $ConfidenceLevelInitialization = initializationOptions["confidenceLevel"]
      ];
      *)
      
      If[KeyExistsQ[initializationOptions, "implicitTokens"],
        implicitTokens = initializationOptions["implicitTokens"];

        $AllowedImplicitTokens = implicitTokens
      ];
      If[KeyExistsQ[initializationOptions, "bracketMatcher"],
        bracketMatcher = initializationOptions["bracketMatcher"];

        $BracketMatcher = bracketMatcher
      ];
      If[KeyExistsQ[initializationOptions, "debugBracketMatcher"],
        debugBracketMatcher = initializationOptions["debugBracketMatcher"];

        $DebugBracketMatcher = debugBracketMatcher
      ];
      If[KeyExistsQ[initializationOptions, "semanticTokens"],
        semanticTokens = initializationOptions["semanticTokens"];

        $SemanticTokens = semanticTokens
      ];
    ];

  log[1, "initialize: Exit"];
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


  log[2, "$AllowedImplicitTokens: ", $AllowedImplicitTokens];
  log[2, "$BracketMatcher: ", $BracketMatcher];
  log[2, "$DebugBracketMatcher: ", $DebugBracketMatcher];
  log[2, "$ConfidenceLevel: ", $ConfidenceLevel];
  log[2, "$SemanticTokens: ", $SemanticTokens];


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

  log[2, "$ColorProvider: ", $ColorProvider];

  
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

  If[$AllowedImplicitTokens != {},

    RegisterDidOpenMethods[{
      "textDocument/runImplicitTokens",
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidCloseMethods[{
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidSaveMethods[{}];

    RegisterDidChangeMethods[{
      "textDocument/clearImplicitTokens",
      "textDocument/publishImplicitTokens"
    }];

    RegisterDidChangeScheduledJobs[{
      Function[{entry}, If[Now - entry["LastChange"] > Quantity[$ImplicitTokensDelayAfterLastChange, "Seconds"],
        {{
          "textDocument/runImplicitTokens",
          "textDocument/publishImplicitTokens"
        }, True},
        {{}, False}]
      ]
    }]
  ];

  If[$BracketMatcher,

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
      "textDocument/clearBracketMismatches",
      "textDocument/publishBracketMismatches"
    }];

    RegisterDidChangeScheduledJobs[{
      Function[{entry}, If[Now - entry["LastChange"] > Quantity[$BracketMatcherDelayAfterLastChange, "Seconds"],
        {{
          "textDocument/runBracketMismatches",
          "textDocument/suggestBracketEdits",
          "textDocument/publishBracketMismatches"
        }, True},
        {{}, False}]
      ]
    }];

    $ExecuteCommandProvider =
      Merge[{$ExecuteCommandProvider, <|
        "commands" -> {
          (*
          enable_bracket_matcher_debug_mode is an undocumented, debug command
          *)
          "enable_bracket_matcher_debug_mode",
          (*
          disable_bracket_matcher_debug_mode is an undocumented, debug command
          *)
          "disable_bracket_matcher_debug_mode",
          (*
          enable_bracket_matcher_design_colors is an undocumented, debug command
          *)
          "enable_bracket_matcher_design_colors",
          (*
          disable_bracket_matcher_design_colors is an undocumented, debug command
          *)
          "disable_bracket_matcher_design_colors",
          (*
          enable_bracket_matcher_display_insertion_text is an undocumented, debug command
          *)
          "enable_bracket_matcher_display_insertion_text",
          (*
          disable_bracket_matcher_display_insertion_text is an undocumented, debug command
          *)
          "disable_bracket_matcher_display_insertion_text"
        }
      |>}, Flatten]
  ];

  If[$SemanticTokens,
    If[KeyExistsQ[textDocument, "semanticTokens"],
      semanticTokensProviderValue = <|
        "legend" -> <|
          "tokenTypes" -> Keys[$SemanticTokenTypes],
          "tokenModifiers" -> Keys[$SemanticTokenModifiers]
        |>,
        "range" -> False,
        "full" -> <| "delta" -> False |>
      |>
      ,
      (*
      if client does not advertise semantic token support, then do not respond with any support
      *)
      semanticTokensProviderValue = Null
    ];
    ,
    semanticTokensProviderValue = Null
  ];

  If[KeyExistsQ[textDocument, "documentSymbol"],
    documentSymbol = textDocument["documentSymbol"];
    hierarchicalDocumentSymbolSupport = documentSymbol["hierarchicalDocumentSymbolSupport"];
    $HierarchicalDocumentSymbolSupport = hierarchicalDocumentSymbolSupport
  ];

  $kernelInitializeTime = Now;

  log[2, "time to intialize: ", $kernelInitializeTime - $kernelStartTime];

  contents = {<| "jsonrpc" -> "2.0", "id" -> id,
    "result" -> <|
      "capabilities"-> <|
        "referencesProvider" -> True,
        "textDocumentSync" -> <|
          "openClose" -> True,
          "save" -> <| "includeText" -> False |>,
          "change" -> $TextDocumentSyncKind["Full"]
        |>,
        "completionProvider" -> <|
          "resolveProvider" -> False, 
          "triggerCharacters" -> {}
        |>,
        "codeActionProvider" -> codeActionProviderValue,
        "colorProvider" -> $ColorProvider,
        "hoverProvider" -> True,
        "definitionProvider" -> True,
        "documentFormattingProvider" -> True,
        "documentRangeFormattingProvider" -> True,
        "executeCommandProvider" -> $ExecuteCommandProvider,
        "documentSymbolProvider" -> True,
        "selectionRangeProvider" -> True,
        "semanticTokensProvider" -> semanticTokensProviderValue,
        "foldingRangeProvider" -> True
      |>
    |>
  |>};

  contents
]


handleContent[content:KeyValuePattern["method" -> "initialized"]] :=
Module[{warningMessages},

  
  log[1, "initialized: Enter"];


  (*
  Some simple thing to warm-up
  *)
  CodeParse["1+1"];

  If[$BracketMatcher,

    Block[{$ContextPath}, Needs["ML4Code`"]];
  
    (*
    Some simple thing to warm-up
    *)
    ML4Code`SuggestBracketEdits["f["];
  ];

  warningMessages = ServerDiagnosticWarningMessages[];

  log[2, "warningMessages: ", warningMessages];

  res = <|
    "jsonrpc" -> "2.0",
    "method" -> "window/showMessage",
    "params" ->
      <|
        "type" -> $MessageType["Warning"],
        "message" -> #
      |>
  |>& /@ warningMessages;

  log[1, "initialized: Exit"];

  res
]


handleContent[content:KeyValuePattern["method" -> "shutdown"]] :=
Catch[
Module[{id},

  
  log[1, "shutdown: Enter"];
  

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "$CancelMap: ", $CancelMap];
  
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  $OpenFilesMap =.;

  $ServerState = "shutdown";

  log[1, "shutdown: Exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}
]]

(*
Unexpected call to exit
*)
handleContent[content:KeyValuePattern["method" -> "exit"]] :=
Module[{},

  
  log[1, "exit: Enter"];
  log[1, "exit: Exit"];
  

  exitSemiGracefully[]
]


handleContent[content:KeyValuePattern["method" -> "$/cancelRequest"]] :=
Catch[
Module[{params, id},
  
  
  log[1, "$/cancelRequest: enter"];
  
  params = content["params"];

  id = params["id"];

  If[!KeyExistsQ[$CancelMap, id],
    Throw[{}]
  ];

  log[2, "cancel was not handled: ", id];

  $CancelMap[id] =.;

  log[2, "$CancelMap: ", $CancelMap];

  log[1, "$/cancelRequest: exit"];

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

  
  log[1, meth <> ": enter"];
  

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
    something like: $/setTraceNotification
    $/cancelRequest is handled elsewhere
    just ignore
    *)
    {}
  ]
]



handleContentAfterShutdown[content:KeyValuePattern["method" -> "exit"]] :=
Module[{},

  log[1, "exit after shutdown: enter"];

  log[1, "exit after shutdown: exit"];

  exitGracefully[]
]

(*
Called if any requests or notifications come in after shutdown
*)
handleContentAfterShutdown[content_?AssociationQ] :=
Module[{id},

  log[1, "message after shutdown: enter: ", #["method"]&[content]];

  log[1, "message after shutdown: exit: "];

  If[KeyExistsQ[content, "id"],
    (*
    has id, so this is a request
    *)
    id = content["id"];
    {<| "jsonrpc" -> "2.0", "id" -> id,
      "error" -> <|
        "code" -> $ErrorCodes["InvalidRequest"],
        "message" -> "Invalid request" |> |>}
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
Module[{params, doc, uri, res},
  
  log[1, "textDocument/didOpen: enter expand"];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/didOpenFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didOpenFencepost"
    } ~Join~ $didOpenMethods);

  log[1, "textDocument/didOpen: Exit"];

  res
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/didOpenFencepost"]] :=
Catch[
Module[{params, doc, uri, text, entry},
  
  
  log[1, "textDocument/didOpenFencepost: Enter"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  text = doc["text"];

  entry = <|
    "Text" -> text,
    "LastChange" -> Now
  |>;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/didOpenFencepost: Exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/concreteParse"]] :=
Catch[
Module[{params, doc, uri, cst, text, entry, fileName, fileFormat},
  
  log[1, "textDocument/concreteParse: Enter"];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  cst = Lookup[entry, "CST", Null];

  If[cst =!= Null,
    Throw[{}]
  ];

  text = entry["Text"];

  log[2, "text: ", stringLineTake[StringTake[ToString[text, InputForm], UpTo[1000]], UpTo[20]]];
  log[2, "...\n"];
  
  log[2, "before CodeConcreteParse"];

  fileName = normalizeURI[uri];

  fileFormat = "Package";
  If[FileExtension[fileName] == "wls",
    fileFormat = "Script"
  ];

  cst = CodeConcreteParse[text, "FileFormat" -> fileFormat];

  log[2, "after CodeConcreteParse"];

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

  log[1, "textDocument/concreteParse: Exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/concreteTabsParse"]] :=
Catch[
Module[{params, doc, uri, text, entry, cstTabs, fileName, fileFormat},

  
  log[1, "textDocument/concreteTabsParse: enter"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  cstTabs = Lookup[entry, "CSTTabs", Null];

  If[cstTabs =!= Null,
    Throw[{}]
  ];

  text = entry["Text"];

  (*
  Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)

  log[2, "before CodeConcreteParse (TabWidth 4)"];

  fileName = normalizeURI[uri];

  fileFormat = "Package";
  If[FileExtension[fileName] == "wls",
    fileFormat = "Script"
  ];

  cstTabs = CodeConcreteParse[text, "TabWidth" -> 4, "FileFormat" -> fileFormat];

  log[2, "after CodeConcreteParse (TabWidth 4)"];

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

  log[1, "textDocument/concreteTabsParse: exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/aggregateParse"]] :=
Catch[
Module[{params, doc, uri, cst, text, entry, agg},

  
  log[1, "textDocument/aggregateParse: Enter"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  text = entry["Text"];

  agg = Lookup[entry, "Agg", Null];

  If[agg =!= Null,
    Throw[{}]
  ];

  cst = entry["CST"];
  
  log[2, "before Aggregate"];

  agg = CodeParser`Abstract`Aggregate[cst];

  log[2, "after Aggregate"];

  entry["Agg"] = agg;

  (*
  save time if the file has no tabs
  *)
  If[!StringContainsQ[text, "\t"],
    entry["AggTabs"] = agg
  ];

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/aggregateParse: Exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/aggregateTabsParse"]] :=
Catch[
Module[{params, doc, uri, entry, cstTabs, aggTabs},

  
  log[1, "textDocument/aggregateTabsParse: enter"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  aggTabs = Lookup[entry, "AggTabs", Null];

  If[aggTabs =!= Null,
    Throw[{}]
  ];

  cstTabs = entry["CSTTabs"];

  (*
  Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)

  log[2, "before Aggregate"];

  aggTabs = CodeParser`Abstract`Aggregate[cstTabs];

  log[2, "after Aggregate"];

  If[FailureQ[aggTabs],
    Throw[aggTabs]
  ];

  entry["AggTabs"] = aggTabs;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/aggregateTabsParse: exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/abstractParse"]] :=
Catch[
Module[{params, doc, uri, entry, agg, ast, userSymbols},

  
  log[1, "textDocument/abstractParse: enter"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{}]
  ];
  
  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  ast = Lookup[entry, "AST", Null];

  If[ast =!= Null,
    Throw[{}]
  ];

  agg = entry["Agg"];
  
  log[2, "before Abstract"];

  ast = CodeParser`Abstract`Abstract[agg];

  userSymbols = findAllUserSymbols[ast];

  log[2, "after Abstract"];

  entry["AST"] = ast;
  entry["PreviousAST"] = ast;

  entry["UserSymbols"] = userSymbols;
  entry["PreviousUserSymbols"] = userSymbols;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/abstractParse: exit"];

  {}
]]


findAllUserSymbols[ast_] := DeleteDuplicates[
  Cases[ast, 
    {
      CallNode[
        LeafNode[Symbol, "SetDelayed" | "Set", <||>],
        {CallNode[LeafNode[Symbol, sym_, _], _, _], rhs : _} |
        {LeafNode[Symbol, sym_, _], rhs : _}, 
      _], 
    _} :> sym,
  8] (* Same depth used in finding function call pattern in Hover feature *)
]


expandContent[content:KeyValuePattern["method" -> "textDocument/didClose"], pos_] :=
Catch[
Module[{params, doc, uri, res},

  
  log[1, "textDocument/didClose: enter expand"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/didCloseFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didCloseFencepost"
    } ~Join~ $didCloseMethods);

  log[1, "textDocument/didClose: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/didCloseFencepost"]] :=
Module[{params, doc, uri},

  
  log[1, "textDocument/didCloseFencepost: Enter"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  $OpenFilesMap[uri] =.;

  log[1, "textDocument/didCloseFencepost: Exit"];

  {}
]



expandContent[content:KeyValuePattern["method" -> "textDocument/didSave"], pos_] :=
Catch[
Module[{params, doc, uri},

  
  log[1, "textDocument/didSave: Enter"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/didSaveFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didSaveFencepost"
    } ~Join~ $didSaveMethods);

  log[1, "textDocument/didSave: Exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/didSaveFencepost"]] :=
Module[{},

  
    log[1, "textDocument/didSaveFencepost: Enter"];

    log[1, "textDocument/didSaveFencepost: Exit"];
  

  {}
]



expandContent[content:KeyValuePattern["method" -> "textDocument/didChange"], pos_] :=
Catch[
Module[{params, doc, uri, res},

  
  log[1, "textDocument/didChange: enter expand"];
  

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/didChangeFencepost", "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ ({
      "textDocument/didChangeFencepost"
    } ~Join~ $didChangeMethods);

  log[1, "textDocument/didChange: Exit"];

  res

]]


handleContent[content:KeyValuePattern["method" -> "textDocument/didChangeFencepost"]] :=
Catch[
Module[{params, doc, uri, text, lastChange, entry, changes},
  
  
  log[1, "textDocument/didChangeFencepost: Enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  entry = Lookup[$OpenFilesMap, uri, Null];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{}]
  ];
  
  changes = params["contentChanges"];

  (*
  Currently only supporting full text, so always only apply the last change
  *)
  lastChange = changes[[-1]];

  text = lastChange["text"];

  (* 
      We do not want to keep entry["AST"] here. As the text is changed, AST needs to be re-evaluated.

      But for fast response to the Completion messages, we can use the backdated AST. 
      
      If there are multiple didChangeFencepost messages in the queue, 
          "PreviousAST" -> entry["AST"]
      would break because from the second message onwards, entry["AST"] would be Missing.
      
      So it's better to assign newly evaluated AST to entry["PreviousAST"] and use it as long as new AST is is not re-evaluated.
  *)

  entry = <|
    "Text" -> text,
    "LastChange" -> Now,
    "ScheduledJobs" -> $didChangeScheduledJobs,
    "PreviousAST" -> entry["PreviousAST"],
    "PreviousUserSymbols" -> entry["PreviousUserSymbols"]
  |>;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/didChangeFencepost: Exit"];

  {}
]]


exitGracefully[] := (
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING GRACEFULLY"];
  log[0, "\n\n"];
  shutdownLSPComm[$commProcess, $initializedComm];
  Pause[1];
  (
  (* :!CodeAnalysis::BeginBlock:: *)
  (* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
  Exit[0]
  (* :!CodeAnalysis::EndBlock:: *)
  )
)

exitSemiGracefully[] := (
  log[0, "Language Server kernel did not shutdown properly."];
  log[0, ""];
  log[0, "This is the command that was used:"];
  log[0, $CommandLine];
  log[0, ""];
  log[0, "To help diagnose the problem, run this in a notebook:\n" <>
  "Needs[\"LSPServer`\"]\n" <>
  "LSPServer`RunServerDiagnostic[{" <>
    StringJoin[Riffle[("\"" <> # <> "\"")& /@ StringReplace[$CommandLine, "\"" -> "\\\""], ", "]] <>
    "}]"];
  log[0, ""];
  log[0, "Fix any problems then restart and try again."];
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING SEMI-GRACEFULLY"];
  log[0, "\n\n"];
  shutdownLSPComm[$commProcess, $initializedComm];
  Pause[1];
  (
  (* :!CodeAnalysis::BeginBlock:: *)
  (* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
  Exit[1]
  (* :!CodeAnalysis::EndBlock:: *)
  )
)

exitHard[] := (
  log[0, "Language Server kernel did not shutdown properly."];
  log[0, ""];
  log[0, "This is the command that was used:"];
  log[0, $CommandLine];
  log[0, ""];
  log[0, "To help diagnose the problem, run this in a notebook:\n" <>
  "Needs[\"LSPServer`\"]\n" <>
  "LSPServer`RunServerDiagnostic[{" <>
    StringJoin[Riffle[("\"" <> # <> "\"")& /@ StringReplace[$CommandLine, "\"" -> "\\\""], ", "]] <>
    "}]"];
  log[0, ""];
  log[0, "Fix any problems then restart and try again."];
  log[0, "\n\n"];
  log[0, "KERNEL IS EXITING HARD"];
  log[0, "\n\n"];
  shutdownLSPComm[$commProcess, $initializedComm];
  Pause[1];
  (
  (* :!CodeAnalysis::BeginBlock:: *)
  (* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
  Exit[1]
  (* :!CodeAnalysis::EndBlock:: *)
  )
)


(*
now cleanup Startup Messages handling
*)
Module[{name},

  If[!FailureQ[$startupMessagesFile],

    name = Close[$startupMessagesFile];

    $startupMessagesText = Import[name, "Text"];

    DeleteFile[name];

    $Messages = $oldMessages
  ]
]


End[]

EndPackage[]
