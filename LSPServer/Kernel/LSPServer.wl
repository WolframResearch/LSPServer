BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the client.\
 StartServer[logDir] logs traffic to logDir."



RegisterDidOpenNotification

RegisterDidCloseNotification

RegisterDidSaveNotification

RegisterDidChangeNotification


handleContent

$OpenFilesMap

Begin["`Private`"]

Needs["LSPServer`Color`"]
Needs["LSPServer`Definitions`"]
Needs["LSPServer`Library`"]
Needs["LSPServer`Hover`"]
Needs["LSPServer`Utils`"]

Needs["CodeFormatter`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`ImplicitTokens`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


(*
This uses func := func = def idiom and is fast
*)
loadAllFuncs[]



ReadLineFromStdIn
ReadBytesFromStdIn

WriteLineToStdOut
WriteBytesToStdOut

LSPEvaluate



$ConfidenceLevel = 0.95

$ImplicitTokens = False

$ColorProvider = False

$HoverProvider = False

$CodeActionLiteralSupport = False



(*
lint objects may be printed to log files and we do not want to include ANSI control codes
*)
CodeInspector`Format`Private`$UseANSI = False




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

$DiagnosticSeverity = <|
  "Error" -> 1,
  "Warning" -> 2,
  "Information" -> 3,
  "Hint" -> 4
|>


$TextDocumentSyncKind = <|
  "None" -> 0,
  "Full" -> 1,
  "Incremental" -> 2
|>





ReadLineFromStdIn[] :=
Module[{res},
  res = libraryFunctionWrapper[readLineFromStdInFunc];
  res
]

ReadBytesFromStdIn[numBytes_Integer] :=
Module[{bytes},
  bytes = ByteArray[Developer`AllocateNumericArray["UnsignedInteger8", {numBytes}]];
  libraryFunctionWrapper[readBytesFromStdInFunc, bytes];
  bytes
]

WriteLineToStdOut[line_String] :=
Module[{res},
  res = libraryFunctionWrapper[writeLineToStdOutFunc, line];
  res
]

WriteBytesToStdOut[bytes_ByteArray] :=
Module[{res},
  res = libraryFunctionWrapper[writeBytesToStdOutFunc, bytes];
  res
]




$Debug

$Debug2



$OpenFilesMap = <||>



(*
setup the REPL to handle traffic from client
*)
StartServer[logDir_String:""] :=
Catch[
Module[{logFile, res, line, numBytesStr, numBytes, bytes, bytess, logFileStream},

  (*
  Ensure that no messages are printed to stdout
  *)
  $Messages = Streams["stderr"];

  $Debug = (logDir != "");

  If[$Debug,

    Quiet[CreateDirectory[logDir], {CreateDirectory::filex}];

    logFile = FileNameJoin[{logDir, "kernelLogFile.txt"}];

    logFileStream = OpenWrite[logFile, CharacterEncoding -> "UTF8"];

    $Messages = $Messages ~Join~ { logFileStream }
  ];

  SetOptions[$Messages, PageWidth -> Infinity];

  Write[$Messages, "$CommandLine: " //OutputForm, $CommandLine //OutputForm];

  While[True,

    (*
    Headers
    *)
    While[True,

      line = ReadLineFromStdIn[];

      If[FailureQ[line],
        Write[$Messages, "\n\n" //OutputForm];
        Write[$Messages, "invalid line from stdin: " //OutputForm, line //OutputForm];
        Write[$Messages, "KERNEL IS EXITING" //OutputForm];
        Write[$Messages, "\n\n" //OutputForm];
        Pause[1];Exit[1]
      ];

      If[$Debug2,
        Write[$Messages, "C-->S  " //OutputForm, line //OutputForm, "  (length:"<>ToString[StringLength[line]]<>")" //OutputForm];
      ];

      Which[
        StringMatchQ[line, RegularExpression["Content-Length: (\\d+)"]],
          numBytesStr = StringCases[line, RegularExpression["Content-Length: (\\d+)"] :> "$1"][[1]];
          numBytes = ToExpression[numBytesStr];
        ,
        line == "",
          Break[]
        ,
        True,
          Write[$Messages, "\n\n" //OutputForm];
          Write[$Messages, "invalid Content-Length from stdin: " //OutputForm, line //OutputForm];
          Write[$Messages, "KERNEL IS EXITING" //OutputForm];
          Write[$Messages, "\n\n" //OutputForm];
          Pause[1];Exit[1]
      ]
    ];(*While*)

    (*Content*)

    bytes = ReadBytesFromStdIn[numBytes];

    If[FailureQ[bytes],
      Write[$Messages, "\n\n" //OutputForm];
      Write[$Messages, "invalid bytes from stdin: " //OutputForm, bytes //OutputForm];
      Write[$Messages, "KERNEL IS EXITING" //OutputForm];
      Write[$Messages, "\n\n" //OutputForm];
      Pause[1];Exit[1]
    ];

    If[$Debug2,
      Write[$Messages, "C-->S  " //OutputForm, FromCharacterCode[Normal[Take[bytes, UpTo[1000]]]] //OutputForm];
    ];

    bytess = LSPEvaluate[bytes];

    Do[
    
      If[!ByteArrayQ[bytes],
        Write[$Messages, "\n\n" //OutputForm];
        Write[$Messages, "invalid bytes from stdin: " //OutputForm, bytes //OutputForm];
        Write[$Messages, "KERNEL IS EXITING" //OutputForm];
        Write[$Messages, "\n\n" //OutputForm];
        Pause[1];Exit[1]
      ];

      If[$Debug2,
        Write[$Messages, "bytes" //OutputForm, Normal[Take[bytes, UpTo[1000]]] //OutputForm];
      ];

      line = "Content-Length: " <> ToString[Length[bytes]];

      If[$Debug2,
        Write[$Messages, "C<--S  " //OutputForm, line //OutputForm, "  (length:"<>ToString[StringLength[line]]<>")" //OutputForm];
      ];

      res = WriteLineToStdOut[line];
      If[res =!= Null,
        Write[$Messages, "\n\n" //OutputForm];
        Write[$Messages, "invalid result from stdout: " //OutputForm, res //OutputForm];
        Write[$Messages, "KERNEL IS EXITING" //OutputForm];
        Write[$Messages, "\n\n" //OutputForm];
        Pause[1];Exit[1]
      ];

      line = "";

      If[$Debug2,
        Write[$Messages, "C<--S  " //OutputForm, line //OutputForm, "  (length:"<>ToString[StringLength[line]]<>")" //OutputForm];
      ];

      res = WriteLineToStdOut[line];
      If[res =!= Null,
        Write[$Messages, "\n\n" //OutputForm];
        Write[$Messages, "invalid result from stdout: " //OutputForm, res //OutputForm];
        Write[$Messages, "KERNEL IS EXITING" //OutputForm];
        Write[$Messages, "\n\n" //OutputForm];
        Pause[1];Exit[1]
      ];

      If[$Debug2,
        Write[$Messages, "C<--S  " //OutputForm, FromCharacterCode[Normal[Take[bytes, UpTo[1000]]]] //OutputForm];
      ];

      res = WriteBytesToStdOut[bytes];
      If[res =!= Null,
        Write[$Messages, "\n\n" //OutputForm];
        Write[$Messages, "invalid result from stdout: " //OutputForm, res //OutputForm];
        Write[$Messages, "KERNEL IS EXITING" //OutputForm];
        Write[$Messages, "\n\n" //OutputForm];
        Pause[1];Exit[1]
      ];
      ,
      {bytes, bytess}
    ](*Do*)
  ](*While*)
],(*Module*)
_,
(
  Write[$Messages, "\n\n" //OutputForm];
  Write[$Messages, "uncaught Throw: " //OutputForm, #1 //OutputForm];
  Write[$Messages, "KERNEL IS EXITING" //OutputForm];
  Write[$Messages, "\n\n" //OutputForm];
  Pause[1];Exit[1])&
]


(*
input string: ByteArray representing an RPC-JSON string

returns: a list of ByteArray (possibly empty), each ByteArray represents an RPC-JSON string
*)
LSPEvaluate[bytes_ByteArray] :=
Catch[
Module[{content, contents, bytess},

  content = ImportByteArray[bytes, "RawJSON"];

  contents = handleContent[content];

  If[!MatchQ[contents, {_Association...}],
    Write[$Messages, "\n\n" //OutputForm];
    Write[$Messages, "invalid contents result: " //OutputForm, contents //OutputForm];
    Write[$Messages, "KERNEL IS EXITING" //OutputForm];
    Write[$Messages, "\n\n" //OutputForm];
    Pause[1];Exit[1]
  ];

  bytess = ExportByteArray[#, "JSON"]& /@ contents;

  bytess
]]





(*
Functions in this list are called as:

func[uri]

*)
$didOpenNotifications = {publishDiagnosticsNotification}

(*
Functions in this list are called as:

func[uri]

clear lints and lines on file close

NOTE: may want to be able to control this behavior
*)
$didCloseNotifications = {publishDiagnosticsNotificationWithLints[#1, {}]&}

(*
Functions in this list are called as:

func[uri]

*)
$didSaveNotifications = {publishDiagnosticsNotification}

(*
Functions in this list are called as:

func[uri]

*)
$didChangeNotifications = {publishDiagnosticsNotification}


RegisterDidOpenNotification[func_] := AppendTo[$didOpenNotifications, func]

RegisterDidCloseNotification[func_] := AppendTo[$didCloseNotifications, func]

RegisterDidSaveNotification[func_] := AppendTo[$didSaveNotifications, func]

RegisterDidChangeNotification[func_] := AppendTo[$didChangeNotifications, func]







(*
content: JSON-RPC Association

returns: a list of associations (possibly empty), each association represents JSON-RPC
*)
handleContent[content:KeyValuePattern["method" -> "initialize"]] :=
Module[{id, params, capabilities, textDocument, codeAction, codeActionLiteralSupport, codeActionKind, valueSet,
  codeActionProviderValue, initializationOptions, confidenceLevel, colorProvider, hoverProvider, implicitTokens},

  id = content["id"];
  params = content["params"];

  If[KeyExistsQ[params, "initializationOptions"],
    initializationOptions = params["initializationOptions"];
    If[KeyExistsQ[initializationOptions, "confidenceLevel"],
      confidenceLevel = initializationOptions["confidenceLevel"];

      $ConfidenceLevel = confidenceLevel;
    ];

    If[KeyExistsQ[initializationOptions, "implicitTokens"],
      implicitTokens = initializationOptions["implicitTokens"];

      $ImplicitTokens = implicitTokens;
    ];

    If[KeyExistsQ[initializationOptions, "colorProvider"],
      colorProvider = initializationOptions["colorProvider"];

      $ColorProvider = colorProvider;
    ];
    If[KeyExistsQ[initializationOptions, "hoverProvider"],
      hoverProvider = initializationOptions["hoverProvider"];

      $HoverProvider = hoverProvider;
    ];
  ];

  capabilities = params["capabilities"];
  textDocument = capabilities["textDocument"];
  codeAction = textDocument["codeAction"];

  If[KeyExistsQ[codeAction, "codeActionLiteralSupport"],
    $CodeActionLiteralSupport = True;
    codeActionLiteralSupport = codeAction["codeActionLiteralSupport"];
    codeActionKind = codeActionLiteralSupport["codeActionKind"];
    valueSet = codeActionKind["valueSet"];
  ];

  If[$CodeActionLiteralSupport,
    codeActionProviderValue = <| "codeActionKinds" -> {"quickfix"} |>
    ,
    codeActionProviderValue = True
  ];

  If[$ImplicitTokens,
    RegisterDidOpenNotification[publishImplicitTokensNotification];
    RegisterDidCloseNotification[publishImplicitTokensNotificationWithLines[#1, {}]&];
    RegisterDidSaveNotification[publishImplicitTokensNotification];
    RegisterDidChangeNotification[publishImplicitTokensNotification];
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id,
     "result" -> <| "capabilities"-> <| "referencesProvider" -> True,
                                        "textDocumentSync" -> <| "openClose" -> True,
                                                                 "save" -> <| "includeText" -> False |>,
                                                                 "change" -> $TextDocumentSyncKind["Full"]
                                                              |>,
                                         "codeActionProvider" -> codeActionProviderValue,
                                         "colorProvider" -> $ColorProvider,
                                         "hoverProvider" -> $HoverProvider,
                                         "definitionProvider" -> True,
                                         "documentFormattingProvider" -> True
                                         (*,
                                         "documentRangeFormattingProvider" -> True*)
                                     |>
                 |>
  |>}
]


handleContent[content:KeyValuePattern["method" -> "initialized"]] :=
Module[{wolframVersion, codeParserVersion, codeInspectorVersion, lspServerVersion, codeFormatterVersion},

  wolframVersion = ToString[$VersionNumber];
  codeParserVersion = "Version" /. PacletManager`PacletInformation["CodeParser"] /. {"Version" -> "bad"};
  codeInspectorVersion = "Version" /. PacletManager`PacletInformation["CodeInspector"] /. {"Version" -> "bad"};
  codeFormatterVersion = "Version" /. PacletManager`PacletInformation["CodeFormatter"] /. {"Version" -> "bad"};
  lspServerVersion = "Version" /. PacletManager`PacletInformation["LSPServer"] /. {"Version" -> "bad"};

  {<| "jsonrpc" -> "2.0",
      "method"  -> "wolfram/versions",
      "params"  -> <| "wolframVersion"-> wolframVersion,
                    "codeParserVersion"-> codeParserVersion,
                    "codeInspectorVersion"-> codeInspectorVersion,
                    "codeFormatterVersion"-> codeFormatterVersion,
                    "lspServerVersion"-> lspServerVersion
                   |>
  |>}
]

handleContent[content:KeyValuePattern["method" -> "shutdown"]] :=
Module[{id},
  id = content["id"];
  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}
]


handleContent[content:KeyValuePattern["method" -> "exit"]] := (
  Exit[0]
)


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
Module[{params, id},
  params = content["params"];
  id = params["id"];
  {<| "jsonrpc" -> "2.0", "id" -> id, "error" -> <| "code" -> $ErrorCodes["MethodNotFound"],
                                                   "message"->"Method Not Found" |> |>}
]



handleContent[content:KeyValuePattern["method" -> "textDocument/references"]] :=
Catch[
Module[{id, params, doc, uri, file, cst, pos, line, char, cases, sym, name, srcs},

  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  pos = params["position"];
  line = pos["line"];
  char = pos["character"];

  (*
  convert from 0-based to 1-based
  *)
  line+=1;
  char+=1;

  file = normalizeURI[uri];

  cst = CodeConcreteParse[File[file], "TabWidth" -> 1];

  If[FailureQ[cst],
    Throw[cst]
  ];

  (*
  Find the name of the symbol at the position
  *)
  cases = Cases[cst, LeafNode[Symbol, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  If[cases == {},
    Throw[<|"jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>]
  ];

  sym = cases[[1]];

  name = sym["String"];

  cases = Cases[cst, LeafNode[Symbol, name, _], Infinity];

  srcs = #[[3, Key[Source] ]]& /@ cases;

  locations = (<|   "uri" -> uri,
                  "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                  "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>
               |>&[Map[Max[#, 0]&, #-1, {2}]])& /@ srcs;

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>}
]]







(*
textDocument/didOpen is a notification (so no response), but take this chance to do linting and send textDocument/publishDiagnostics
*)
handleContent[content:KeyValuePattern["method" -> "textDocument/didOpen"]] :=
Catch[
Module[{params, doc, uri, cst, text},

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  text = doc["text"];

  cst = CodeConcreteParse[text, "TabWidth" -> 1];

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

  If[$Debug2,
    Write[$Messages, "Calling didOpenNotifications: " //OutputForm, $didOpenNotifications //OutputForm];
    Write[$Messages, "with these args: " //OutputForm, {uri} //OutputForm];
  ];

  (*
  This is a File, not a String
  *)
  cst[[1]] = File;

  $OpenFilesMap[uri] = {text, cst};

  #[uri]& /@ $didOpenNotifications
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/didClose"]] :=
Module[{params, doc, uri, res},

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[$Debug2,
    Write[$Messages, "Calling didCloseNotifications: " //OutputForm, $didCloseNotifications //OutputForm];
    Write[$Messages, "with these args: " //OutputForm, {uri} //OutputForm];
  ];

  res = #[uri]& /@ $didCloseNotifications;

  $OpenFilesMap[uri] =.;

  res
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didSave"]] :=
  {}

handleContent[content:KeyValuePattern["method" -> "textDocument/didChange"]] :=
Catch[
Module[{params, doc, uri, cst, text, lastChange},
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  
  changes = params["contentChanges"];

  (*
  Currently only supporting full text, so always only apply the last change
  *)
  lastChange = changes[[-1]];

  text = lastChange["text"];

  cst = CodeConcreteParse[text, "TabWidth" -> 1];

  If[FailureQ[cst],
    Throw[cst]
  ];

  (*
  This is a File, not a String
  *)
  cst[[1]] = File;

  $OpenFilesMap[uri] = {text, cst};

  If[$Debug2,
    Write[$Messages, "Calling didChangeNotifications: " //OutputForm, $didChangeNotifications //OutputForm];
    Write[$Messages, "with these args: " //OutputForm, {uri} //OutputForm];
  ];

  #[uri]& /@ $didChangeNotifications
]]



(*
convert from CodeTools Lint severities to LSP severities
*)
lintSeverityToLSPSeverity[severity_String] :=
Switch[severity,
  "Formatting" | "ImplicitTimes", $DiagnosticSeverity["Hint"],
  "Remark", $DiagnosticSeverity["Information"],
  "Warning", $DiagnosticSeverity["Warning"],
  "Error" | "Fatal", $DiagnosticSeverity["Error"]
]


publishDiagnosticsNotification[uri_String] :=
Module[{lints, lintsWithConfidence, shadowing, cst, entry},

  entry = $OpenFilesMap[uri];

  cst = entry[[2]];

  lints = CodeInspectCST[cst];

  (*
  Might get something like FileTooLarge
  Still want to update
  *)
  If[FailureQ[lints],
    lints = {}
  ];

  lintsWithConfidence = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  If[$Debug2,
   Write[$Messages, "shadowing: " //OutputForm, ToString[shadowing, InputForm] //OutputForm];
  ];

  lints = Complement[lints, shadowing];


  (*
  Make sure to sort lints before taking
  *)
  lints = SortBy[lints, #[[4, Key[Source]]]&];

  lints = Take[lints, UpTo[CodeInspector`Summarize`$LintLimit]];



  publishDiagnosticsNotificationWithLints[uri, lints]
]


publishDiagnosticsNotificationWithLints[uri_String, lints_List] :=
Module[{diagnostics},

  (*
  
  if no lints, must still publish in order to update client

  If[lints == {},
    Throw[Null]
  ];
  *)

  diagnostics = lintToDiagnostics /@ lints;

  diagnostics = Flatten[diagnostics];

  <| "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishDiagnostics",
      "params" -> <|         "uri" -> uri,
                     "diagnostics" -> diagnostics |> |>
]






publishImplicitTokensNotification[uri_String] :=
Catch[
Module[{inspectedFileObj, lines, cst, entry},

  entry = $OpenFilesMap[uri];

  cst = entry[[2]];

  inspectedFileObj = CodeInspectImplicitTokensCSTSummarize[cst];

  (*
  Might get something like FileTooLarge
  Still want to update
  *)
  If[FailureQ[inspectedFileObj],
    Throw[publishImplicitTokensNotificationWithLines[uri, {}]]
  ];

  (*
  Even though we have:
  Format[LintTimesCharacter, StandardForm] := "\[Times]"

  we cannot evaluate Format[LintTimesCharacter, StandardForm] to get "\[Times]"
  *)
  
  lines = <|
    "line" -> #[[2]],
    "characters" -> ((# /. {
      LintTimesCharacter -> "x",
      LintOneCharacter -> "1",
      LintAllCharacter -> "A",
      LintNullCharacter -> "N",
      LintTimesOneCharacter -> "y",
      LintAllTimesCharacter -> "B",
      LintAllTimesOneCharacter -> "C",
      LintOpenOneCharacter -> "1",
      LintAllCloseCharacter -> "A",
      LintCloseTimesOneCharacter -> "y"
    })& /@ ((# /. LintMarkup[content_, ___] :> content)& /@ #[[4, 2, 2;;]]))
  |>& /@ inspectedFileObj[[2]];

  publishImplicitTokensNotificationWithLines[uri, lines]
]]


publishImplicitTokensNotificationWithLines[uri_String, lines_List] :=
Module[{},

  <| "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishImplicitTokens",
      "params" -> <|   "uri" -> uri,
                     "lines" -> lines |> |>
]






lintToDiagnostics[InspectionObject[tag_, message_, severity_, data_]] :=
Catch[
Module[{srcs},

  If[!KeyExistsQ[data, Source],
    (*
    It is possible that abstracted problems may not have Source

    An example would be  a < b > c  being abstracted as an Inequality expression

    Inequality is an undocumented symbol, but it does not actually show up in the source code

    So it would be wrong to report "Inequality is an undocumented symbol" for  a < b > c
    *)
    Throw[{}]
  ];

  srcs = { data[Source] } ~Join~ Lookup[data, "AdditionalSources", {}];

  ((<|     "code" -> tag,
        "message" -> plainify[message],
       "severity" -> lintSeverityToLSPSeverity[severity],
          "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                          "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
         "source" -> "wolfram lint" |>)&[Map[Max[#, 0]&, #-1, {2}]])& /@ srcs
]]



handleContent[content:KeyValuePattern["method" -> "textDocument/codeAction"]] :=
Catch[
Module[{id, params, doc, uri, actions, range, lints, lspAction, lspActions, edit, diagnostics,
  command, label, actionData, actionSrc, replacementNode, insertionNode, replacementText, lintsWithConfidence,
  shadowing, insertionText, cursor},
  
  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  range = params["range"];

  cursor = { { range["start"]["line"], range["start"]["character"] },
             {   range["end"]["line"],   range["end"]["character"] } };

  (* convert to 1-based *)
  cursor+=1;

  If[$Debug2,
    Write[$Messages, "cursor: " //OutputForm, ToString[cursor] //OutputForm];
  ];

  file = normalizeURI[uri];

  lints = CodeInspect[File[file], "TabWidth" -> 1];

  If[$Debug2,
    Write[$Messages, "lints: " //OutputForm, ToString[lints, InputForm] //OutputForm];
  ];

  (*
  Might get something like FileTooLarge
  *)
  If[FailureQ[lints],
    Throw[{<|"jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  lintsWithConfidence = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  If[$Debug2,
   Write[$Messages, "shadowing: " //OutputForm, ToString[shadowing, InputForm] //OutputForm];
  ];

  lints = Complement[lints, shadowing];

  lspActions = {};

  Do[

    diagnostics = lintToDiagnostics[lint];

    If[$Debug2,
      Write[$Messages, "diagnostics: " //OutputForm, ToString[diagnostics] //OutputForm];
    ];

    actions = Cases[lint, CodeAction[_, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, cursor]]], Infinity];

    If[$Debug2,
      Write[$Messages, "actions: " //OutputForm, ToString[actions] //OutputForm];
    ];

    Do[

      label = action[[1]];

      label = plainify[label];

      command = action[[2]];
      actionData = action[[3]];

      actionSrc = actionData[Source];

      Switch[command,

        InsertNode,

        insertionNode = actionData["InsertionNode"];

        If[$Debug2,
          Write[$Messages, "insertionNode: " //OutputForm, ToString[insertionNode] //OutputForm];
        ];

        (*
        For inserting, don't use the [start, end) range, only use [start, start)
        *)
        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                              "end" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |> |>,
                                            "newText" -> ToSourceCharacterString[insertionNode]|> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <|      "title"  -> label,
                             "kind"  -> "quickfix",
                             "edit"  -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        InsertText,

        insertionText = actionData["InsertionText"];

        If[$Debug2,
          Write[$Messages, "insertionText: " //OutputForm, ToString[insertionText] //OutputForm];
        ];

        (*
        For inserting, don't use the [start, end) range, only use [start, start)
        *)
        edit = (<| "changes" -> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                               "end" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |> |>,
                                             "newText" -> insertionText|> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <|       "title" -> label,
                              "kind" -> "quickfix",
                              "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        DeleteNode,

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                              "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                            "newText" -> "" |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <|       "title" -> label,
                              "kind" -> "quickfix",
                              "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        ReplaceNode,

        replacementNode = actionData["ReplacementNode"];

        If[$Debug2,
          Write[$Messages, "replacementNode: " //OutputForm, ToString[replacementNode] //OutputForm];
        ];

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                              "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                            "newText" -> ToSourceCharacterString[replacementNode] |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <|        "title" -> label,
                               "kind" -> "quickfix",
                               "edit" -> edit,
                        "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        ReplaceText,

        replacementText = actionData["ReplacementText"];

        If[$Debug2,
          Write[$Messages, "replacementText: " //OutputForm, ToString[replacementText]];
        ];

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                              "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                            "newText" -> replacementText|> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <|       "title" -> label,
                              "kind" -> "quickfix",
                              "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

      ]

      ,
      {action, actions}
    ]

    ,
    {lint, lints}
  ];

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> lspActions |>}
]]



handleContent[content:KeyValuePattern["method" -> "textDocument/formatting"]] :=
Catch[
Module[{params, doc, uri, id, file, cst, formatted, startLineCol, endLineCol, textEdit, options, tabSize, insertSpaces,
  indentationString},

  id = content["id"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  options = params["options"];
  tabSize = options["tabSize"];
  insertSpaces = options["insertSpaces"];

  file = normalizeURI[uri];

  cst = CodeConcreteParse[File[file], "TabWidth" -> tabSize];

  startLineCol = cst[[2, 1, 3, Key[Source], 1]];
  endLineCol = cst[[2, -1, 3, Key[Source], 2]];

  startLineCol--;
  endLineCol--;

  If[insertSpaces,
    indentationString = StringJoin[Table[" ", {tabSize}]]
    ,
    indentationString = "\t"
  ];

  formatted = CodeFormatCST[cst, "TabWidth" -> tabSize, "IndentationString" -> indentationString];

  If[FailureQ[cst],
    Throw[cst]
  ];

  textEdit = <| "range" -> <| "start" -> <| "line" -> startLineCol[[1]], "character" -> startLineCol[[2]] |>,
                              "end" ->   <| "line" -> endLineCol[[1]], "character" -> endLineCol[[2]] |> |>,
                "newText" -> formatted|>;

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]

(*
handleContent[content:KeyValuePattern["method" -> "textDocument/rangeFormatting"]] :=
Catch[
Module[{params, doc, uri, id, file, cst, formatted, startLineCol, endLineCol, textEdit},

  id = content["id"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  file = normalizeURI[uri];

  cst = CodeConcreteParse[ xx only a range xx File[file], "TabWidth" -> 1];

  startLineCol = cst[[2, 1, 3, Key[Source], 1]];
  endLineCol = cst[[2, -1, 3, Key[Source], 2]];

  startLineCol--;
  endLineCol--;

  formatted = CodeFormatCST[cst];

  If[FailureQ[cst],
    Throw[cst]
  ];

  textEdit = <| "range" -> <| "start" -> <| "line" -> startLineCol[[1]], "character" -> startLineCol[[2]] |>,
                              "end" ->   <| "line" -> endLineCol[[1]], "character" -> endLineCol[[2]] |> |>,
                "newText" -> formatted|>;

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]
*)

End[]

EndPackage[]
