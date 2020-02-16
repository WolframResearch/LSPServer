BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the client.\
 StartServer[logDir] logs traffic to logDir."



handleContent

Begin["`Private`"]

Needs["LSPServer`Color`"]
Needs["LSPServer`Library`"]
Needs["LSPServer`Hover`"]
Needs["LSPServer`Utils`"]

Needs["CodeInspector`"]
Needs["CodeInspector`ImplicitTimes`"]
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

$ColorProvider = False

$HoverProvider = False



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






$logFileStream

$Debug

$Debug2


(*
setup the REPL to handle traffic from client
*)
StartServer[logDir_String:""] :=
Module[{logFile, res, line, numBytesStr, numBytes, bytes, bytess},

  $Debug = (logDir != "");

  If[$Debug,

    logFile = FileNameJoin[{logDir, "kernelLogFile.txt"}];

    $logFileStream = OpenWrite[logFile, CharacterEncoding -> "UTF8"];

    WriteString[$logFileStream, "$CommandLine: ", $CommandLine, "\n"];
  ];

  (*
  Ensure that no messages are printed to stdout
  *)
  If[$Debug,
    $Messages = { $logFileStream }
    ,
    $Messages = {}
  ];

  While[True,

    (*
    Headers
    *)
    While[True,

      line = ReadLineFromStdIn[];

      If[FailureQ[line],
        If[$Debug,
          WriteString[$logFileStream, "C-->S  ", line, "\n"];
        ];
        Exit[1]
      ];

      If[$Debug2,
        WriteString[$logFileStream, "C-->S  ", line, "  (length:"<>ToString[StringLength[line]]<>")\n"];
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
          Exit[1]
      ]
    ];(*While*)

    (*Content*)

    bytes = ReadBytesFromStdIn[numBytes];

    If[FailureQ[bytes],
      If[$Debug,
        WriteString[$logFileStream, "C-->S  ", bytes, "\n"];
      ];
      Exit[1]
    ];

    If[$Debug2,
      WriteString[$logFileStream, "C-->S  ", FromCharacterCode[Normal[Take[bytes, UpTo[1000]]]], "\n"];
    ];

    bytess = LSPEvaluate[bytes];

    Do[
    
      If[!ByteArrayQ[bytes],
        If[$Debug,
          WriteString[$logFileStream, bytes, "\n"]
        ];
        Exit[1]
      ];

      If[$Debug2,
        WriteString[$logFileStream, "bytes", Normal[Take[bytes, UpTo[1000]]], "\n"];
      ];

      line = "Content-Length: " <> ToString[Length[bytes]];

      If[$Debug2,
        WriteString[$logFileStream, "C<--S  ", line, "  (length:"<>ToString[StringLength[line]]<>")\n"];
      ];

      res = WriteLineToStdOut[line];
      If[res =!= Null,
        If[$Debug,
          WriteString[$logFileStream, "C<--S  ", res, "\n"];
        ];
        Exit[1]
      ];

      line = "";

      If[$Debug2,
        WriteString[$logFileStream, "C<--S  ", line, "  (length:"<>ToString[StringLength[line]]<>")\n"];
      ];

      res = WriteLineToStdOut[line];
      If[res =!= Null,
        If[$Debug,
          WriteString[$logFileStream, "C<--S  ", res, "\n"];
        ];
        Exit[1]
      ];

      If[$Debug2,
        WriteString[$logFileStream, "C<--S  ", FromCharacterCode[Normal[Take[bytes, UpTo[1000]]]], "\n"];
      ];

      res = WriteBytesToStdOut[bytes];
      If[res =!= Null,
        If[$Debug,
          WriteString[$logFileStream, "C<--S  ", res, "\n"];
        ];
        Exit[1]
      ];
      ,
      {bytes, bytess}
    ](*Do*)
  ](*While*)
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
    If[$Debug,
      WriteString[$logFileStream, contents, "\n"];
    ];
    Exit[1]
  ];

  bytess = ExportByteArray[#, "JSON"]& /@ contents;

  bytess
]]







(*
content: JSON-RPC Association

returns: a list of associations (possibly empty), each association represents JSON-RPC
*)
handleContent[content:KeyValuePattern["method" -> "initialize"]] :=
Module[{id, params, capabilities, textDocument, codeAction, codeActionLiteralSupport, codeActionKind, valueSet,
  codeActionProviderValue, initializationOptions, confidenceLevel, colorProvider, hoverProvider},

  id = content["id"];
  params = content["params"];

  If[KeyExistsQ[params, "initializationOptions"],
    initializationOptions = params["initializationOptions"];
    If[KeyExistsQ[initializationOptions, "confidenceLevel"],
      confidenceLevel = initializationOptions["confidenceLevel"];

      $ConfidenceLevel = confidenceLevel;
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

  {<| "jsonrpc" -> "2.0", "id" -> id,
     "result" -> <| "capabilities"-> <| "referencesProvider" -> True,
                                        "textDocumentSync" -> <| "openClose" -> True,
                                                                 "save" -> <| "includeText" -> False |>,
                                                                 "change" -> $TextDocumentSyncKind["None"]
                                                              |>,
                                         "codeActionProvider" -> codeActionProviderValue,
                                         "colorProvider" -> $ColorProvider,
                                         "hoverProvider" -> $HoverProvider
                                     |>
                 |>
  |>}
]


(*
Do not send a response back
*)
handleContent[content:KeyValuePattern["method" -> "initialized"]] := (
  {}
)



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

  cst = CodeConcreteParse[File[file]];

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
Module[{params, doc, uri},

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  
  {publishDiagnosticsNotification[uri], publishImplicitTimesNotification[uri]}
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didClose"]] :=
Module[{params, doc, uri},

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  (*
  clear lints on file close

  NOTE: may want to be able to control this behavior
  *)
  {publishDiagnosticsNotification[uri, {}], publishImplicitTimesNotification[uri, {}]}
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didSave"]] :=
Module[{params, doc, uri},
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  
  {publishDiagnosticsNotification[uri], publishImplicitTimesNotification[uri]}
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didChange"]] := (
  {}
)



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
Module[{file, lints, lintsWithConfidence, shadowing},

  file = normalizeURI[uri];

  lints = CodeInspect[File[file]];

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
   WriteString[$logFileStream, "shadowing: ", ToString[shadowing, InputForm], "\n"];
  ];

  lints = Complement[lints, shadowing];

  publishDiagnosticsNotification[uri, lints]
]


publishDiagnosticsNotification[uri_String, lints_List] :=
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






publishImplicitTimesNotification[uri_String] :=
Catch[
Module[{file, inspectedFileObj, lines},

  file = normalizeURI[uri];

  inspectedFileObj = CodeInspectImplicitTimesSummarize[File[file]];

  (*
  Might get something like FileTooLarge
  Still want to update
  *)
  If[FailureQ[inspectedFileObj],
    Throw[publishImplicitTimesNotification[uri, {}]]
  ];

  (*
  Even though we have:
  Format[LintTimesCharacter, StandardForm] := "\[Times]"

  we cannot evaluate Format[LintTimesCharacter, StandardForm] to get "\[Times]"
  *)

  lines = <| "line" -> #[[2]], "characters" -> ((# /. {CodeInspector`Format`LintTimesCharacter -> "\[Times]"})& /@ ((# /. LintMarkup[content_] :> content)& /@ #[[4, 2, 2;;]])) |> & /@ inspectedFileObj[[2]];

  publishImplicitTimesNotification[uri, lines]
]]


publishImplicitTimesNotification[uri_String, lines_List] :=
Module[{},

  <| "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishImplicitTimes",
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
    WriteString[$logFileStream, "cursor: ", ToString[cursor], "\n"];
  ];

  file = normalizeURI[uri];

  lints = CodeInspect[File[file]];

  If[$Debug2,
    WriteString[$logFileStream, "lints: ", ToString[lints, InputForm], "\n"];
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
   WriteString[$logFileStream, "shadowing: ", ToString[shadowing, InputForm], "\n"];
  ];

  lints = Complement[lints, shadowing];

  lspActions = {};

  Do[

    diagnostics = lintToDiagnostics[lint];

    If[$Debug2,
      WriteString[$logFileStream, "diagnostics: ", ToString[diagnostics], "\n"];
    ];

    actions = Cases[lint, CodeAction[_, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, cursor]]], Infinity];

    If[$Debug2,
      WriteString[$logFileStream, "actions: ", ToString[actions], "\n"];
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
          WriteString[$logFileStream, "insertionNode: ", ToString[insertionNode], "\n"];
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
          WriteString[$logFileStream, "insertionText: ", ToString[insertionText], "\n"];
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
          WriteString[$logFileStream, "replacementNode: ", ToString[replacementNode], "\n"];
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
          WriteString[$logFileStream, "replacementText: ", ToString[replacementText], "\n"];
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

End[]

EndPackage[]
