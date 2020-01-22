BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the wolfram_lsp_proxy script.\
 StartServer[logFile] logs traffic to logFile."



handleContent

Begin["`Private`"]

Needs["LSPServer`Color`"]
Needs["LSPServer`Utils`"]
Needs["Lint`"]
Needs["Lint`Report`"]
Needs["Lint`Utils`"]
Needs["AST`"]
Needs["AST`Utils`"]




LSPEvaluate



$ConfidenceLevel = 0.95

$ColorProvider = False


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






$logFileStream

$Debug


(*
setup the REPL to handle traffic from wolfram_lsp_proxy script
*)
StartServer[logFile_String:""] :=
Module[{},

  $Debug = (logFile != "");

  If[$Debug,
    
    $logFileStream = OpenWrite[logFile, CharacterEncoding -> "UTF8"];

    WriteString[$logFileStream, "$CommandLine: ", $CommandLine, "\n"];
  ];

  (*
  Convert the line of text from stdin into a String
  *)
  $PreRead = ToString[#, InputForm]&;
  
  $Pre = LSPEvaluate;
  
  (*
  Some mode is turned on where everything is printed as InputForm.
  Control by wrapping in OutputForm, and make sure to return Null.
  *)
  $Post = Print[OutputForm[#]]&;
  
  (*
  Ensure that no messages are printed to stdout
  *)
  If[$Debug,
    $Messages = { $logFileStream }
    ,
    $Messages = {}
  ];
]


(*
input string: RPC-JSON string on a single line

returns: RPC-JSON string
*)
LSPEvaluate[string_String] :=
Catch[
Module[{content, json},

  If[$Debug,
    WriteString[$logFileStream, "P-->K  ", string, "\n"];
  ];

  content = string;

  (*
  RawJSON assumes UTF8-encoded strings and cannot handle non-ASCII characters

  e.g., the single \[Alpha] character needs to be in the string as \[CapitalIHat]\[PlusMinus]

  This is all a little mixed up

  *)
  content = ToString[content, CharacterEncoding -> "UTF8"];

  content = ImportString[content, "RawJSON"];

  content = handleContent[content];

  If[content === Null,
    Throw["Null"]
  ];
  If[!AssociationQ[content],

    If[$Debug,
      WriteString[$logFileStream, "ERROR\n"];
      WriteString[$logFileStream, content, "\n"];
    ];

    Exit[1]
  ];

  json = ExportString[content, "JSON"];

  If[!StringQ[json],

    If[$Debug,
      WriteString[$logFileStream, "ERROR\n"];
      WriteString[$logFileStream, content, "\n"];
    ];

    Exit[2]
  ];

  json = StringReplace[json, "\n" -> ""];

  If[$Debug,
    WriteString[$logFileStream, "P<--K  ", json, "\n"];
  ];

  json
]]



(*
content: JSON-RPC Association

returns: JSON-RPC Association
*)
handleContent[content:KeyValuePattern["method" -> "initialize"]] :=
Module[{id, params, capabilities, textDocument, codeAction, codeActionLiteralSupport, codeActionKind, valueSet,
  codeActionProviderValue, initializationOptions, confidenceLevel, colorProvider},

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

  <| "jsonrpc" -> "2.0", "id" -> id,
     "result" -> <| "capabilities"-> <| "referencesProvider" -> True,
                                        "textDocumentSync" -> <| "openClose" -> True,
                                                                 "save" -> <| "includeText" -> False |>,
                                                                 "change" -> $TextDocumentSyncKind["None"]
                                                              |>,
                                         "codeActionProvider" -> codeActionProviderValue,
                                         "colorProvider" -> $ColorProvider
                                     |>
                 |>
  |>
]


(*
Do not send a response back
*)
handleContent[content:KeyValuePattern["method" -> "initialized"]] := (
  Null
)



handleContent[content:KeyValuePattern["method" -> "shutdown"]] :=
Module[{id},
  id = content["id"];
  <| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>
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
  <| "jsonrpc" -> "2.0", "id" -> id, "error" -> <| "code" -> $ErrorCodes["MethodNotFound"],
                                                   "message"->"Method Not Found" |> |>
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

  cst = ConcreteParseFile[File[file]];

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

  <|"jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>
]]







(*
textDocument/didOpen is a notification (so no response), but take this chance to do linting and send textDocument/publishDiagnostics
*)
handleContent[content:KeyValuePattern["method" -> "textDocument/didOpen"]] :=
Module[{params, doc, uri},

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  
  publishDiagnosticsNotification[uri]
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
  publishDiagnosticsNotification[uri, {}]
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didSave"]] :=
Module[{params, doc, uri},
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  
  publishDiagnosticsNotification[uri]
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didChange"]] := (
  Null
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

  lints = LintFile[File[file]];

  (*
  Might get something like FileTooLarge
  Still want to update
  *)
  If[FailureQ[lints],
    lints = {}
  ];

  lintsWithConfidence = Cases[lints, Lint[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, Lint[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  If[$Debug,
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



lintToDiagnostics[Lint[tag_, message_, severity_, data_]] :=
Catch[
Module[{srcs},

  If[!KeyExistsQ[data, Source],
    (*
    It is possible that abstracted problems may not have Source

    An example would be  a < b > c  being abstracted as an Inequality expression

    Inequality is an undocumented symbol, but it does actually show up in the source code

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

  If[$Debug,
    WriteString[$logFileStream, "cursor: ", ToString[cursor], "\n"];
  ];

  file = normalizeURI[uri];

  lints = LintFile[File[file]];

  If[$Debug,
    WriteString[$logFileStream, "lints: ", ToString[lints, InputForm], "\n"];
  ];

  (*
  Might get something like FileTooLarge
  *)
  If[FailureQ[lints],
    Throw[<|"jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>]
  ];

  lintsWithConfidence = Cases[lints, Lint[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, Lint[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  If[$Debug,
   WriteString[$logFileStream, "shadowing: ", ToString[shadowing, InputForm], "\n"];
  ];

  lints = Complement[lints, shadowing];

  lspActions = {};

  Do[

    diagnostics = lintToDiagnostics[lint];

    If[$Debug,
      WriteString[$logFileStream, "diagnostics: ", ToString[diagnostics], "\n"];
    ];

    actions = Cases[lint, CodeAction[_, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, cursor]]], Infinity];

    If[$Debug,
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

        If[$Debug,
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

        If[$Debug,
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

        If[$Debug,
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

        If[$Debug,
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

  <|"jsonrpc" -> "2.0", "id" -> id, "result" -> lspActions |>
]]

End[]

EndPackage[]
