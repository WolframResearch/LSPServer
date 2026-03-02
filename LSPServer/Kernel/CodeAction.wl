BeginPackage["LSPServer`CodeAction`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/codeAction"], pos_] :=
Catch[
Module[{params, id, doc, uri, res},

  
  log[1, "textDocument/codeAction: enter expand"];
  

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];
    
    Throw[{<| "method" -> "textDocument/codeActionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/codeActionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/suppressedRegions",
    "textDocument/runConcreteDiagnostics",
    "textDocument/aggregateParse",
    "textDocument/runAggregateDiagnostics",
    "textDocument/abstractParse",
    "textDocument/runAbstractDiagnostics",
    "textDocument/codeActionFencepost"
  };

  log[1, "textDocument/codeAction: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/codeActionFencepost"]] :=
Catch[
Module[{id, params, doc, uri, actions, range, lints, lspAction, lspActions, edit, diagnostics,
  command, label, actionData, actionSrc, replacementNode, insertionNode, replacementText, lintsWithConfidence,
  shadowing, insertionText, cursor, entry, text, cst, agg, ast, cstLints, aggLints, astLints},
  
  
  log[1, "textDocument/codeActionFencepost: enter"];
  

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];
    
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  range = params["range"];

  cursor = { { range["start"]["line"], range["start"]["character"] },
             { range["end"]["line"], range["end"]["character"] } };

  (* convert from 0-based to 1-based *)
  cursor+=1;

  log[2, "cursor: ", ToString[cursor]];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  cstLints = entry["CSTLints"];

  (*
  Might get something like FileTooLarge
  *)
  If[FailureQ[cstLints],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];
  
  aggLints = entry["AggLints"];

  astLints = entry["ASTLints"];

  lints = cstLints ~Join~ aggLints ~Join~ astLints;

  log[2, "lints: ", stringLineTake[StringTake[ToString[lints, InputForm], UpTo[1000]], UpTo[20]]];
  log[2, "...\n"];

  lintsWithConfidence = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];


  (*

  Disable shadow filtering for now

  Below is quadratic time

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  If[$Debug2,
   Write[$Messages, "shadowing: " //OutputForm, ToString[shadowing, InputForm] //OutputForm];
  ];

  lints = Complement[lints, shadowing];
  *)
  
  (*
  Make sure to sort lints before taking

  Sort by severity, then sort by Source

  severityToInteger maps "Remark" -> 1 and "Fatal" -> 4, so make sure to negate that
  *)
  lints = SortBy[lints, {-severityToInteger[#[[3]]]&, #[[4, Key[Source]]]&}];

  lints = Take[lints, UpTo[CodeInspector`Summarize`$DefaultLintLimit]];

  
  lspActions = {};

  Do[

    diagnostics = lintToDiagnostics[lint];

    log[2, "diagnostics (up to 20): ", ToString[Take[diagnostics, UpTo[20]]]];

    (*
    Need to filter the actions that match the cursor
    *)
    actions = Cases[lint, CodeAction[_, _, _], Infinity];

    log[2, "actions (up to 20): ", ToString[Take[actions, UpTo[20]]]];

    (*
    Need to filter the actions that match the cursor
    *)
    actions = Cases[actions, CodeAction[_, _, KeyValuePattern[Source -> src_ /; SourceMemberIntersectingQ[src, cursor]]]];

    log[2, "actions (up to 20): ", ToString[Take[actions, UpTo[20]]]];

    Do[

      label = action[[1]];

      label = plainify[label];

      command = action[[2]];
      actionData = action[[3]];

      actionSrc = actionData[Source];

      Switch[command,

        InsertNode,

        insertionNode = actionData["InsertionNode"];

        log[2, "insertionNode: ", ToString[insertionNode]];

        (*
        For inserting, don't use the [start, end) range, only use [start, start)
        *)
        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |> |>,
                                              "newText" -> ToSourceCharacterString[insertionNode] |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title"  -> label,
                       "kind"  -> "quickfix",
                       "edit"  -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        InsertText,

        insertionText = actionData["InsertionText"];

        log[2, "insertionText: ", ToString[insertionText]];

        (*
        For inserting, don't use the [start, end) range, only use [start, start)
        *)
        edit = (<| "changes" -> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                               "end" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |> |>,
                                               "newText" -> insertionText|> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction];

        ,

        DeleteNode,

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> "" |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        ReplaceNode,

        replacementNode = actionData["ReplacementNode"];

        log[2, "replacementNode: ", ToString[replacementNode]];

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> ToSourceCharacterString[replacementNode] |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <|  "title" -> label,
                        "kind" -> "quickfix",
                        "edit" -> edit,
                        "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        ReplaceText,

        replacementText = actionData["ReplacementText"];

        log[2, "replacementText: ", ToString[replacementText]];

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> replacementText |> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        DeleteText,

        edit = (<| "changes"-> <| uri -> { <| "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                                            "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>,
                                              "newText" -> ""|> } |> |>)&[Map[Max[#, 0]&, actionSrc-1, {2}]];

        lspAction = <| "title" -> label,
                       "kind" -> "quickfix",
                       "edit" -> edit,
                       "diagnostics" -> diagnostics |>;

        AppendTo[lspActions, lspAction]

        ,

        _,

        log[2, "UNSUPPORTED COMMAND: ", command];

      ]

      ,
      {action, actions}
    ]

    ,
    {lint, lints}
  ];

  res = {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> lspActions |>};

  log[1, "textDocument/codeActionFencepost: exit"];

  res
]]

End[]

EndPackage[]
