BeginPackage["LSPServer`Diagnostics`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeInspector`"]
Needs["CodeInspector`SuppressedRegions`"] (* for SuppressedRegions *)
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"] (* for scopingDataObject *)


expandContent[content:KeyValuePattern["method" -> "textDocument/runDiagnostics"], pos_] :=
Catch[
Module[{params, doc, uri, res},

  log[1, "textDocument/runDiagnostics: enter expand"];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/suppressedRegions",
    "textDocument/runConcreteDiagnostics",
    "textDocument/aggregateParse",
    "textDocument/runAggregateDiagnostics",
    "textDocument/abstractParse",
    "textDocument/runAbstractDiagnostics",
    "textDocument/runScopingData", (* implemented in SemanticTokens.wl *)
    "textDocument/runScopingDiagnostics"
  };

  log[1, "textDocument/runDiagnostics: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/suppressedRegions"]] :=
Catch[
Module[{params, doc, uri, entry, cst, suppressedRegions},

  log[1, "textDocument/suppressedRegions: enter"];

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

  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions =!= Null,
    Throw[{}]
  ];
  
  cst = entry["CST"];

  log[2, "before SuppressedRegions"];

  suppressedRegions = SuppressedRegions[cst];

  log["after SuppressedRegions"];
  
  entry["SuppressedRegions"] = suppressedRegions;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/suppressedRegions: exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runConcreteDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, cst, cstLints, suppressedRegions},

  log[1, "textDocument/runConcreteDiagnostics: enter"];

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

  cstLints = Lookup[entry, "CSTLints", Null];

  If[cstLints =!= Null,
    Throw[{}]
  ];
  
  cst = entry["CST"];


  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions == Null,
    suppressedRegions = SuppressedRegions[cst];
    entry["SuppressedRegions"] = suppressedRegions;
  ];

  log[2, "before CodeInspectCST"];

  cstLints = CodeInspectCST[cst, "AggregateRules" -> <||>, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

  log[2, "after CodeInspectCST"];

  If[!MatchQ[cstLints, _List],
    log["cstLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  log[2, "cstLints: ", #["Tag"]& /@ cstLints];

  entry["CSTLints"] = cstLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runConcreteDiagnostics: exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAggregateDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, agg, aggLints, suppressedRegions},

  log[1, "textDocument/runAggregateDiagnostics: Enter"];

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

  aggLints = Lookup[entry, "AggLints", Null];

  If[aggLints =!= Null,
    Throw[{}]
  ];

  agg = entry["Agg"];

  suppressedRegions = entry["SuppressedRegions"];

  log[2, "before CodeInspectAgg"];

  aggLints = CodeInspectAgg[agg, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

  log[2, "after CodeInspectAgg"];

  If[!MatchQ[aggLints, _List],
    log[2, "aggLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  log[2, "aggLints: ", #["Tag"]& /@ aggLints];

  entry["AggLints"] = aggLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runAggregateDiagnostics: Exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAbstractDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, ast, cst, astLints, suppressedRegions},

  log[1, "textDocument/runAbstractDiagnostics: enter"];

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

  astLints = Lookup[entry, "ASTLints", Null];

  If[astLints =!= Null,
    Throw[{}]
  ];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[{}]
  ];

  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions == Null,
    cst = entry["CST"];
    suppressedRegions = SuppressedRegions[cst];
    entry["SuppressedRegions"] = suppressedRegions;
  ];
  

  log[2, "before CodeInspectAST"];

  astLints = CodeInspectAST[ast, "SuppressedRegions" -> suppressedRegions];

  log[2, "after CodeInspectAST"];

  If[!MatchQ[astLints, _List],
    log[2, "astLints: NOT A LIST!!!"];
    Throw[{}]
  ];

  log[2, "astLints: ", #["Tag"]& /@ astLints];

  entry["ASTLints"] = astLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runAbstractDiagnostics: exit"];

  {}
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runScopingDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, cst, scopingLints, scopingData, filtered, suppressedRegions, isActive},

  log[1, "textDocument/runScopingDiagnostics: enter"];

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

  scopingLints = Lookup[entry, "ScopingLints", Null];

  If[scopingLints =!= Null,
    Throw[{}]
  ];

  scopingData = entry["ScopingData"];

  log[2, "scopingData (up to 20): ", Take[scopingData, UpTo[20]]];

  (*
  Filter those that have non-empty modifiers
  *)
  filtered = Cases[scopingData, scopingDataObject[_, _, {_, ___}, _]];

  scopingLints = scopingDataObjectToLints /@ filtered;

  scopingLints = Flatten[scopingLints];

  (*
  Filter out suppressed
  *)

  suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

  If[suppressedRegions == Null,
    cst = entry["CST"];
    suppressedRegions = SuppressedRegions[cst];
    entry["SuppressedRegions"] = suppressedRegions;
  ];

  isActive = makeIsActiveFunc[suppressedRegions];

  scopingLints = Select[scopingLints, isActive];
  
  (*
  If $SemanticTokens, then only keep:
  errors

  These will be semantic highlighted AND shown in diagnostics
  Everything else will just be semantic highlighted


  If NOT $SemanticTokens, then only keep:
  errors
  unused variables

  Everything else, such as shadowed and unused parameters is a bit too noisy
  *)
  If[$SemanticTokens,
    scopingLints =
      Cases[scopingLints, InspectionObject[_, _, "Warning" | "Error" | "Fatal", _]]
    ,
    scopingLints =
      Cases[scopingLints,
        InspectionObject[_, _, "Warning" | "Error" | "Fatal", _] |
          InspectionObject["UnusedVariable", _, "Scoping", _]]
  ];

  log[2, "scopingLints: ", #["Tag"]& /@ scopingLints];

  entry["ScopingLints"] = scopingLints;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runScopingDiagnostics: exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/clearDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry},

  log[1, "textDocument/clearDiagnostics: enter"];

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
  
  entry["CSTLints"] =.;

  entry["AggLints"] =.;
  
  entry["ASTLints"] =.;

  entry["ScopingLints"] =.;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/clearDiagnostics: exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/publishDiagnostics"]] :=
Catch[
Module[{params, doc, uri, entry, lints, lintsWithConfidence, cstLints, aggLints, astLints, scopingLints, diagnostics, res},

  log[1, "textDocument/publishDiagnostics: enter"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{}]
  ];
  
  entry = Lookup[$OpenFilesMap, uri, Null];

  (*
  Possibly cleared
  *)
  If[entry === Null,
    Throw[{<| "jsonrpc" -> "2.0",
              "method" -> "textDocument/publishDiagnostics",
              "params" -> <| "uri" -> uri,
                              "diagnostics" -> {} |> |>}]
  ];

  (*
  Possibly cleared
  *)
  cstLints = Lookup[entry, "CSTLints", {}];

  (*
  Possibly cleared
  *)
  aggLints = Lookup[entry, "AggLints", {}];

  (*
  Possibly cleared
  *)
  astLints = Lookup[entry, "ASTLints", {}];

  (*
  Possibly cleared
  *)
  scopingLints = Lookup[entry, "ScopingLints", {}];

  lints = cstLints ~Join~ aggLints ~Join~ astLints ~Join~ scopingLints;

  log[2, "lints: ", #["Tag"]& /@ lints];


  lintsWithConfidence = Cases[lints, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _]]];

  lints = Cases[lintsWithConfidence, InspectionObject[_, _, _, KeyValuePattern[ConfidenceLevel -> _?(GreaterEqualThan[$ConfidenceLevel])]]];

  (*

  Disable shadow filtering for now

  Below is quadratic time

  shadowing = Select[lints, Function[lint, AnyTrue[lints, shadows[lint, #]&]]];

  lints = Complement[lints, shadowing];
  *)

  
  (*
  Make sure to sort lints before taking

  Sort by severity, then sort by Source

  severityToInteger maps "Remark" -> 1 and "Fatal" -> 4, so make sure to negate that
  *)
  lints = SortBy[lints, {-severityToInteger[#[[3]]]&, #[[4, Key[Source]]]&}];

  lints = Take[lints, UpTo[CodeInspector`Summarize`$DefaultLintLimit]];

  log[2, "lints: ", #["Tag"]& /@ lints];

  diagnostics = lintToDiagnostics /@ lints;

  diagnostics = Flatten[diagnostics];

  res = {<| "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishDiagnostics",
      "params" -> <| "uri" -> uri,
                      "diagnostics" -> diagnostics |> |>};

  log[1, "textDocument/publishDiagnostics: exit"];

  res
]]




(*
returns a function lint -> True|False
*)
makeIsActiveFunc[suppressedRegions_] :=
  Function[{lint},
    AllTrue[suppressedRegions,
      Function[{region},
        !SourceMemberQ[region[[1;;2]], lint] ||
          AllTrue[region[[3]],
            Function[{suppressed}, isTagActive[lint, suppressed]]
          ]
      ]
    ]
  ]


isTagActive[InspectionObject[tag1_, _, _, KeyValuePattern["Argument" -> arg1_]], {tag2_, arg2_}] :=
  !(tag1 === tag2 && arg1 === arg2)

(*
The lint has an Argument, but there is no argument in the suppressed
*)
isTagActive[InspectionObject[_, _, _, KeyValuePattern["Argument" -> _]], {_}] :=
  True

isTagActive[InspectionObject[tag1_, _, _, _], {tag2_, _}] :=
  !(tag1 === tag2)

isTagActive[InspectionObject[tag1_, _, _, _], {tag2_}] :=
  !(tag1 === tag2)


End[]

EndPackage[]
