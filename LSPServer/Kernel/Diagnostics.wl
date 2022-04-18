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
  Module[{params, doc, uri},

    If[$Debug2,
      log["textDocument/runDiagnostics: enter expand"]
    ];
    
    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{}]
    ];

    <| "method" -> #, "params" -> params |>& /@ {
      "textDocument/concreteParse",
      "textDocument/suppressedRegions",
      "textDocument/runConcreteDiagnostics",
      "textDocument/aggregateParse",
      "textDocument/runAggregateDiagnostics",
      "textDocument/abstractParse",
      "textDocument/runAbstractDiagnostics",
      "textDocument/runScopingData", (* implemented in SemanticTokens.wl *)
      "textDocument/runScopingDiagnostics"
    }
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/suppressedRegions"]] :=
  Catch[
  Module[{params, doc, uri, entry, cst, suppressedRegions},

    If[$Debug2,
      log["textDocument/suppressedRegions: enter"]
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

    suppressedRegions = Lookup[entry, "SuppressedRegions", Null];

    If[suppressedRegions =!= Null,
      Throw[{}]
    ];
    
    cst = entry["CST"];

    If[$Debug2,
      log["before SuppressedRegions"];
    ];

    suppressedRegions = SuppressedRegions[cst];

    If[$Debug2,
      log["after SuppressedRegions"]
    ];

    If[$Debug2,
      log["suppressedRegions: ", suppressedRegions]
    ];
    
    entry["SuppressedRegions"] = suppressedRegions;

    $OpenFilesMap[uri] = entry;

    {}
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runConcreteDiagnostics"]] :=
  Catch[
  Module[{params, doc, uri, entry, cst, cstLints, suppressedRegions},

    If[$Debug2,
      log["textDocument/runConcreteDiagnostics: enter"]
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

    cstLints = Lookup[entry, "CSTLints", Null];

    If[cstLints =!= Null,
      Throw[{}]
    ];
    
    cst = entry["CST"];

    suppressedRegions = entry["SuppressedRegions"];

    If[$Debug2,
      log["before CodeInspectCST"]
    ];

    cstLints = CodeInspectCST[cst, "AggregateRules" -> <||>, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

    If[$Debug2,
      log["after CodeInspectCST"]
    ];

    If[$Debug2,
      log["cstLints: ", #["Tag"]& /@ cstLints]
    ];

    entry["CSTLints"] = cstLints;

    $OpenFilesMap[uri] = entry;

    {}
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAggregateDiagnostics"]] :=
  Catch[
  Module[{params, doc, uri, entry, agg, aggLints, suppressedRegions},

    If[$Debug2,
      log["textDocument/runAggregateDiagnostics: enter"]
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

    aggLints = Lookup[entry, "AggLints", Null];

    If[aggLints =!= Null,
      Throw[{}]
    ];

    agg = entry["Agg"];

    suppressedRegions = entry["SuppressedRegions"];

    If[$Debug2,
      log["before CodeInspectAgg"]
    ];

    aggLints = CodeInspectAgg[agg, "AbstractRules" -> <||>, "SuppressedRegions" -> suppressedRegions];

    If[$Debug2,
      log["after CodeInspectAgg"]
    ];

    If[$Debug2,
      log["aggLints: ", #["Tag"]& /@ aggLints]
    ];

    entry["AggLints"] = aggLints;

    $OpenFilesMap[uri] = entry;

    {}
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runAbstractDiagnostics"]] :=
  Catch[
  Module[{params, doc, uri, entry, ast, astLints, suppressedRegions},

    If[$Debug2,
      log["textDocument/runAbstractDiagnostics: enter"]
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

    astLints = Lookup[entry, "ASTLints", Null];

    If[astLints =!= Null,
      Throw[{}]
    ];

    ast = entry["AST"];

    If[FailureQ[ast],
      Throw[{}]
    ];

    suppressedRegions = entry["SuppressedRegions"];

    If[$Debug2,
      log["before CodeInspectAST"]
    ];

    astLints = CodeInspectAST[ast, "SuppressedRegions" -> suppressedRegions];

    If[$Debug2,
      log["after CodeInspectAST"]
    ];

    If[$Debug2,
      log["astLints: ", #["Tag"]& /@ astLints]
    ];

    entry["ASTLints"] = astLints;

    $OpenFilesMap[uri] = entry;

    {}
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runScopingDiagnostics"]] :=
  Catch[
  Module[{params, doc, uri, entry, scopingLints, scopingData, filtered, suppressedRegions, isActive},

    If[$Debug2,
      log["textDocument/runScopingDiagnostics: enter"]
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

    scopingLints = Lookup[entry, "ScopingLints", Null];

    If[scopingLints =!= Null,
      Throw[{}]
    ];

    scopingData = entry["ScopingData"];

    If[$Debug2,
      log["scopingData (up to 20): ", Take[scopingData, UpTo[20]]]
    ];

    (*
    Filter those that have non-empty modifiers
    *)
    filtered = Cases[scopingData, scopingDataObject[_, _, {_, ___}, _]];

    scopingLints = scopingDataObjectToLints /@ filtered;

    scopingLints = Flatten[scopingLints];

    (*
    Filter out suppressed
    *)
    suppressedRegions = entry["SuppressedRegions"];

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

    If[$Debug2,
      log["scopingLints: ", #["Tag"]& /@ scopingLints]
    ];

    entry["ScopingLints"] = scopingLints;

    $OpenFilesMap[uri] = entry;

    {}
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/clearDiagnostics"]] :=
  Catch[
  Module[{params, doc, uri, entry},

    If[$Debug2,
      log["textDocument/clearDiagnostics: enter"]
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

    entry["CSTLints"] =.;

    entry["AggLints"] =.;
    
    entry["ASTLints"] =.;

    entry["ScopingLints"] =.;

    $OpenFilesMap[uri] = entry;

    {}
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/publishDiagnostics"]] :=
  Catch[
  Module[{params, doc, uri, entry, lints, lintsWithConfidence, cstLints, aggLints, astLints, scopingLints, diagnostics},

    If[$Debug2,
      log["textDocument/publishDiagnostics: enter"]
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

    If[$Debug2,
      log["lints: ", #["Tag"]& /@ lints]
    ];


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

    If[$Debug2,
      log["lints: ", #["Tag"]& /@ lints]
    ];

    diagnostics = lintToDiagnostics /@ lints;

    diagnostics = Flatten[diagnostics];

    {<| "jsonrpc" -> "2.0",
        "method" -> "textDocument/publishDiagnostics",
        "params" -> <| "uri" -> uri,
                       "diagnostics" -> diagnostics |> |>}
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
