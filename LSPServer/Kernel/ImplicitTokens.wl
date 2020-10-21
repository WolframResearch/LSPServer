BeginPackage["LSPServer`ImplicitTokens`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`ImplicitTokens`"]
Needs["CodeParser`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/runImplicitTokens"], pos_] :=
  Catch[
  Module[{params, doc, uri},

    If[$Debug2,
      log["textDocument/runImplicitTokens: enter expand"]
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

    params = content["params"];

    <| "method" -> #, "params" -> params |>& /@ {
       "textDocument/concreteParse",
       "textDocument/aggregateParse",
       "textDocument/runImplicitTokensFencepost"
    }
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runImplicitTokensFencepost"]] :=
  Catch[
  Module[{params, doc, uri, entry, cst, inspectedFileObj, implicitTokens, agg},

    If[$Debug2,
      log["textDocument/runImplicitTokensFencepost: enter"]
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

    inspectedFileObj = Lookup[entry, "InspectedFileObject", Null];

    If[inspectedFileObj =!= Null,
      Throw[{}]
    ];

    cst = entry["CST"];

    agg = entry["Agg"];

    If[$Debug2,
      log["before CodeStructuralSyntaxAggQ"]
    ];

    If[!CodeStructuralSyntaxAggQ[agg],

      entry["InspectedFileObject"] = Failure["NotCodeStructuralSyntaxAggQ", <| "URI" -> uri |>];

      $OpenFilesMap[uri] = entry;

      Throw[{}]
    ];

    If[$Debug2,
      log["after CodeStructuralSyntaxAggQ"]
    ];

    If[$Debug2,
      log["before CodeInspectImplicitTokensAgg"]
    ];

    implicitTokens = CodeInspectImplicitTokensAgg[agg];

    If[$Debug2,
      log["after CodeInspectImplicitTokensAgg"];
      log["implicitTokens (up to 20): ", Take[implicitTokens, UpTo[20]]]
    ];

    If[$Debug2,
      log["before CodeInspectImplicitTokensCSTSummarize"]
    ];

    inspectedFileObj = CodeInspectImplicitTokensCSTSummarize[cst, implicitTokens];

    If[$Debug2,
      log["after CodeInspectImplicitTokensCSTSummarize"]
    ];

    entry["InspectedFileObject"] = inspectedFileObj;

    $OpenFilesMap[uri] = entry;

    {}
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/clearImplicitTokens"]] :=
  Catch[
  Module[{params, doc, uri, entry},

    If[$Debug2,
      log["textDocument/clearImplicitTokens: enter"]
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

    entry["InspectedFileObject"] =.;

    $OpenFilesMap[uri] = entry;

    {}
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/publishImplicitTokens"]] :=
  Catch[
  Module[{params, doc, uri, entry, inspectedFileObj, lines},

    If[$Debug2,
      log["textDocument/publishImplicitTokens: enter"]
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
                "method" -> "textDocument/publishImplicitTokens",
                "params" -> <| "uri" -> uri,
                               "lines" -> {} |> |>}]
    ];

    inspectedFileObj = Lookup[entry, "InspectedFileObject", Null];

    (*
    Possibly cleared
    *)
    If[inspectedFileObj === Null,
      Throw[{<| "jsonrpc" -> "2.0",
                "method" -> "textDocument/publishImplicitTokens",
                "params" -> <| "uri" -> uri,
                               "lines" -> {} |> |>}]
    ];

    (*
    Might get something like FileTooLarge or NotCodeStructuralSyntaxAggQ
    Still want to update
    *)
    If[FailureQ[inspectedFileObj],
      Throw[{<| "jsonrpc" -> "2.0",
                "method" -> "textDocument/publishImplicitTokens",
                "params" -> <| "uri" -> uri,
                               "lines" -> {} |> |>}]
    ];

    (*
    Even though we have:
    Format[LintTimesCharacter, StandardForm] := "\[Times]"

    we cannot evaluate Format[LintTimesCharacter, StandardForm] to get "\[Times]"
    *)
    
    lines = <|
      "line" -> #[[2]],
      "characters" -> ((# /. {
        (*
        convert characters to the little markup language described in notes.md

        Openers and closers are dropped here
        *)
        LintAllCharacter -> "A",
        LintNullCharacter -> "N",
        LintOneCharacter -> "1",
        LintTimesCharacter -> "x",
        LintExpectedOperandCharacter -> "e",
        LintAllCloseCharacter -> "A",
        LintAllTimesCharacter -> "B",
        LintCloseCloseCharacter -> " ",
        LintCloseTimesCharacter -> "x",
        LintOpenOneCharacter -> "1",
        LintOpenOpenCharacter -> " ",
        LintTimesOneCharacter -> "y",
        LintExpectedOperandTimesCharacter -> "f",
        LintExpectedOperandCloseCharacter -> "e",
        LintOpenExpectedOperandCharacter -> "e",
        LintAllTimesOneCharacter -> "C",
        LintCloseTimesOneCharacter -> "y"
      })& /@ ((# /. LintMarkup[content1_, ___] :> content1)& /@ #[[3, 2, 2;;]]))
    |>& /@ inspectedFileObj[[2]];

    {<| "jsonrpc" -> "2.0",
        "method" -> "textDocument/publishImplicitTokens",
        "params" -> <| "uri" -> uri,
                       "lines" -> lines |> |>}
  ]]


End[]

EndPackage[]
