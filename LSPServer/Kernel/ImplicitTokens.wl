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

    implicitTokens = CodeInspectImplicitTokensAgg[agg, "AllowedImplicitTokens" -> $AllowedImplicitTokens];

    If[$Debug2,
      log["after CodeInspectImplicitTokensAgg"];
      log["implicitTokens (up to 20): ", Replace[Take[implicitTokens, UpTo[20]], {
          (*
          Do not print the internals
          *)
          InfixNode[Times | Comma | CompoundExpression, _, _] :> InfixNode[Times, "\[Ellipsis]", "\[Ellipsis]"],
          BinaryNode[Span, _, _] :> BinaryNode[Span, "\[Ellipsis]", "\[Ellipsis]"],
          TernaryNode[Span, _, _] :> TernaryNode[Span, "\[Ellipsis]", "\[Ellipsis]"],
          ErrorNode[Token`Error`ExpectedOperand, _, _] :> ErrorNode[Token`Error`ExpectedOperand, "\[Ellipsis]", "\[Ellipsis]"]
        }, {1}
      ]]
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
  Module[{params, doc, uri, entry, inspectedFileObj, tokens},

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
                               "tokens" -> {} |> |>}]
    ];

    inspectedFileObj = Lookup[entry, "InspectedFileObject", Null];

    (*
    Possibly cleared
    *)
    If[inspectedFileObj === Null,
      Throw[{<| "jsonrpc" -> "2.0",
                "method" -> "textDocument/publishImplicitTokens",
                "params" -> <| "uri" -> uri,
                               "tokens" -> {} |> |>}]
    ];

    (*
    Might get something like FileTooLarge or NotCodeStructuralSyntaxAggQ
    Still want to update
    *)
    If[FailureQ[inspectedFileObj],
      Throw[{<| "jsonrpc" -> "2.0",
                "method" -> "textDocument/publishImplicitTokens",
                "params" -> <| "uri" -> uri,
                               "tokens" -> {} |> |>}]
    ];

    (*
    Even though we have:
    Format[LintTimesCharacter, StandardForm] := "\[Times]"

    we cannot evaluate Format[LintTimesCharacter, StandardForm] to get "\[Times]"
    *)

    tokens = Flatten[
      Function[{ignored1, line, content1(*, ignore rest of args*)},
        MapIndexed[
          Function[{sym, columnList},
            <| "character" -> markupSymbolToChar[sym], "line" -> line, "column" -> columnList[[1]] |>]
          ,
          content1[[2, 2;;]]
        ]
      ] @@@ inspectedFileObj[[2]]
    ];

    tokens = DeleteCases[tokens, KeyValuePattern["character" -> " "]];

    {<| "jsonrpc" -> "2.0",
        "method" -> "textDocument/publishImplicitTokens",
        "params" -> <| "uri" -> uri,
                       "tokens" -> tokens |> |>}
  ]]


(*
convert characters to the little markup language described in notes.md

Openers and closers are dropped here
*)
markupSymbolToChar[LintMarkup[LintAllCharacter, ___]] := "A"
markupSymbolToChar[LintMarkup[LintNullCharacter, ___]] := "N"
markupSymbolToChar[LintMarkup[LintOneCharacter, ___]] := "1"
markupSymbolToChar[LintMarkup[LintTimesCharacter, ___]] := "x"
markupSymbolToChar[LintMarkup[LintSpaceTimesCharacter, ___]] := "z"
markupSymbolToChar[LintMarkup[LintExpectedOperandCharacter, ___]] := "e"

markupSymbolToChar[LintMarkup[LintAllCloseCharacter, ___]] := "A"
markupSymbolToChar[LintMarkup[LintAllTimesCharacter, ___]] := "B"
markupSymbolToChar[LintMarkup[LintAllOneCharacter, ___]] := "D"

markupSymbolToChar[LintMarkup[LintCloseCloseCharacter, ___]] := " "
markupSymbolToChar[LintMarkup[LintCloseTimesCharacter, ___]] := "x"
markupSymbolToChar[LintMarkup[LintOpenOneCharacter, ___]] := "1"
markupSymbolToChar[LintMarkup[LintOpenOpenCharacter, ___]] := " "
markupSymbolToChar[LintMarkup[LintTimesOneCharacter, ___]] := "y"
markupSymbolToChar[LintMarkup[LintExpectedOperandTimesCharacter, ___]] := "f"
markupSymbolToChar[LintMarkup[LintExpectedOperandCloseCharacter, ___]] := "e"
markupSymbolToChar[LintMarkup[LintOpenExpectedOperandCharacter, ___]] := "e"
markupSymbolToChar[LintMarkup[LintAllTimesOneCharacter, ___]] := "C"
markupSymbolToChar[LintMarkup[LintCloseTimesOneCharacter, ___]] := "y"

markupSymbolToChar[LintMarkup[LintUnhandledCharacter, ___]] := " "

markupSymbolToChar[LintMarkup["(", ___]] := " "
markupSymbolToChar[LintMarkup[")", ___]] := " "
markupSymbolToChar[" "] := " "



End[]

EndPackage[]
