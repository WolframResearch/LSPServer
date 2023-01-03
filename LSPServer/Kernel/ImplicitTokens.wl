BeginPackage["LSPServer`ImplicitTokens`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeInspector`Format`"]
Needs["CodeInspector`ImplicitTokens`"]
Needs["CodeParser`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/runImplicitTokens"], pos_] :=
Catch[
Module[{params, doc, uri, res},

  log[1, "textDocument/runImplicitTokens: enter expand"];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{}]
  ];

  res = <| "method" -> #, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/runImplicitTokensFencepost"
  };

  log[1, "textDocument/runImplicitTokens: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runImplicitTokensFencepost"]] :=
Catch[
Module[{params, doc, uri, entry, cst, inspectedFileObj, implicitTokens, agg},

  log[1, "textDocument/runImplicitTokensFencepost: enter"];

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

  inspectedFileObj = Lookup[entry, "InspectedFileObject", Null];

  If[inspectedFileObj =!= Null,
    Throw[{}]
  ];

  cst = entry["CST"];

  agg = entry["Agg"];

  log[2, "before CodeStructuralSyntaxAggQ"];

  If[!CodeStructuralSyntaxAggQ[agg],

    entry["InspectedFileObject"] = Failure["NotCodeStructuralSyntaxAggQ", <| "URI" -> uri |>];

    $OpenFilesMap[uri] = entry;

    Throw[{}]
  ];

  log[2, "after CodeStructuralSyntaxAggQ"];

  log[2, "before CodeInspectImplicitTokensAgg"];

  implicitTokens = CodeInspectImplicitTokensAgg[agg, "AllowedImplicitTokens" -> $AllowedImplicitTokens];

  log[2, "after CodeInspectImplicitTokensAgg"];
  log[2, "implicitTokens (up to 20): ", Replace[Take[implicitTokens, UpTo[20]], {
      (*
      Do not print the internals
      *)
      BinaryNode[tag_, _, _] :> BinaryNode[tag, "\[Ellipsis]", "\[Ellipsis]"],
      InfixNode[tag_, _, _] :> InfixNode[tag, "\[Ellipsis]", "\[Ellipsis]"],
      PrefixNode[tag_, _, _] :> PrefixNode[tag, "\[Ellipsis]", "\[Ellipsis]"],
      PostfixNode[tag_, _, _] :> PostfixNode[tag, "\[Ellipsis]", "\[Ellipsis]"],
      TernaryNode[tag_, _, _] :> TernaryNode[tag, "\[Ellipsis]", "\[Ellipsis]"],
      ErrorNode[tag_, _, _] :> ErrorNode[tag, "\[Ellipsis]", "\[Ellipsis]"]
    }, {1}
  ]];

  log[2, "before CodeInspectImplicitTokensCSTSummarize"];

  inspectedFileObj = CodeInspectImplicitTokensCSTSummarize[cst, implicitTokens];

  log[2, "after CodeInspectImplicitTokensCSTSummarize"];

  entry["InspectedFileObject"] = inspectedFileObj;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runImplicitTokensFencepost: exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/clearImplicitTokens"]] :=
Catch[
Module[{params, doc, uri, entry},

  log[1, "textDocument/clearImplicitTokens: enter"];

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
  
  entry["InspectedFileObject"] =.;

  $OpenFilesMap[uri] = entry;

  log["textDocument/clearImplicitTokens: exit"];

  {}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/publishImplicitTokens"]] :=
Catch[
Module[{params, doc, uri, entry, inspectedFileObj, tokens},

  log[1, "textDocument/publishImplicitTokens: enter"];

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

  log[1, "textDocument/publishImplicitTokens: exit"];

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
