BeginPackage["LSPServer`DocumentSymbol`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


$SymbolKind = <|
  "File" -> 1,
  "Module" -> 2,
  "Namespace" -> 3,
  "Package" -> 4,
  "Class" -> 5,
  "Method" -> 6,
  "Property" -> 7,
  "Field" -> 8,
  "Constructor" -> 9,
  "Enum" -> 10,
  "Interface" -> 11,
  "Function" -> 12,
  "Variable" -> 13,
  "Constant" -> 14,
  "String" -> 15,
  "Number" -> 16,
  "Boolean" -> 17,
  "Array" -> 18,
  "Object" -> 19,
  "Key" -> 20,
  "Null" -> 21,
  "EnumMember" -> 22,
  "Struct" -> 23,
  "Event" -> 24,
  "Operator" -> 25,
  "TypeParameter" -> 26
|>

expandContent[content:KeyValuePattern["method" -> "textDocument/documentSymbol"], pos_] :=
  Catch[
  Module[{params, id, doc, uri},

    If[$Debug2,
      log["textDocument/documentColor: enter expand"]
    ];

    id = content["id"];
    params = content["params"];
    
    If[Lookup[$CancelMap, id, False],

      $CancelMap[id] =.;

      If[$Debug2,
        log["canceled"]
      ];
      
      Throw[{<| "method" -> "textDocument/documentSymbolFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
    ];

    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/documentSymbolFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
      "textDocument/concreteParse",
      "textDocument/aggregateParse",
      "textDocument/abstractParse",
      "textDocument/documentSymbolFencepost"
    }
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/documentSymbolFencepost"]] :=
Catch[
Module[{id, params, doc, uri, cst, ast, entry, symbolInfo, documentSymbols,
  flatBag, comments, sorted, toInsert, completed, nodeList},

  If[$Debug2,
    log["textDocument/documentSymbolFencepost: enter"]
  ];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
    
  entry = $OpenFilesMap[uri];

  cst = entry["CST"];

  If[FailureQ[cst],
    Throw[cst]
  ];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[ast]
  ];

  (*
  populate flatBag
  *)
  comments = Cases[cst[[2]], LeafNode[Token`Comment, _, _]];

  If[$Debug2,
    log["comments: ", comments]
  ];

  Block[{$FlatBag},
    $FlatBag = Internal`Bag[];
    walkCommentPair /@ Partition[comments, 2, 1];
    walkAST /@ ast[[2]];
    flatBag = $FlatBag;
  ];

  If[$Debug2,
    log["flatBag: ", flatBag]
  ];

  (*
  sort
  *)
  sorted = SortBy[Internal`BagPart[flatBag, All], #[[3, Key[Source]]] &];

  If[$Debug2,
    log["sorted: ", sorted]
  ];

  toInsert = createToInsert[sorted];

  completed = Fold[Insert[#1, #2[[1]], #2[[2]]] &, sorted, ReverseSortBy[Normal[Internal`BagPart[toInsert, All]], #[[2;;3]]&]];

  If[$Debug2,
    log["completed: ", completed]
  ];

  nodeList = createNodeList[completed];

  If[$Debug2,
    log["nodeList: ", nodeList]
  ];

  documentSymbols = Flatten[walkOutline /@ nodeList];

  If[$Debug2,
    log["documentSymbols: ", documentSymbols]
  ];

  If[$HierarchicalDocumentSymbolSupport,
    {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> documentSymbols |>}
    ,

    symbolInfo = Flatten[flattenDocumentSymbolToSymbolInfo /@ documentSymbols];

    {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> symbolInfo |>}
  ]
]]


abstractContextString[str_String /; StringStartsQ[str, "\""]] :=
	Quiet[ToExpression[str], {Syntax::stresc, Syntax::snthex, Syntax::sntoct1, Syntax::sntoct2, Syntax::snthex32}]


flattenDocumentSymbolToSymbolInfo[documentSymbol:KeyValuePattern["children" -> children_]] :=
  Flatten[{KeyDrop[documentSymbol, "children"], flattenDocumentSymbolToSymbolInfo /@ children}]

flattenDocumentSymbolToSymbolInfo[documentSymbol_] :=
  documentSymbol




walkCommentPair[{LeafNode[Token`Comment, "(* ::Package:: *)", data_], _}] :=
  Internal`StuffBag[$FlatBag, packageComment[Null, "", KeyTake[data, {Source}]]]

walkCommentPair[{LeafNode[Token`Comment, "(* ::Title:: *)", _], LeafNode[Token`Comment, com_, data_]}] := 
  Internal`StuffBag[$FlatBag, titleComment[StringTrim[StringTake[com, {3, -3}]], "", KeyTake[data, {Source}]]]

walkCommentPair[{LeafNode[Token`Comment, "(* ::Section:: *)", _], LeafNode[Token`Comment, com_, data_]}] :=
  Internal`StuffBag[$FlatBag, sectionComment[StringTrim[StringTake[com, {3, -3}]], "", KeyTake[data, {Source}]]]

walkCommentPair[{LeafNode[Token`Comment, "(* ::Section::Closed:: *)", _], LeafNode[Token`Comment, com_, data_]}] :=
  Internal`StuffBag[$FlatBag, sectionComment[StringTrim[StringTake[com, {3, -3}]], "", KeyTake[data, {Source}]]]

walkCommentPair[{LeafNode[Token`Comment, "(* ::Subsection:: *)", _], LeafNode[Token`Comment, com_, data_]}] :=
  Internal`StuffBag[$FlatBag, subsectionComment[StringTrim[StringTake[com, {3, -3}]], "", KeyTake[data, {Source}]]]

walkCommentPair[{LeafNode[Token`Comment, "(* ::Subsection::Closed:: *)", _], LeafNode[Token`Comment, com_, data_]}] :=
  Internal`StuffBag[$FlatBag, subsectionComment[StringTrim[StringTake[com, {3, -3}]], "", KeyTake[data, {Source}]]]

walkCommentPair[{LeafNode[Token`Comment, "(* ::Subsubsection:: *)", _], LeafNode[Token`Comment, com_, data_]}] :=
  Internal`StuffBag[$FlatBag, subsubsectionComment[StringTrim[StringTake[com, {3, -3}]], "", KeyTake[data, {Source}]]]

walkCommentPair[{LeafNode[Token`Comment, "(* ::Subsubsection::Closed:: *)", _], LeafNode[Token`Comment, com_, data_]}] :=
  Internal`StuffBag[$FlatBag, subsubsectionComment[StringTrim[StringTake[com, {3, -3}]], "", KeyTake[data, {Source}]]]


walkAST[PackageNode[ctxts_, children_, data_]] :=
Catch[
Module[{src},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  src = data[Source];
  Internal`StuffBag[$FlatBag, beginPackageNode[abstractContextString[ctxts[[1, 2]]], "", <|Source -> {src[[1]], src[[1]]}|>]];
  walkAST /@ children;
  Internal`StuffBag[$FlatBag, endPackageNode[Null, "", <| Source -> {src[[2]], src[[2]]} |>]];
]]

walkAST[ContextNode[ctxts_, children_, data_]] :=
Catch[
Module[{src},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  src = data[Source];
  Internal`StuffBag[$FlatBag, beginNode[abstractContextString[ctxts[[1, 2]]], "", <|Source -> {src[[1]], src[[1]]}|>]];
  walkAST /@ children;
  Internal`StuffBag[$FlatBag, endNode[Null, "", <| Source -> {src[[2]], src[[2]]} |>]];
]]

walkAST[NewContextPathNode[ctxts_, children_, data_]] :=
Catch[
Module[{src},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  src = data[Source];
  Internal`StuffBag[$FlatBag, beginNewContextPathNode[abstractContextString[#[[2]]]& /@ ctxts, "", <|Source -> {src[[1]], src[[1]]}|>]];
  walkAST /@ children;
  Internal`StuffBag[$FlatBag, endNewContextPathNode[Null, "", <| Source -> {src[[2]], src[[2]]} |>]];
]]

walkAST[CallNode[LeafNode[Symbol, "SetDelayed", _], _, data:KeyValuePattern["Definitions" -> defs_]]] :=
Catch[
Module[{},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  walkFunctionDef /@ defs;
]]

walkAST[CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "SetDelayed", _], _, data:KeyValuePattern["Definitions" -> defs_]], _}, _]] :=
Catch[
Module[{},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  walkFunctionDef /@ defs;
]]

walkAST[CallNode[LeafNode[Symbol, "Set", _], _, data:KeyValuePattern["Definitions" -> defs_]]] :=
Catch[
Module[{},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  walkConstantDef /@ defs;
]]

walkAST[CallNode[LeafNode[Symbol, "CompoundExpression", _], {CallNode[LeafNode[Symbol, "Set", _], _, data:KeyValuePattern["Definitions" -> defs_]], _}, _]] :=
Catch[
Module[{},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  walkConstantDef /@ defs;
]]

walkFunctionDef[LeafNode[Symbol, str_, data_]] :=
Catch[
Module[{},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  Internal`StuffBag[$FlatBag, functionDefinitionNode[str, "", KeyTake[data, {Source}]]]
]]

walkConstantDef[LeafNode[Symbol, str_, data_]] :=
Catch[
Module[{},
  If[!KeyExistsQ[data, Source],
    Throw[Null]
  ];
  Internal`StuffBag[$FlatBag, constantDefinitionNode[str, "", KeyTake[data, {Source}]]]
]]



createToInsert[sorted_] :=
Module[{operatorStack, toInsert, x, peek},
  operatorStack = System`CreateDataStructure["Stack"];
  operatorStack["Push", None];
  toInsert = Internal`Bag[];
  Do[
    x = sorted[[i]];
    Switch[x,
      packageComment[_, _, _],
      operatorStack["Push", x]
      ,
      sectionComment[_, _, _],
      peek = operatorStack["Peek"];
      If[MatchQ[peek, subsubsectionComment[_, _, _]],
        operatorStack["Pop"];
        Internal`StuffBag[toInsert, {endSubsubsectionComment[], i, 1}]
      ];
      peek = operatorStack["Peek"];
      If[MatchQ[peek, subsectionComment[_, _, _]],
        operatorStack["Pop"];
        Internal`StuffBag[toInsert, {endSubsectionComment[], i, 2}]
      ];
      peek = operatorStack["Peek"];
      If[MatchQ[peek, sectionComment[_, _, _]],
        operatorStack["Pop"];
        Internal`StuffBag[toInsert, {endSectionComment[], i, 3}]
      ];
      operatorStack["Push", x]
      ,
      subsectionComment[_, _, _],
      peek = operatorStack["Peek"];
      If[MatchQ[peek, subsubsectionComment[_, _, _]],
        operatorStack["Pop"];
        Internal`StuffBag[toInsert, {endSubsubsectionComment[], i, 1}]
      ];
      peek = operatorStack["Peek"];
      If[MatchQ[peek, subsectionComment[_, _, _]],
        operatorStack["Pop"];
        Internal`StuffBag[toInsert, {endSubsectionComment[], i, 2}]
      ];
      operatorStack["Push", x]
      ,
      subsubsectionComment[_, _, _],
      peek = operatorStack["Peek"];
      If[MatchQ[peek, subsubsectionComment[_, _, _]],
        operatorStack["Pop"];
        Internal`StuffBag[toInsert, {endSubsubsectionComment[], i, 1}]
      ];
      operatorStack["Push", x]
      ,
      _,
      Null
    ]
    ,
    {i, 1, Length[sorted]}
  ];
  peek = operatorStack["Peek"];
  If[MatchQ[peek, subsubsectionComment[_, _, _]],
    operatorStack["Pop"];
    Internal`StuffBag[
    toInsert, {endSubsubsectionComment[], Length[sorted] + 1, 1}]
  ];
  peek = operatorStack["Peek"];
  If[MatchQ[peek, subsectionComment[_, _, _]],
    operatorStack["Pop"];
    Internal`StuffBag[
    toInsert, {endSubsectionComment[], Length[sorted] + 1, 2}]
  ];
  peek = operatorStack["Peek"];
  If[MatchQ[peek, sectionComment[_, _, _]],
    operatorStack["Pop"];
    Internal`StuffBag[
    toInsert, {endSectionComment[], Length[sorted] + 1, 3}]
  ];
  peek = operatorStack["Peek"];
  If[MatchQ[peek, packageComment[_, _, _]],
    operatorStack["Pop"];
    Internal`StuffBag[
    toInsert, {endPackageComment[], Length[sorted] + 1, 4}]
  ];

  toInsert
]





createNodeList[completed_] :=
Module[{nodeListStack, operatorStack, x, currentOperator, currentList, peek, nodeList},
  nodeListStack = System`CreateDataStructure["Stack"];
  operatorStack = System`CreateDataStructure["Stack"];
  nodeListStack["Push", System`CreateDataStructure["Stack"]];
  operatorStack["Push", None];
  Do[
    x = completed[[i]];
    Switch[x,
      packageComment[_, _, _],
      operatorStack["Push", packageCommentNode[x[[1]], {}, x[[3]]]];
      nodeListStack["Push", System`CreateDataStructure["Stack"]];
      ,
      sectionComment[_, _, _],
      operatorStack["Push", sectionCommentNode[x[[1]], {}, x[[3]]]];
      nodeListStack["Push", System`CreateDataStructure["Stack"]];
      ,
      subsectionComment[_, _, _],
      operatorStack["Push", subsectionCommentNode[x[[1]], {}, x[[3]]]];
      nodeListStack["Push", System`CreateDataStructure["Stack"]];
      ,
      subsubsectionComment[_, _, _],
      operatorStack["Push", 
      subsubsectionCommentNode[x[[1]], {}, x[[3]]]];
      nodeListStack["Push", System`CreateDataStructure["Stack"]];
      ,
      endPackageComment[] | endSectionComment[] | endSubsectionComment[] | endSubsubsectionComment[],
      currentOperator = operatorStack["Pop"];
      currentList = nodeListStack["Pop"];
      currentOperator[[2]] = Normal[currentList];
      peek = nodeListStack["Peek"];
      peek["Push", currentOperator];
      ,
      _,
      peek = nodeListStack["Peek"];
      peek["Push", x];
    ]
    ,
    {i, 1, Length[completed]}
  ];

  peek = nodeListStack["Peek"];
  nodeList = Normal[peek];
  nodeList
]






walkOutline[packageCommentNode[_, {}, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "Package",
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[packageCommentNode[_, children_, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "Package",
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> children[[-1, 3, Key[Source], 2, 1]], 
        "character" -> children[[-1, 3, Key[Source], 2, 2]]|>|>, 
    "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>,
    "children" -> Flatten[walkOutline /@ children]
    |>}
]

walkOutline[sectionCommentNode[name_, {}, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[sectionCommentNode[name_, children_, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> children[[-1, 3, Key[Source], 2, 1]], 
        "character" -> children[[-1, 3, Key[Source], 2, 2]]|>|>, 
    "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>,
    "children" -> Flatten[walkOutline /@ children]
    |>}
]

walkOutline[subsectionCommentNode[name_, {}, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[subsectionCommentNode[name_, children_, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> children[[-1, 3, Key[Source], 2, 1]], 
        "character" -> children[[-1, 3, Key[Source], 2, 2]]|>|>, 
    "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>,
    "children" -> Flatten[walkOutline /@ children]
    |>}
]

walkOutline[subsubsectionCommentNode[name_, {}, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[subsubsectionCommentNode[name_, children_, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> children[[-1, 3, Key[Source], 2, 1]], 
        "character" -> children[[-1, 3, Key[Source], 2, 2]]|>|>, 
    "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>,
    "children" -> Flatten[walkOutline /@ children]
    |>}
]

walkOutline[functionDefinitionNode[name_, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["Function"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[constantDefinitionNode[name_, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["Constant"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[titleComment[name_, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> name,
    "kind" -> $SymbolKind["File"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[beginNode[name_, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "Begin[" <> name <> "]",
    "kind" -> $SymbolKind["Namespace"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[endNode[Null, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "End[]",
    "kind" -> $SymbolKind["Namespace"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[beginPackageNode[name_, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "BeginPackage[" <> name <> "]",
    "kind" -> $SymbolKind["Package"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[endPackageNode[Null, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "EndPackage[]",
    "kind" -> $SymbolKind["Package"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[beginNewContextPathNode[ctxts_, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "NewContextPath[" <> ToString[ctxts] <> "]",
    "kind" -> $SymbolKind["Namespace"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

walkOutline[endNewContextPathNode[Null, _, data_]] :=
Module[{src},
  src = data[Source];
  src--;
  {<|
    "name" -> "RestoreContextPath[]",
    "kind" -> $SymbolKind["Namespace"],
    "range" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], "character" -> src[[2, 2]]|>|>,
     "selectionRange" -> <|
      "start" -> <|"line" -> src[[1, 1]], "character" -> src[[1, 2]]|>,
       "end" -> <|"line" -> src[[2, 1]], 
        "character" -> src[[2, 2]]|>|>
    |>}
]

End[]

EndPackage[]
