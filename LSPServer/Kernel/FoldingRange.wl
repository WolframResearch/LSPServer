BeginPackage["LSPServer`FoldingRange`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`DocumentSymbol`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


$FoldingRangeKind = <|
  (*
  pre-defined range kinds
  *)
	"Comment" -> "comment",
	"Imports" -> "imports",
	"Region" -> "region",
  (*
  custom range kinds
  *)
  "Function" -> "function"
|>


expandContent[content:KeyValuePattern["method" -> "textDocument/foldingRange"], pos_] :=
Catch[
Module[{params, id, doc, uri},

  If[$Debug2,
    log["textDocument/foldingRange: enter expand"]
  ];

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "method" -> "textDocument/foldingRangeFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/foldingRangeFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/documentNodeList",
    "textDocument/foldingRangeFencepost"
  }
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/foldingRangeFencepost"]] :=
Catch[
Module[{id, params, doc, uri, cst, entry, foldingRange,
  flatBag, comments, sorted, toInsert, completed, nodeList},

  If[$Debug2,
    log["textDocument/foldingRangeFencepost: enter"]
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
    
  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  nodeList = Lookup[entry, "NodeList", Null];

  If[nodeList === Null,
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  foldingRange = Flatten[walkOutline /@ nodeList];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> foldingRange |>}
]]



walkOutline[packageCommentNode[_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[sectionCommentNode[name_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[subsectionCommentNode[name_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[subsubsectionCommentNode[name_, children_, data_]] :=
Catch[
Module[{walkedChildren, src},

  walkedChildren = Flatten[walkOutline /@ children];

  src = data[Source];
  src--;
  
  If[walkedChildren == {},
    Throw[{<|
      "kind" -> $FoldingRangeKind["Region"],
      "startLine" -> src[[1, 1]],
      "endLine" -> src[[2, 1]]
    |>}]
  ];

  {<|
    "kind" -> $FoldingRangeKind["Region"],
    "startLine" -> src[[1, 1]],
    "endLine" -> walkedChildren[[-1, Key["endLine"]]]
  |>, walkedChildren}
]]

walkOutline[functionDefinitionNode[name_, _, data_]] :=
Catch[
Module[{src},

  src = data[Source];
  src--;
  
  <|
    "kind" -> $FoldingRangeKind["Function"],
    "startLine" -> src[[1, 1]],
    "endLine" -> src[[2, 1]]
  |>
]]

walkOutline[constantDefinitionNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[propertyDefinitionNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[titleComment[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[beginNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[endNode[Null, _, data_]] :=
Module[{},
  {}
]

walkOutline[beginPackageNode[name_, _, data_]] :=
Module[{},
  {}
]

walkOutline[endPackageNode[Null, _, data_]] :=
Module[{},
  {}
]

walkOutline[beginNewContextPathNode[ctxts_, _, data_]] :=
Module[{},
  {}
]

walkOutline[endNewContextPathNode[Null, _, data_]] :=
Module[{},
  {}
]

End[]

EndPackage[]
