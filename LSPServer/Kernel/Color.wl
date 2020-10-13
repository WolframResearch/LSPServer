BeginPackage["LSPServer`Color`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/documentColor"], pos_] :=
  Catch[
  Module[{params, id, doc, uri},

    If[$Debug2,
      log["textDocument/documentColor: enter expand"]
    ];

    id = content["id"];

    If[Lookup[$CancelMap, id, False],

      $CancelMap[id] =.;

      If[$Debug2,
        log["canceled"]
      ];
      
      Throw[{<| "method" -> "textDocument/documentColorFencepost", "id" -> id, "params" -> params |>}]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/documentColorFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
       "textDocument/concreteParse",
       "textDocument/aggregateParse",
       "textDocument/abstractParse",
       "textDocument/documentColorFencepost"
    }
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/documentColorFencepost"]] :=
Catch[
Module[{id, params, doc, uri, colorInformations, ast, colorNodes, entry},

  If[$Debug2,
    log["textDocument/documentColor: enter"]
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

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  entry = $OpenFilesMap[uri];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[ast]
  ];

  colorNodes = Cases[ast, CallNode[LeafNode[Symbol, "RGBColor" | "Hue", _], _, _], Infinity];

  colorInformations = colorNodeToColorInformation /@ colorNodes;

  colorInformations = DeleteCases[colorInformations, Null];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> colorInformations |>}
]]


colorNodeToColorInformation[CallNode[LeafNode[Symbol, "RGBColor", _], {
  r:LeafNode[Integer|Real, _, _],
  g:LeafNode[Integer|Real, _, _],
  b:LeafNode[Integer|Real, _, _],
  a:LeafNode[Integer|Real, _, _]:LeafNode[Integer, "1", <||>]}, data_]] :=
Module[{rVal, gVal, bVal, aVal, src},

  rVal = FromNode[r];
  gVal = FromNode[g];
  bVal = FromNode[b];
  aVal = FromNode[a];

  src = data[Source];

  src-=1;

  <| "range" -> <| "start" -> <| "line" -> src[[1, 1]], "character" -> src[[1, 2]] |>,
                   "end" -> <| "line" -> src[[2, 1]], "character" -> src[[2, 2]] |> |>,
     "color" -> <| "red" -> rVal,
                   "green" -> gVal,
                   "blue" -> bVal,
                   "alpha" -> aVal |> |>
]


colorNodeToColorInformation[CallNode[LeafNode[Symbol, "Hue", _], {h:LeafNode[Integer|Real, _, _]}, data_]] :=
  colorNodeToColorInformation[CallNode[LeafNode[Symbol, "Hue", <||>], {h, LeafNode[Integer, "1", <||>], LeafNode[Integer, "1", <||>]}, data]]



colorNodeToColorInformation[CallNode[LeafNode[Symbol, "Hue", _], {
  h:LeafNode[Integer|Real, _, _],
  s:LeafNode[Integer|Real, _, _],
  b:LeafNode[Integer|Real, _, _],
  a:LeafNode[Integer|Real, _, _]:LeafNode[Integer, "1", <||>]}, data_]] :=
Module[{hVal, sVal, bVal, aVal, src, rgba},

  hVal = FromNode[h];
  sVal = FromNode[s];
  bVal = FromNode[b];
  aVal = FromNode[a];

  rgba = ColorConvert[Hue[hVal, sVal, bVal, aVal], "RGB"];

  src = data[Source];

  src-=1;

  <| "range" -> <| "start" -> <| "line" -> src[[1, 1]], "character" -> src[[1, 2]] |>,
                   "end" -> <| "line" -> src[[2, 1]], "character" -> src[[2, 2]] |> |>,
     "color" -> <| "red" -> rgba[[1]],
                   "green" -> rgba[[2]],
                   "blue" -> rgba[[3]],
                   "alpha" -> rgba[[4]] |> |>
]


colorNodeToColorInformation[_] :=
  Null


End[]

EndPackage[]
