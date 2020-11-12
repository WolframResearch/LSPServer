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

  colorNodes = Cases[ast,
    CallNode[LeafNode[Symbol, "RGBColor" | "Hue" | "GrayLevel", _], _, _] |
    LeafNode[Symbol, "Red" | "Green" | "Blue" | "Black" | "White" | "Gray" | "Cyan" | "Magenta" | "Yellow" | "Brown" | "Orange" | "Pink" | "Purple" |
      "LightRed" | "LightGreen" | "LightBlue" | "LightGray" | "LightCyan" | "LightMagenta" | "LightYellow" | "LightBrown" | "LightOrange" | "LightPink" | "LightPurple" |
      "Transparent", _], Infinity];

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

colorNodeToColorInformation[CallNode[LeafNode[Symbol, "RGBColor", _], {s:LeafNode[String, _, _]}, data_]] :=
Catch[
Module[{sVal, src, cases, rVal, gVal, bVal, aVal, case},

  sVal = FromNode[s];

  cases = StringCases[sVal, RegularExpression["#([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{2})"] :> {"$1", "$2", "$3"}];

  If[cases == {},
    Throw[Null]
  ];

  case = cases[[1]];

  rVal = FromDigits[case[[1]], 16]/255.0;
  gVal = FromDigits[case[[2]], 16]/255.0;
  bVal = FromDigits[case[[3]], 16]/255.0;
  aVal = 1;

  src = data[Source];

  src-=1;

  <| "range" -> <| "start" -> <| "line" -> src[[1, 1]], "character" -> src[[1, 2]] |>,
                   "end" -> <| "line" -> src[[2, 1]], "character" -> src[[2, 2]] |> |>,
     "color" -> <| "red" -> rVal,
                   "green" -> gVal,
                   "blue" -> bVal,
                   "alpha" -> aVal |> |>
]]


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


colorNodeToColorInformation[CallNode[LeafNode[Symbol, "GrayLevel", _], {
  g:LeafNode[Integer|Real, _, _],
  a:LeafNode[Integer|Real, _, _]:LeafNode[Integer, "1", <||>]}, data_]] :=
Module[{gVal, aVal, src},

  gVal = FromNode[g];
  aVal = FromNode[a];

  src = data[Source];

  src-=1;

  <| "range" -> <| "start" -> <| "line" -> src[[1, 1]], "character" -> src[[1, 2]] |>,
                   "end" -> <| "line" -> src[[2, 1]], "character" -> src[[2, 2]] |> |>,
     "color" -> <| "red" -> gVal,
                   "green" -> gVal,
                   "blue" -> gVal,
                   "alpha" -> aVal |> |>
]


colorNodeToColorInformation[n:LeafNode[Symbol, "Red" | "Green" | "Blue" | "Black" | "White" | "Gray" | "Cyan" | "Magenta" | "Yellow" | "Brown" | "Orange" | "Pink" | "Purple" |
      "LightRed" | "LightGreen" | "LightBlue" | "LightGray" | "LightCyan" | "LightMagenta" | "LightYellow" | "LightBrown" | "LightOrange" | "LightPink" | "LightPurple" |
      "Transparent", data_]] :=
Module[{},

  colorNodeToColorInformation[
    FromNode[n] /. {
      RGBColor[r_, g_, b_] :> CallNode[LeafNode[Symbol, "RGBColor", <||>], {ToNode[r], ToNode[g], ToNode[b]}, data],
      RGBColor[r_, g_, b_, a_] :> CallNode[LeafNode[Symbol, "RGBColor", <||>], {ToNode[r], ToNode[g], ToNode[b], ToNode[a]}, data],
      GrayLevel[g_] :> CallNode[LeafNode[Symbol, "GrayLevel", <||>], {ToNode[g]}, data],
      GrayLevel[g_, a_] :> CallNode[LeafNode[Symbol, "GrayLevel", <||>], {ToNode[g], ToNode[a]}, data]
    }
  ]
]


colorNodeToColorInformation[_] :=
  Null



handleContent[content:KeyValuePattern["method" -> "textDocument/colorPresentation"]] :=
Catch[
Module[{id, params, doc, uri, color, range, rVal, gVal, bVal, aVal, label},
  
  If[$Debug2,
    log["textDocument/documentColorPresentation: enter"]
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
  color = params["color"];
  doc = params["textDocument"];
  uri = doc["uri"];
  range = params["range"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  rVal = color["red"];
  gVal = color["green"];
  bVal = color["blue"];
  aVal = color["alpha"];

  If[aVal == 1.0,
    label = ToString[RGBColor[rVal, gVal, bVal]];
    ,
    label = ToString[RGBColor[rVal, gVal, bVal, aVal]];
  ];

  colorPresentations = { <| "label" -> label |> };

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> colorPresentations |>}
]]

End[]

EndPackage[]
