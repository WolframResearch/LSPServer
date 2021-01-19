BeginPackage["LSPServer`SemanticTokens`"]

$SemanticTokenTypes

$SemanticTokenModifiers

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


$SemanticTokenTypes = <|
  "variable" -> 0,
  "parameter" -> 1
|>

$SemanticTokenModifiers = <|
  "Module" -> 0,
  "Block" -> 1,
  "shadowed" -> 2
|>


tokenType[{"parameter"..}] := $SemanticTokenTypes["parameter"]
tokenType[_] := $SemanticTokenTypes["variable"]

modifiersBitset[{"parameter"}] = 0
modifiersBitset[{decl_}] := BitShiftLeft[1, $SemanticTokenModifiers[decl]];
modifiersBitset[_] := BitShiftLeft[1, $SemanticTokenModifiers["shadowed"]];


expandContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/full"], pos_] :=
  Catch[
  Module[{params, id, doc, uri},

    If[$Debug2,
      log["textDocument/semanticTokens/full: enter expand"]
    ];

    id = content["id"];
    params = content["params"];
    
    If[Lookup[$CancelMap, id, False],

      $CancelMap[id] =.;

      If[$Debug2,
        log["canceled"]
      ];
      
      Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
    ];

    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
       "textDocument/concreteParse",
       "textDocument/aggregateParse",
       "textDocument/abstractParse",
       "textDocument/semanticTokens/fullFencepost"
    }
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/fullFencepost"]] :=
Catch[
Module[{id, params, doc, uri, entry, ast, data,
  line, char, oldLine, oldChar},

  If[$Debug2,
    log["textDocument/semanticTokens/fullFencepost: enter"]
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

  data = Lookup[entry, "SemanticTokens", Null];

  If[data =!= Null,
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> data |> |>}]
  ];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[ast]
  ];

  Block[{$LexicalScope, $Data},
  
    (*
    $LexicalScope is an assoc of names -> decls
    *)
    $LexicalScope = <||>;

    (*
    $Data is a list of {line, startChar, len, tokenType, tokenModifiers}
    *)
    $Data = {};

    walk[ast];

    data = $Data;
  ];

  (*
  Relativize the tokens
  *)

  data = Sort[data];

  line = 0;
  char = 0;

  data = Function[{t},
    oldLine = line;
    oldChar = char;
    line = t[[1]];
    char = t[[2]];
    {line - oldLine, If[oldLine == line, char - oldChar, char], t[[3]], t[[4]], t[[5]]}
  ] /@ data;

  data = Flatten[data];

  entry["SemanticTokens"] = data;

  $OpenFilesMap[uri] = entry;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> data |> |>}
]]



walk[ContainerNode[File, children_, _]] :=
  Scan[walk, children]

walk[PackageNode[_, children_, _]] :=
  Scan[walk, children]

walk[ContextNode[_, children_, _]] :=
  Scan[walk, children]

walk[NewContextPathNode[_, children_, _]] :=
  Scan[walk, children]

walk[CallNode[LeafNode[Symbol, "Module", _], children:{CallNode[LeafNode[Symbol, "List", _], _, _], _}, _]] :=
Module[{variableSymbols, newScope},

  variableSymbols = children[[1, 2]];
  variableSymbols = Replace[variableSymbols, {
    sym:LeafNode[Symbol, _, _] :> sym,
    CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], _}, _] :> lhs
  }, 1];

  newScope = <| (#[[2]] -> {"Module"})& /@ variableSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "DynamicModule", _], children:{CallNode[LeafNode[Symbol, "List", _], _, _], _}, _]] :=
Module[{variableSymbols, newScope},

  variableSymbols = children[[1, 2]];
  variableSymbols = Replace[variableSymbols, {
    sym:LeafNode[Symbol, _, _] :> sym,
    CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], _}, _] :> lhs
  }, 1];

  newScope = <| (#[[2]] -> {"Module"})& /@ variableSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Block", _], children:{CallNode[LeafNode[Symbol, "List", _], _, _], _}, _]] :=
Module[{variableSymbols, newScope},

  variableSymbols = children[[1, 2]];
  variableSymbols = Replace[variableSymbols, {
    sym:LeafNode[Symbol, _, _] :> sym,
    CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], _}, _] :> lhs
  }, 1];

  newScope = <| (#[[2]] -> {"Block"})& /@ variableSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Internal`InheritedBlock", _], children:{CallNode[LeafNode[Symbol, "List", _], _, _], _}, _]] :=
Module[{variableSymbols, newScope},

  variableSymbols = children[[1, 2]];
  variableSymbols = Replace[variableSymbols, {
    sym:LeafNode[Symbol, _, _] :> sym,
    CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], _}, _] :> lhs
  }, 1];

  newScope = <| (#[[2]] -> {"Block"})& /@ variableSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "With", _], children:{CallNode[LeafNode[Symbol, "List", _], _, _], _}, _]] :=
Module[{variableSymbols, newScope},

  variableSymbols = children[[1, 2]];
  variableSymbols = Replace[variableSymbols, {
    sym:LeafNode[Symbol, _, _] :> sym,
    CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs:LeafNode[Symbol, _, _], _}, _] :> lhs
  }, 1];

  newScope = <| (#[[2]] -> {"parameter"})& /@ variableSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "SetDelayed", _], children:{_, _}, _]] :=
Module[{patterns, patternSymbols, newScope},

  patterns = Cases[children[[1]], CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, _, _], _}, _], {0, Infinity}];
  patternSymbols = #[[2, 1]]& /@ patterns;

  newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "TagSetDelayed", _], children:{_, _, _}, _]] :=
Module[{patterns, patternSymbols, newScope},

  patterns = Cases[children[[2]], CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, _, _], _}, _], {0, Infinity}];
  patternSymbols = #[[2, 1]]& /@ patterns;

  newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "RuleDelayed", _], children:{_, _}, _]] :=
Module[{patterns, patternSymbols, newScope},

  patterns = Cases[children[[1]], CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, _, _], _}, _], {0, Infinity}];
  patternSymbols = #[[2, 1]]& /@ patterns;

  newScope = <| (#[[2]] -> {"parameter"})& /@ patternSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Function", _], children:{LeafNode[Symbol, _, _], _}, _]] :=
Module[{param, newScope},

  param = children[[1]];

  newScope = <| (param[[2]] -> {"parameter"}) |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Function", _], children:{LeafNode[Symbol, _, _], _, _}, _]] :=
Module[{param, newScope},

  param = children[[1]];

  newScope = <| (param[[2]] -> {"parameter"}) |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Function", _], children:{CallNode[LeafNode[Symbol, "List", _], _, _], _}, _]] :=
Module[{paramSymbols, newScope},

  paramSymbols = children[[1, 2]];
  paramSymbols = Replace[paramSymbols, {
    sym:LeafNode[Symbol, _, _] :> sym
  }, 1];

  newScope = <| (#[[2]] -> {"parameter"})& /@ paramSymbols |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Do", _], children:{_, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _}, _]}, _]] :=
Module[{param, newScope},

  param = children[[2, 2, 1]];

  newScope = <| (param[[2]] -> {"parameter"}) |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Do", _], children:{_, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _, _}, _]}, _]] :=
Module[{param, newScope},

  param = children[[2, 2, 1]];

  newScope = <| (param[[2]] -> {"parameter"}) |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Table", _], children:{_, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _}, _]}, _]] :=
Module[{param, newScope},

  param = children[[2, 2, 1]];

  newScope = <| (param[[2]] -> {"parameter"}) |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[LeafNode[Symbol, "Table", _], children:{_, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, _, _], _, _}, _]}, _]] :=
Module[{param, newScope},

  param = children[[2, 2, 1]];

  newScope = <| (param[[2]] -> {"parameter"}) |>;

  Internal`InheritedBlock[{$LexicalScope},

    $LexicalScope = Merge[{$LexicalScope, newScope}, Flatten];

    Scan[walk, children]
  ]
]

walk[CallNode[head_, children_, _]] := (
  walk[head];
  Scan[walk, children]
)

walk[LeafNode[Symbol, name_, data_]] :=
Module[{decls = Lookup[$LexicalScope, name, {}]},
  If[!empty[decls],
      AppendTo[$Data, {
          #[[1, 1]], #[[1, 2]], #[[2, 2]] - #[[1, 2]],
          tokenType[decls],
          modifiersBitset[decls]
        }&[data[[Key[Source]]]-1]
      ]
  ]
]

walk[LeafNode[_, _, _]] :=
  Null

walk[ErrorNode[_, _, _]] :=
  Null

walk[UnterminatedCallNode[_, _, _]] :=
  Null

walk[UnterminatedGroupNode[_, _, _]] :=
  Null


End[]

EndPackage[]
