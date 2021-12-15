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
Module[{id, params, doc, uri, ast, entry, symbolInfo, defs},

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

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[ast]
  ];

  defs = Cases[ast, _[_, _, KeyValuePattern["Definitions" -> defs_]] :> defs, Infinity];
  
  defs = Flatten[defs];

  (*
  Source may have been abstracted away

  We are only interested in definitions that have Source
  *)
  defs = Cases[defs, LeafNode[Symbol, _, KeyValuePattern[Source -> _]]];

  symbolInfo =
    Function[{sym}, <|
      "name" -> sym["String"],
      "kind" -> $SymbolKind["Function"],
      "range" -> <|
        "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
        "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |>
      |>,
      "selectionRange" -> <|
        "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
        "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |>
      |>
      |>&[sym[[3, Key[Source]]] - 1]
    ] /@ defs;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> symbolInfo |>}
]]




End[]

EndPackage[]
