BeginPackage["LSPServer`SemanticTokens`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/full"], pos_] :=
  Catch[
  Module[{params, id, doc, uri},

    If[$Debug2,
      log["textDocument/semanticTokens/full: enter expand"]
    ];

    id = content["id"];

    If[Lookup[$CancelMap, id, False],

      $CancelMap[id] =.;

      If[$Debug2,
        log["canceled"]
      ];
      
      Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params |>}]
    ];

    params = content["params"];
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
Module[{id, params, doc, uri, entry, ast, data, modules, variableSymbols, variableNames, variableOccurences,
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

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[ast]
  ];

  modules = Cases[ast, CallNode[LeafNode[Symbol, "Module", _], {_, _}, _], Infinity];

  data = {};

  Function[{module},
    variableSymbols = module[[2, 1, 2]];
    variableNames = #[[2]]& /@ variableSymbols;

    Function[{name},
      variableOccurences = Cases[module, LeafNode[Symbol, name, _], Infinity];

      Function[{occurence},
        AppendTo[data, {#[[1, 1]], #[[1, 2]], #[[2, 2]] - #[[1, 2]], 0, 0}&[occurence[[3, Key[Source]]]-1]]
      ] /@ variableOccurences

    ] /@ variableNames

  ] /@ modules;

  data = Sort[data];

  (*
  Relativize the tokens
  *)

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

  If[$Debug2,
    log["data: ", data]
  ];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> data |> |>}
]]


End[]

EndPackage[]
