BeginPackage["LSPServer`SemanticTokens`"]

$SemanticTokenTypes

$SemanticTokenModifiers

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Scoping`"]
Needs["CodeParser`Utils`"]


$SemanticTokenTypes = <|
  "variable" -> 0,
  "parameter" -> 1,
  "function" -> 2,
  "constant" -> 3
|>

$SemanticTokenModifiers = <|
  "Module" -> 0,
  "Block" -> 1,
  "With" -> 2,
  "shadowed" -> 3,
  "unused" -> 4,
  "error" -> 5,
  "definition" -> 6
|>


expandContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/full"], pos_] :=
Catch[
Module[{params, id, doc, uri, res},

  log[1, "textDocument/semanticTokens/full: enter"];

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];
    
    Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    log[2, "stale"];

    Throw[{<| "method" -> "textDocument/semanticTokens/fullFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  res = <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/runScopingData",
    "textDocument/semanticTokens/fullFencepost"
  };

  log[1, "textDocument/semanticTokens/full: exit"];

  res
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/semanticTokens/fullFencepost"]] :=
Catch[
Module[{id, params, doc, uri, entry, semanticTokens, scopingData, transformed,
  line, char, oldLine, oldChar},

  log[1, "textDocument/semanticTokens/fullFencepost: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];
  
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[Lookup[content, "stale", False] || isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  semanticTokens = Lookup[entry, "SemanticTokens", Null];

  If[semanticTokens =!= Null,
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}]
  ];

  scopingData = Lookup[entry, "ScopingData", Null];

  If[scopingData === Null,
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  If[FailureQ[scopingData],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  (*
  transform data

  Related links: https://microsoft.github.io/language-server-protocol/specification#textDocument_semanticTokens
  *)

  transformed =
    Function[{source, scope, modifiers},
      {#[[1, 1]], #[[1, 2]], #[[2, 2]] - #[[1, 2]],
        
        $SemanticTokenTypes[
          Switch[scope,
            {___, "Module" | "Block" | "DynamicModule" | "Internal`InheritedBlock"},
              "variable"
            ,
            {___, "With"},
              "constant"
            ,
            {___, "Defined"},
              "function"
            ,
            _,
              "parameter"
          ]
        ],
        
        BitOr @@ BitShiftLeft[1, Lookup[$SemanticTokenModifiers, modifiers ~Join~ (
            Replace[scope,
              {
                "Module" | "DynamicModule" -> "Module",
                "Block" | "Internal`InheritedBlock" -> "Block",
                "With" -> "With",
                _ :> Sequence @@ {}
              }
              ,
              {1}
            ]
          )]]}&[source - 1]
    ] @@@ scopingData;

  (*
  Relativize the tokens
  *)

  transformed = Sort[transformed];

  line = 0;
  char = 0;

  transformed = Function[{t},
    oldLine = line;
    oldChar = char;
    line = t[[1]];
    char = t[[2]];
    {line - oldLine, If[oldLine == line, char - oldChar, char], t[[3]], t[[4]], t[[5]]}
  ] /@ transformed;

  transformed = Flatten[transformed];

  semanticTokens = transformed;
  
  entry["SemanticTokens"] = semanticTokens;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/semanticTokens/fullFencepost: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> <| "data" -> semanticTokens |> |>}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/runScopingData"]] :=
Catch[
Module[{params, doc, uri, entry, ast, scopingData},

  log[1, "textDocument/runScopingData: enter"];

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
  
  scopingData = Lookup[entry, "ScopingData", Null];

  If[scopingData =!= Null,
    Throw[{}]
  ];

  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[{}]
  ];

  log[2, "before ScopingData"];

  scopingData = ScopingData[ast];

  log[2, "after ScopingData"];

  entry["ScopingData"] = scopingData;

  $OpenFilesMap[uri] = entry;

  log[1, "textDocument/runScopingData: exit"];

  {}
]]


End[]

EndPackage[]
