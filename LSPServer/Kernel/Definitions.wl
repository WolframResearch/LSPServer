BeginPackage["LSPServer`Definitions`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/definition"], pos_] :=
Catch[
Module[{params, id, doc, uri},

  If[$Debug2,
    log["textDocument/definition: enter expand"]
  ];
  
  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "method" -> "textDocument/definitionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/definitionFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/aggregateParse",
    "textDocument/abstractParse",
    "textDocument/definitionFencepost"
  }
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/definitionFencepost"]] :=
Catch[
Module[{id, params, doc, uri, ast, position, locations, line, char, cases, sym, namePat, srcs, entry},

  If[$Debug2,
    log["textDocument/definitionFencepost: enter"]
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

  position = params["position"];
  line = position["line"];
  char = position["character"];

  (*
  convert from 0-based to 1-based
  *)
  line+=1;
  char+=1;

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  ast = entry["AST"];

  If[FailureQ[ast],
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  (*
  Find the name of the symbol at the position
  *)
  cases = Cases[ast, LeafNode[Symbol, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  If[cases == {},
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  sym = cases[[1]];

  namePat = sym["String"];

  (*
  Remove contexts
  *)
  namePat = StringReplace[namePat, __ ~~ "`" -> ""];

  (*
  Definitions may be specified with or without context
  *)
  namePat = (__ ~~ "`" ~~ namePat) | namePat;

  cases = Flatten[Cases[ast, _[_, _, KeyValuePattern["Definitions" -> defs_ /; AnyTrue[defs, StringMatchQ[#[[2]], namePat]&]]], Infinity]];

  srcs = #[[3, Key[Source]]]& /@ cases;

  locations =
    Function[{src},
      <| "uri" -> uri,
         "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                       "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>
      |>&[Map[Max[#, 0]&, src-1, {2}]]] /@ srcs;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>}
]]


End[]

EndPackage[]
