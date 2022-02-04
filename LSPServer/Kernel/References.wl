BeginPackage["LSPServer`References`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/references"], pos_] :=
  Catch[
  Module[{params, id, doc, uri},

    If[$Debug2,
      log["textDocument/references: enter expand"]
    ];

    id = content["id"];
    params = content["params"];
    
    If[Lookup[$CancelMap, id, False],

      $CancelMap[id] =.;

      If[$Debug2,
        log["canceled"]
      ];
      
      Throw[{<| "method" -> "textDocument/referencesFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
    ];

    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{<| "method" -> "textDocument/referencesFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
    ];

    <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
      "textDocument/concreteParse",
      "textDocument/referencesFencepost"
    }
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/referencesFencepost"]] :=
Catch[
Module[{id, params, doc, uri, cst, pos, line, char, cases, sym, name, srcs, entry, locations},
  
  If[$Debug2,
    log["textDocument/referencesFencepost: enter"]
  ];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["$CancelMap: ", $CancelMap]
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

  pos = params["position"];
  line = pos["line"];
  char = pos["character"];

  (*
  convert from 0-based to 1-based
  *)
  line+=1;
  char+=1;

  entry = $OpenFilesMap[uri];

  cst = entry["CST"];

  If[FailureQ[cst],
    Throw[cst]
  ];

  (*
  Find the name of the symbol at the position
  *)
  cases = Cases[cst, LeafNode[Symbol, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  If[cases == {},
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}]
  ];

  sym = cases[[1]];

  name = sym["String"];

  cases = Cases[cst, LeafNode[Symbol, name, _], Infinity];

  srcs = #[[3, Key[Source]]]& /@ cases;

  locations =
    Function[{src}, <|
      "uri" -> uri,
      "range" -> <|
        "start" -> <| "line" -> #[[1, 1]],"character" -> #[[1, 2]] |>,
        "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |>
      |>
    |>&[Map[Max[#, 0]&, src-1, {2}]]] /@ srcs;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>}
]]

End[]

EndPackage[]
