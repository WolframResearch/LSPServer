BeginPackage["LSPServer`SelectionRange`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/selectionRange"], pos_] :=
Catch[
Module[{params, id, doc, uri},

  If[$Debug2,
    log["textDocument/selectionRange: enter expand"]
  ];

  id = content["id"];
  params = content["params"];
  
  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["canceled"]
    ];
    
    Throw[{<| "method" -> "textDocument/selectionRangeFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
  
    If[$Debug2,
      log["stale"]
    ];

    Throw[{<| "method" -> "textDocument/selectionRangeFencepost", "id" -> id, "params" -> params, "stale" -> True |>}]
  ];

  <| "method" -> #, "id" -> id, "params" -> params |>& /@ {
    "textDocument/concreteParse",
    "textDocument/selectionRangeFencepost"
  }
]]

handleContent[content:KeyValuePattern["method" -> "textDocument/selectionRangeFencepost"]] :=
Catch[
Module[{id, params, doc, uri, entry, cst, positions, cursor, cases, firstCase, firstCaseSrc, firstCasePos,
  posChain, rangeify, selectionRanges},

  If[$Debug2,
    log["textDocument/selectionRangeFencepost: enter"]
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

  positions = params["positions"];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  cst = entry["CST"];

  If[FailureQ[cst],
    Throw[cst]
  ];

  selectionRanges = Function[{position},
    
    cursor = { #["line"], #["character"] }&[position];

    (* convert from 0-based to 1-based *)
    cursor+=1;

    cases = Cases[cst, _[_, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, cursor]]], Infinity];

    firstCase = cases[[1]];

    firstCaseSrc = firstCase[[3, Key[Source]]];

    firstCasePos = Position[cst, firstCase][[1]];

    posChain = NestWhileList[If[ListQ[Extract[cst, #[[;; -2]]]], #[[;; -3]], #[[;; -2]]]&, firstCasePos, (Length[#] >= 2)&];

    posChain = Reverse[posChain];

    posChain = Rest[posChain];

    If[$Debug2,
      log["posChain: ", posChain]
    ];

    rangeify[pos_] := <|
      "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
      "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>&[Extract[cst, pos][[3, Key[Source]]]-1];

    Fold[<| "range" -> rangeify[#2], "parent" -> #1 |>&, <| "range" -> rangeify[First[posChain]] |>, Rest[posChain]]

  ] /@ positions;

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> selectionRanges |>}
]]


End[]

EndPackage[]
