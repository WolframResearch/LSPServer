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

(*
no expandContent here because textDocument/documentSymbol is a request
*)

handleContent[content:KeyValuePattern["method" -> "textDocument/documentSymbol"]] :=
Catch[
Module[{id, params, doc, uri, ast, entry, cst, agg, symbolInfo, defs, text, fileName, fileFormat},

  If[$Debug2,
    log["textDocument/documentSymbol: enter"]
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

  ast = Lookup[entry, "AST", Null];

  If[ast === Null,
    
    agg = Lookup[entry, "Agg", Null];

    If[agg === Null,
      
      cst = Lookup[entry, "CST", Null];

      If[cst === Null,

        text = entry["Text"];

        fileName = normalizeURI[uri];

        fileFormat = Automatic;
        If[FileExtension[fileName] == "wls",
          fileFormat = "Script"
        ];

        cst = CodeConcreteParse[text, "FileFormat" -> fileFormat];
      ];

      If[FailureQ[cst],
        Throw[cst]
      ];

      agg = CodeParser`Abstract`Aggregate[cst];
      
      entry["Agg"] = agg;

      $OpenFilesMap[uri] = entry
    ];
    
    If[FailureQ[agg],
      Throw[agg]
    ];

    ast = CodeParser`Abstract`Abstract[agg];
    
    entry["AST"] = ast;

    $OpenFilesMap[uri] = entry
  ];

  If[FailureQ[ast],
    Throw[ast]
  ];

  defs = Cases[
    ast,
    CallNode[LeafNode[Symbol, "SetDelayed" | "Set", _], {_, _}, KeyValuePattern["Definition" -> _]],
    Infinity
  ];
    
  symbolInfo = <| "name" -> #[[1]]["String"],
                  "kind" -> $SymbolKind[#[[2]]],
                  "range" -> <|
                    "start" -> <| "line" -> #[[1]][[3, Key[Source], 1, 1]] - 1, "character" -> #[[1]][[3, Key[Source], 1, 2]] - 1 |>,
                    "end" -> <| "line" -> #[[1]][[3, Key[Source], 2, 1]] - 1, "character" -> #[[1]][[3, Key[Source], 2, 2]] - 1 |> |>,
                  "selectionRange" -> <|
                    "start" -> <| "line" -> #[[1]][[3, Key[Source], 1, 1]] - 1, "character" -> #[[1]][[3, Key[Source], 1, 2]] - 1 |>,
                    "end" -> <| "line" -> #[[1]][[3, Key[Source], 2, 1]] - 1, "character" -> #[[1]][[3, Key[Source], 2, 2]] - 1 |> |>
               |>& /@ DeleteCases[findDefSymbol[#[[2, 1]]]& /@ defs, $Failed];

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> symbolInfo |>}
]]




findDefSymbol[n:LeafNode[Symbol, _, KeyValuePattern[Source -> _]]] :=
  {n, "Variable"}

findDefSymbol[CallNode[LeafNode[Symbol, "Condition", _], {node_, _}, KeyValuePattern[Source -> _]]] :=
  findDefSymbol[node]

findDefSymbol[CallNode[LeafNode[Symbol, "Pattern", _], {_, node_}, KeyValuePattern[Source -> _]]] :=
  findDefSymbol[node]

findDefSymbol[CallNode[LeafNode[Symbol, "PatternTest", _], {node_, _}, KeyValuePattern[Source -> _]]] :=
  findDefSymbol[node]

findDefSymbol[CallNode[LeafNode[Symbol, "HoldPattern", _], {node_}, KeyValuePattern[Source -> _]]] :=
  findDefSymbol[node]

(*
just give the first arg here
*)
findDefSymbol[CallNode[LeafNode[Symbol, "MessageName", _], {node_, ___}, KeyValuePattern[Source -> _]]] :=
  (If[FailureQ[#], #, {#[[1]], "Constant"}])&[findDefSymbol[node]]

findDefSymbol[CallNode[node_, _, KeyValuePattern[Source -> _]]] :=
  (If[FailureQ[#], #, {#[[1]], "Function"}])&[findDefSymbol[node]]

(*
findDefSymbol[CallNode[LeafNode[Symbol, "HoldPattern", _], {GroupNode[GroupSquare, {_, node_, _}, _]}, _]] := findDefSymbol[node]
*)

(*
findDefSymbol[CallNode[{node_, ___}, _, _]] := {findDefSymbol[node][[1]], "Function"}
*)

(*
findDefSymbol[BinaryNode[Condition, {node_, _, _}, _]] := findDefSymbol[node]
findDefSymbol[BinaryNode[Pattern, {_, _, node_}, _]] := findDefSymbol[node]
findDefSymbol[BinaryNode[BinaryAt, {node_, _, _}, _]] := findDefSymbol[node]
findDefSymbol[BinaryNode[PatternTest, {node_, _, _}, _]] := findDefSymbol[node]

(*
just give the first arg here
*)
findDefSymbol[InfixNode[MessageName, {node_, ___}, _]] := {findDefSymbol[node][[1]], "Constant"}

findDefSymbol[GroupNode[GroupParen, {_, node_, _}, _]] := findDefSymbol[node]

findDefSymbol[CompoundNode[PatternBlank, {_, node_}, _]] := findDefSymbol[node]
findDefSymbol[CompoundNode[Blank, {_, node_}, _]] := findDefSymbol[node]
*)

findDefSymbol[_] :=
  $Failed





End[]

EndPackage[]
