BeginPackage["LSPServer`Definitions`"]

Begin["`Private`"]


Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

handleContent[content:KeyValuePattern["method" -> "textDocument/definition"]] :=
Catch[
Module[{id, params, doc, uri, ast, position, locations, line, char, cases, sym, namePat, srcs, entry, cst, agg},

  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  position = params["position"];
  line = position["line"];
  char = position["character"];

  (*
  convert from 0-based to 1-based
  *)
  line+=1;
  char+=1;

  entry = $OpenFilesMap[uri];

  ast = entry[[4]];

  If[ast === Null,
    
    cst = entry[[2]];
    agg = CodeParser`Abstract`Aggregate[cst];
    ast = CodeParser`Abstract`Abstract[agg];
    
    $OpenFilesMap[[Key[uri], 4]] = ast;
  ];

  If[FailureQ[ast],
    Throw[ast]
  ];

  (*
  Find the name of the symbol at the position
  *)
  cases = Cases[ast, LeafNode[Symbol, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

  If[cases == {},
    Throw[<|"jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>]
  ];

  sym = cases[[1]];

  namePat = sym["String"];

  (*
  Remove contexts
  *)
  namePat = StringReplace[namePat, __ ~~ "`" -> ""];

  (*
  Definition may be specified with or without context
  *)
  namePat = (__ ~~ "`" ~~ namePat) | namePat;

  cases =
    Cases[
      ast,
      CallNode[LeafNode[Symbol, "SetDelayed" | "Set", _], {lhs_, rhs_}, KeyValuePattern["Definition" -> name_ /; StringMatchQ[name, namePat]]] :> lhs,
      Infinity
    ];

  srcs = #[[3, Key[Source]]]& /@ cases;

  locations = (<|   "uri" -> uri,
                  "range" -> <| "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
                                  "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |> |>
               |>&[Map[Max[#, 0]&, #-1, {2}]])& /@ srcs;

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>}
]]





End[]

EndPackage[]
