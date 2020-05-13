BeginPackage["LSPServer`Definitions`"]

Begin["`Private`"]


Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]

handleContent[content:KeyValuePattern["method" -> "textDocument/definition"]] :=
Catch[
Module[{id, params, doc, uri, ast, position, locations, line, char, cases, sym, name, srcs, entry, cst, agg},

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
  cst = entry[[2]];
  agg = CodeParser`Abstract`Aggregate[cst];
  ast = CodeParser`Abstract`Abstract[agg];

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

  name = sym["String"];

  cases =
    Cases[
      ast,
      CallNode[LeafNode[Symbol, "SetDelayed", _], {lhs_, rhs_}, KeyValuePattern["Definition" -> name]] :> lhs,
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
