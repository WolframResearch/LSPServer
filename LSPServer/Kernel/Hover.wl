BeginPackage["LSPServer`Hover`"]

Begin["`Private`"]


Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]


handleContent[content:KeyValuePattern["method" -> "textDocument/hover"]] :=
Catch[
Module[{id, params, doc, uri, file, ast, position, hover},

  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  position = params["position"];

  file = normalizeURI[uri];

  ast = CodeParse[File[file]];

  If[FailureQ[ast],
    Throw[ast]
  ];

  hover = <| "contents" -> "123 XXX 456" |>;

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> hover |>}
]]


End[]

EndPackage[]
