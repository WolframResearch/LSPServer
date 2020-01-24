BeginPackage["LSPServer`Hover`"]

Begin["`Private`"]


Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["AST`"]


handleContent[content:KeyValuePattern["method" -> "textDocument/hover"]] :=
Module[{id, params, doc, uri, file, ast, position, hover},

  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  position = params["position"];

  file = normalizeURI[uri];

  ast = ParseFile[File[file]];

  hover = <| "contents" -> "123 XXX 456" |>;

  <|"jsonrpc" -> "2.0", "id" -> id, "result" -> hover |>
]


End[]

EndPackage[]
