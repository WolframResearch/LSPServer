<< LSPServer`

initFunction[filePath_]:=
Module[{uri, fileText},
  uri = LocalObjects`PathToURI[filePath];
  fileText = ReadString[filePath];

  LSPServer`handleContent[<|
    "method"-> #, 
    "params"-> <|
      "textDocument"-> <|
        "uri" -> uri, 
        "languageId"->"wolfram", 
        "version"-> 1, 
        "text"-> fileText
      |>
    |>
  |>]& /@
  {
    "textDocument/didOpenFencepost",
    "textDocument/concreteParse",
    "textDocument/concreteTabsParse",
    "textDocument/aggregateParse",
    "textDocument/aggregateTabsParse",
    "textDocument/abstractParse"
  }
]