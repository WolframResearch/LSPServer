Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "MissingCasesTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "MissingCasesTest.wl"}]];

(* Function is defined with no usage message *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 5, "character" -> 11|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nNo usage message\\.\n\n**Function Definition Patterns**\n\nnoFunctionUsage\\[x\\_Integer\\]\n\n"
        |>
      |>
    |>
  },
TestID -> "IDE-Test-NoUsage"
]


(* Function is not defined but only has usage message *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 8, "character" -> 5|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nTest function with only usage\\.\n\n**Function Definition Patterns**\n\nNo function defined\\."
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-OnlyUsage"]


(* Function is not defined and has usage message *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 3, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 11, "character" -> 8|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "No function information."
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-NoFunction-Information"]
  
