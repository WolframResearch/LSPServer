Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "InFileUsageTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "InFileUsageTest.wl"}]];

(* Function with simple usage message *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 5, "character" -> 5|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nmySin is just a sin function\\.\n\n**Function Definition Patterns**\n\nmySin\\[x\\_\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-InFileUsage-Simple"
]


(* A function with usage message one tab space away from start of line *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 9, "character" -> 8|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nmyTan is Tan function\\.\n\n**Function Definition Patterns**\n\nmyTan\\[x\\_\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-InFileUsage-Tab"
]


(* Function with usage message containing linear syntax *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 3, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 7, "character" -> 3|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nCos\\[**z**\\] gives the cosine of *z*\\. \n\n**Function Definition Patterns**\n\nmyCos\\[x\\_\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-InFileUsage-LinearSyntax"
]


(* Usage message defined with SetDelay *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 4, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 18, "character" -> 1|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nUsage with SetDelay\n\n**Function Definition Patterns**\n\naa\\[x\\_\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-Usage-with-SetDelay"
]


(* Function without usage tag *)
VerificationTest[
LSPServer`handleContent[<|"method" -> "textDocument/hoverFencepost", 
  "id" -> 5, 
  "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 23, "character" -> 2|>|>
  |>
],
{
  <|"jsonrpc" -> "2.0", "id" -> 5, 
  "result" -> <|"contents" -> <|
    "kind" -> "markdown", 
    "value" -> "No function information."
      |>
    |>
  |>
},
TestID -> "IDE-Test-No-Usage-Tag"
]
