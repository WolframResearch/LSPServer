Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "ValuesTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "ValuesTest.wl"}]];

(* Function with SubValues *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 4, "character" -> 2|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nNo usage message\\.\n\n**Function Definition Patterns**\n\nsubFn\\[a\\_\\]\\[b\\_\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-SubValues-Simple"
]


(* A Function with SubValues and usage message*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 2, 
      "params" -> <|
        "textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 8, "character" -> 7
        |>
      |>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nSubValue function with usage\\.\n\n**Function Definition Patterns**\n\nsubFnUsage\\[a\\_\\]\\[b\\_\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-SubValues-Usage"
]


(* A Function with UpValue and usage message*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 3, 
      "params" -> <|
        "textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 12, "character" -> 7
        |>
      |>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\ng is a function with UpValue\\.\n\n**Function Definition Patterns**\n\nf\\[g\\]\n\n\n\nf\\[g\\[x\\_\\]\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-UpValues-Usage"
]


(* A Function with UpValue and usage message*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 4, 
      "params" -> <|
        "textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 13, "character" -> 0
        |>
      |>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "No function information."
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-UpValues-Untagged"
]


(* A Function with DownValue and usage message*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 5, 
      "params" -> <|
        "textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 17, "character" -> 6
        |>
      |>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 5, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\ndownValFn is a function with DownValue\\.\n\n**Function Definition Patterns**\n\ndownValFn\\[x\\_ /; x &gt; \\-2\\]\n\n\n\ndownValFn\\[x\\_ /; x &lt; 2\\]\n\n"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DownValues-Usage"
]


(* 
A Function with DownValue and usage message
downValFn[x_ /; x < 2] := g2[x]
hover over g2
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 6, 
      "params" -> <|
        "textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 18, "character" -> 27
        |>
      |>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 6, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "No function information."
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-DownValues-Untagged"
]
