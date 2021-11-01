Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "PackageTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "PackageTest.wl"}]];

(* SystemSymbol in a package *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 25, "character" -> 2|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "Plot\\[*f*,\\{*x*,*x*\\_*min*,*x*\\_*max*\\}\\] generates a plot of \
*f* as a function of *x* from *x*\\_*min* to *x*\\_*max*\\. \n\n\
Plot\\[\\{*f*\\_1,*f*\\_2,\[Ellipsis]\\},\\{*x*,*x*\\_*min*,*x*\\_*\
max*\\}\\] plots several functions *f*\\_*i*\\. \n\nPlot\\[\\{\
\[Ellipsis],*w*\\[*f*\\_*i*\\],\[Ellipsis]\\},\[Ellipsis]\\] plots \
*f*\\_*i* with features defined by the symbolic wrapper *w*\\.\n\n\
Plot\\[\[Ellipsis],\\{*x*\\}\[Element]*reg*\\] takes the variable *x* \
to be in the geometric region *reg*\\.\n\n_[Plot: Web \
Documentation](https://reference.wolfram.com/language/ref/Plot.html)_"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-SystemSymbol-In-Package"
]


(* Function is defined in the Private context in a package*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 11, "character" -> 14|>|>
    |>
  ]
  , 
  {
    <|
      "jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nUsage message of testFunction\\.\n\n**Function Definition Patterns**\n\ntestFunction\\[a\\_\\]\\[b\\_\\]\n\n"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-ContextDepth-1-In-Package"
]


(* 
  If a function is defined one level below the Private context level,  
  we are not going to detect that.
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 3, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 16, "character" -> 18|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nUsage message of testFunction\\.\n\n**Function Definition Patterns**\n\ntestFunction\\[a\\_\\]\\[b\\_\\]\n\n"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-ContextDepth-2-In-Package"
]


(* Function with multiple usage in a package, defined with Set and SetDelayed *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 4, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 35, "character" -> 15|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nFirst usage message of multiUsageFunction\\.\n\nSecond usage message of multiUsageFunction\\.\n\n**Function Definition Patterns**\n\nmultiUsageFunction\\[x\\_\\]\n\n"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-MultipleUsage-In-Package"
]


(* Function defined one tabspace away from the start of the line inside a package *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 5, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 31, "character" -> 5|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 5, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nNo usage message\\.\n\n**Function Definition Patterns**\n\nfoo\\[\\]\n\n"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-Tab-Function-In-Package"
]


(* A function is defined with UpSetDelayed inside a package *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 6, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 23, "character" -> 2|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 6, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "**Usage**\n\nNo usage message\\.\n\n**Function Definition Patterns**\n\nf\\[g\\[x\\_\\]\\]\n\n"
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-UpSetDelayed-Function-1-In-Package"
]


(* 
  If a function is defined with TagSetDelayed,
  hovering over the untagged function "f" will show "No function information." 
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", "id" -> 7, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 27, "character" -> 5|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 7, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "No function information."
        |>
      |>
    |>
  }, 
  TestID -> "IDE-Test-TagSetDelayed-Function-2-In-Package"
]
