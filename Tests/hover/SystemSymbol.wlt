Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "SystemSymbolTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "SystemSymbolTest.wl"}]];

(* SystemSymbol containing usage message with linear syntax *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 0, "character" -> 1|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "Sin\\[*z*\\] gives the sine of *z*\\. \n\n_[Sin: Web Documentation](https://reference.wolfram.com/language/ref/Sin.html)_"
        |>
      |>
    |>
  },
  TestID -> "IDE-Test-SystemSymbol-Linear-Syntax"
]


(* SystemSymbol (cos) is located one tabspace away from start of line in SystemSymbolTest.wl *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 2, "character" -> 6|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, 
      "result" -> <|"contents" -> <|
        "kind" -> "markdown", 
        "value" -> "Cos\\[*z*\\] gives the cosine of *z*\\. \n\n_[Cos: Web Documentation](https://reference.wolfram.com/language/ref/Cos.html)_"
        |>
      |>
    |>
  },
TestID -> "IDE-Test-SystemSymbol-withTab"]


(* SystemSymbol (ExternalEvaluate) with multi-line usage message *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/hoverFencepost", 
      "id" -> 3, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 4, "character" -> 11|>|>
    |>
  ],
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, 
      "result" -> <|"contents" -> 
        <|"kind" -> "markdown", 
          "value" -> "ExternalEvaluate\\[\"*sys*\",\"*cmd*\"\\] evaluates the command \
*cmd* in the external evaluator *sys*, returning an expression corresponding \
to the output\\.\n\nExternalEvaluate\\[\\{\"*sys*\",*opts*\\},\"*cmd*\"\\] \
uses the options *opts* for the external evaluator\\.\n\nExternalEvaluate\\[DatabaseReference\\[*ref*\\],\"*cmd*\"\\] \
evaluates *cmd* using the database specified by *ref*\\.\n\nExternalEvaluate\\[*assoc*,\"*cmd*\"\\] evaluates *cmd* \
using the external evaluator specified by *assoc*\\.\n\nExternalEvaluate\\[*session*,\"*cmd*\"\\] evaluates *cmd* \
in the specified running ExternalSessionObject\\.\n\nExternalEvaluate\\[*sys*\\-&gt;\"*type*\",\[Ellipsis]\\] \
returns output converted to the specified type\\. \n\nExternalEvaluate\\[*spec*,*obj*\\] evaluates the content \
of the specified File, URL or CloudObject\\.\n\nExternalEvaluate\\[*spec*,*assoc*\\] evaluates the command specified \
by *assoc*\\.\n\nExternalEvaluate\\[*spec*,\\{*cmd*\\_1,*cmd*\\_2,\[Ellipsis]\\}\\] evaluates the list of \
commands *cmd*\\_*i*\\.\n\nExternalEvaluate\\[*spec*\\] represents an operator form of ExternalEvaluate that \
can be applied to a command or object\\.\n\nEXPERIMENTAL\n\n_[ExternalEvaluate: Web Documentation](https://reference.wolfram.com/language/ref/ExternalEvaluate.html)_"
      |>
    |>
  |>
  },
  TestID -> "IDE-Test-SystemSymbol-Multiline-Usage"
]
  

