Get[FileNameJoin[{DirectoryName[$TestFileName], "Init.wl"}]];
initFunction[FileNameJoin[{DirectoryName[$TestFileName], "SymbolTest.wl"}]];
uri = LocalObjects`PathToURI[FileNameJoin[{DirectoryName[$TestFileName], "SymbolTest.wl"}]];

(* 
    SystemSymbol in a script file: Autocompletion of Plo
    no user-symbol
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/completion", "id" -> 1, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 5, "character" -> 3|>|>
    |>
  ]
  ,
  {
    <|"jsonrpc" -> "2.0", "id" -> 1, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
  }, 
  TestID -> "IDE-Test-SystemSymbol",
  SameTest -> MatchQ
]


(* 
  SystemSymbol and use-symbol in a script file: Autocompletion of plo
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/completion", "id" -> 2, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 7, "character" -> 3|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* 
      should contain plotVar1 but not plotVar2
      <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> 
    *)
  }, 
  TestID -> "IDE-Test-UserSymbol-and-SystemSymbol",
  SameTest -> MatchQ
]


(* 
  Scoped local variable test
*)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/completion", "id" -> 3, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 14, "character" -> 7|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 3, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain var01, var02 *)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  }, 
  TestID -> "IDE-Test-scoped-1",
  SameTest -> MatchQ
]


VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/completion", "id" -> 4, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 15, "character" -> 7|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 4, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain name01, name02 not name1, name2*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  }, 
  TestID -> "IDE-Test-scoped-2",
  SameTest -> MatchQ
]


(* Scoped variable at the outer level *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/completion", "id" -> 5, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 34, "character" -> 7|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 5, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* No modBlock*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  }, 
  TestID -> "IDE-Test-Scoped-Outer-Block",
  SameTest -> MatchQ
]


(* Scoped variable at the inner level *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/completion", "id" -> 6, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 44, "character" -> 11|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 6, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain modBlock and modVar ...*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  }, 
  TestID -> "IDE-Test-Scoped-Inner-Block",
  SameTest -> MatchQ
]


(* Scoped variable at the inner level *)
VerificationTest[
  LSPServer`handleContent[
    <|"method" -> "textDocument/completion", "id" -> 7, 
      "params" -> <|"textDocument" -> <|"uri" -> uri|>, "position" -> <|"line" -> 9, "character" -> 4|>|>
    |>
  ]
  , 
  {
    <|"jsonrpc" -> "2.0", "id" -> 7, "result" -> <|"isIncomplete" -> True, "items" -> {__Association}|>|>
    (* should contain functionName1 and functionName2; user-defined variable*)
    (* <|"jsonrpc" -> "2.0", "id" -> 2, "result" -> <|"isIncomplete" -> True, "items" -> {KeyValuePattern["Label"-> "plotVar1"]}, __Association|>|> *)
  }, 
  TestID -> "IDE-Test-Function-Name",
  SameTest -> MatchQ
]


