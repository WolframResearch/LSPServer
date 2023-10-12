BeginPackage["LSPServer`Completion`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Hover`"]
Needs["LSPServer`ReplaceLongNamePUA`"]
Needs["LSPServer`Utils`"]
Needs["CodeFormatter`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]
Needs["CodeParser`Scoping`"]


(* Maximum number of suggested system symbols in completion popup menu *)
$maxSuggestedFunction = 10;


(* 
	 The kind of the completion item. Based of the kind an icon is chosen by the editor. The 
   standardized set of available values is defined in LSPServer specification page.
	  
   https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
*)
$CompletionItemKind = <|
  "Text" -> 1,
  "Method" -> 2,
  "Function" -> 3,
  "Constructor" -> 4,
  "Field" -> 5,
  "Variable" -> 6,
  "Class" -> 7,
  "Interface" -> 8,
  "Module" -> 9,
  "Property" -> 10,
  "Unit" -> 11,
  "Value" -> 12,
  "Enum" -> 13,
  "Keyword" -> 14,
  "Snippet" -> 15,
  "Color" -> 16,
  "File" -> 17,
  "Reference" -> 18,
  "Folder" -> 19,
  "EnumMember" -> 20,
  "Constant" -> 21,
  "Struct" -> 22,
  "Event" -> 23,
  "Operator" -> 24,
  "TypeParameter" -> 25
|>


handleContent[content:KeyValuePattern["method" -> "textDocument/completion"]] :=
Catch[
Module[{id, params, doc, uri, position, line, char, entry, text, ast, cstTabs, textLines, textInCurrentLine, scopedLocalVars,
  tokenSymbol, userSymbols, systemSymbols, optionSymbols, userSymbolItems, systemSymbolItems, optionSymbolItems, items},


  log[1, "textDocument/completion: enter"];


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
  line+=1; (* line number of the cursor position *)
  char+=1; (* character number of the cursor position from the start of the line *)


  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  (* Full text of the file *)
  text = entry["Text"];
  ast = Lookup[entry, "AST", Null];


  If[ast === Null,
    ast = Lookup[entry, "PreviousAST", Null]
  ];

  cstTabs = Lookup[entry, "CSTTabs", Null];

  textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];

  (* Text in same line where the cursor is *)
  textInCurrentLine = StringTake[textLines[[line]], char-1];

  (* 
  If the text contains tab, then the cursor column position needs to be adjusted.
  Depending upon the position of the tab cursor column position goes to the nearest multiple of 4.
  *)
  If[StringContainsQ[text, "\t"],
    (*
    FIXME: Must use the tab width from the editor
    *)
    char = 1;
    Scan[(If[# == "\t", char = 4 * (Quotient[char-1, 4] + 1) + 1, char++])&, Characters[textInCurrentLine]];
  ];

  (*
  Using "TabWidth" -> 4 here because the notific
  ation is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)
  cstTabs = CodeConcreteParse[textInCurrentLine, "TabWidth" -> 4];

  (* 
  Find the symbol available at the cursor column position char.
  SourceMemberQ condition takes only the symbol available at char.
  The line no is 1 as we are using cstTabs of a single line.

  As cstTabs is CST of a line, finding the token symbol upto Infinity level 
  is not computationally expensive.
  *)
  tokenSymbol = Cases[cstTabs, 
    LeafNode[Symbol, ts_, 
      KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {1, char}]] 
    ] :> ts, 
    Infinity
  ];
  
  log[2, "tokenSymbol :> ", InputForm[tokenSymbol]];
  
  scopedLocalVars = findScopedLocalVarsAST[ast, {line, char}];

  If[tokenSymbol === {},
    Throw[{
      <|
        "jsonrpc" -> "2.0", 
        "id" -> id, 
        "result" -> <| "isIncomplete" -> True, "items" -> <||> |> 
      |>
    }]
  ];

  tokenSymbol = First[tokenSymbol];

  userSymbols = Lookup[entry, "UserSymbols", Null];

  If[userSymbols === Null,
    userSymbols = Lookup[entry, "PreviousUserSymbols", Null]
  ];

  userSymbols = Join[userSymbols, scopedLocalVars];


  (* Find the user defined symbols that are starting with tokenSymbol *)
  userSymbols = ReplaceAll[
    StringCases[userSymbols, StartOfString ~~ tokenSymbol ~~ ___], {{x_} :> x}
  ];

  userSymbols = DeleteCases[userSymbols, {}];


  systemSymbols = Names["System`" <> tokenSymbol <> "*", IgnoreCase -> True];
  systemSymbols = Take[systemSymbols, UpTo[$maxSuggestedFunction]];
  optionSymbols = Intersection[WolframLanguageSyntax`Generate`$options, systemSymbols];
  systemSymbols = Complement[systemSymbols, optionSymbols];

  userSymbolItems = <| "label" -> #, "kind" -> $CompletionItemKind["Variable"], "detail" -> "User Defined Symbol" |>& /@ userSymbols;

  systemSymbolItems = <| 
    "label" -> #, 
    "kind" -> $CompletionItemKind["Function"], 
    "detail" -> "System Function", 
    "documentation" ->  <| "kind" -> "markdown"
                          , "value" -> mdUsage[#] 
                        |>
  |>& /@ systemSymbols;

  optionSymbolItems = <|
    "label" -> #, 
    "kind" -> $CompletionItemKind["Field"], 
    "detail" -> "Function Option", 
    "documentation" -> <| "kind" -> "markdown", "value" -> mdUsage[#] |>
  |>& /@ optionSymbols;

  items = Join[userSymbolItems, systemSymbolItems, optionSymbolItems];


  log[1, "completion: exiting"];


  {<| "jsonrpc" -> "2.0", "id" ->id, "result" -> <| "isIncomplete" -> True, "items" -> items |> |>}
]]



mdUsage[sym_] := 
Module[{symbolUsage},
  symbolUsage = ToExpression[sym <> "::usage"];
  symbolUsage = ReplaceAll[symbolUsage, 
    {
      _MessageName :> "No usage message.",
      _ :> StringJoin[linearToMDSyntax[symbolUsage]]
    }
  ]
]

(* 
A sorted list of all the scoped symbols available from the level 
specified by the cursorPosition upto the last level. 
*)
findScopedLocalVarsAST[ast_, cursorPosition_List] := Union[Last /@ ScopingData[ast, SourceMemberQ[#[[3, Key[Source]]], cursorPosition] &]]



End[]

EndPackage[]
