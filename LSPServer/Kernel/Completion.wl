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
Module[{id, params, doc, uri, position, line, char, entry, text, ast, cstTabs, textLines, partialText, pre, scopedLocalVars,
  tokenSymbol, userSymbols, systemSymbols, optionSymbols, userSymbolItems, systemSymbolItems, optionSymbolItems, items},

  (* TODO: Delete this line when PR review is over *)
  log["textDocument/completion: enter"];

  If[$Debug2,
    log["textDocument/completion: enter"]
  ];

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
  line+=1;
  char+=1;

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];


  text = entry["Text"];
  ast = Lookup[entry, "AST", Null];

  If[ast === Null,
    ast = Lookup[entry, "PreviousAST", Null]
  ];

  cstTabs = Lookup[entry, "CSTTabs", Null];

  textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
  partialText = textLines[[line]];

  If[StringContainsQ[text, "\t"],
    pre = StringTake[textLines[[line]], char-1];
    char = 1;
    Scan[(If[# == "\t", char = (4 * Quotient[char, 4] + 1) + 4, char++])&, Characters[pre]]
  ];

  cstTabs = CodeConcreteParse[partialText, "TabWidth" -> 4];

  tokenSymbol = Cases[cstTabs, 
    LeafNode[Symbol, ts_, 
      KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {1, char}]]
    ] :> ts, 
    Infinity
  ];

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


  userSymbols = ReplaceAll[
    (StringCases[#, StartOfString ~~ tokenSymbol ~~ ___]& /@ userSymbols), {
      {x_} :> x}
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

  (* TODO: Delete this line when PR review is over *)
  log["textDocument/completion: exit"];

  If[$Debug2,
    log["completion: exiting"]
  ];


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


findScopedLocalVarsAST[ast_, cursorPosition_List] := Union[Last /@ ScopingData[ast, SourceMemberQ[#[[3, Key[Source]]], cursorPosition] &]]



End[]

EndPackage[]
