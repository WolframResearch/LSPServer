BeginPackage["LSPServer`BracketMismatches`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeInspector`BracketMismatches`"]
Needs["CodeParser`"]


expandContent[content:KeyValuePattern["method" -> "textDocument/runBracketMismatches"], pos_] :=
  Catch[
  Module[{params, doc, uri},

    If[$Debug2,
      log["textDocument/runBracketMismatches: enter expand"]
    ];
    
    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$PreExpandContentQueue[[pos[[1]]+1;;]], uri],
    
      If[$Debug2,
        log["stale"]
      ];

      Throw[{}]
    ];

    <| "method" -> #, "params" -> params |>& /@ {
       "textDocument/concreteTabsParse",
       "textDocument/aggregateTabsParse",
       "textDocument/runBracketMismatchesFencepost"
    }
  ]]

handleContent[content:KeyValuePattern["method" -> "textDocument/runBracketMismatchesFencepost"]] :=
  Catch[
  Module[{params, doc, uri, entry, text, mismatches, aggTabs},
    
    If[$Debug2,
      log["textDocument/runBracketMismatchesFencepost: enter"]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];
    
    If[isStale[$ContentQueue, uri],
      
      If[$Debug2,
        log["stale"]
      ];

      Throw[{}]
    ];

    entry = $OpenFilesMap[uri];

    mismatches = Lookup[entry, "BracketMismatches", Null];

    If[mismatches =!= Null,
      Throw[{}]
    ];

    text = entry["Text"];
    aggTabs = entry["AggTabs"];

    (*
    Using $BracketMatcher here
    *)

    If[$Debug2,
      log["before CodeInspectBracketMismatchesAgg"]
    ];

    mismatches = CodeInspectBracketMismatchesAgg[aggTabs];

    If[$Debug2,
      log["after CodeInspectBracketMismatchesAgg"]
    ];

    If[$Debug2,
      log["mismatches: ", stringLineTake[StringTake[ToString[mismatches], UpTo[1000]], UpTo[20]]];
      log["...\n"]
    ];

    entry["BracketMismatches"] = mismatches;

    $OpenFilesMap[uri] = entry;

    {}
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/suggestBracketEdits"]] :=
  Catch[
  Module[{params, doc, uri, entry, text, mismatches, textLines, suggestions, badChunkLineNums,
    badChunkLines, badChunk, data, res},
    
    If[$Debug2,
      log["textDocument/suggestBracketEdits: enter"]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];
    
    If[isStale[$ContentQueue, uri],
      
      If[$Debug2,
        log["stale"]
      ];

      Throw[{}]
    ];
  
    entry = $OpenFilesMap[uri];

    data = Lookup[entry, "BracketEditsData", Null];

    If[data =!= Null,
      Throw[{}]
    ];

    text = entry["Text"];

    mismatches = entry["BracketMismatches"];

    textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];

    If[$Debug2,
      log["mismatches: ", mismatches]
    ];

    data =
      Function[{mismatch},

        badChunkLineNums = mismatch[[3, Key[Source], All, 1]];
        badChunkLines = Take[textLines, badChunkLineNums];
        badChunk = StringJoin[Riffle[badChunkLines, "\n"]];

        If[$Debug2,
          log["before ML4Code`SuggestBracketEdits"];
          log["badChunk: ", badChunk]
        ];

        (*
        Using $DefaultTabWidth of 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML

        I do not feel like going and changing the ML4Code code to properly call CodeConcreteParse with "TabWidth" -> 4

        FIXME: Must use the tab width from the editor
        *)
        Block[{CodeParser`Private`$DefaultTabWidth = 4},
          res =
            TimeConstrained[
              ML4Code`SuggestBracketEdits[badChunk]
              ,
              $ML4CodeTimeLimit
              ,
              $timeOut
            ];
            If[$Debug2,
              log["res: ", res]
            ];
            suggestions = res /. {$timeOut -> {}, $Failed -> {}};
        ];

        If[$Debug2,
          log["after ML4Code`SuggestBracketEdits"]
        ];

        {badChunkLineNums, badChunkLines, suggestions}

      ] /@ mismatches;

    entry["BracketEditsData"] = data;

    $OpenFilesMap[uri] = entry;

    {}
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/clearBracketMismatches"]] :=
  Catch[
  Module[{params, doc, uri, entry},

    If[$Debug2,
      log["textDocument/clearBracketMismatches: enter"]
    ];

    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];

    If[isStale[$ContentQueue, uri],
      
      If[$Debug2,
        log["stale"]
      ];

      Throw[{}]
    ];

    entry = $OpenFilesMap[uri];

    entry["BracketMismatches"] =.;

    entry["BracketEditsData"] =.;

    $OpenFilesMap[uri] = entry;

    {}
  ]]


handleContent[content:KeyValuePattern["method" -> "textDocument/publishBracketMismatches"]] :=
  Catch[
  Module[{params, doc, uri, lines, entry, text, actions, textLines, action, suggestions, confidenceMap, badChunkLineNums,
    badChunkLines, originalColumnCount, rank, chunkOffset, line1, line2, line3, line4,
    line1Map, line2Map, line3Map, line4Map,
    data},
    
    If[$Debug2,
      log["textDocument/publishBracketMismatches: enter"]
    ];
    
    params = content["params"];
    doc = params["textDocument"];
    uri = doc["uri"];
    
    If[isStale[$ContentQueue, uri],
      
      If[$Debug2,
        log["stale"]
      ];

      Throw[{}]
    ];
  
    entry = Lookup[$OpenFilesMap, uri, Null];

    (*
    Possibly cleared
    *)
    If[entry === Null,
      Throw[{<| "jsonrpc" -> "2.0",
                "method" -> "textDocument/publishHTMLSnippet",
                "params" -> <| "uri" -> uri,
                               "lines" -> {},
                               "actions" -> {} |> |>}]
    ];

    text = Lookup[entry, "Text", Null];

    (*
    Possibly cleared
    *)
    If[text === Null,
      Throw[{<| "jsonrpc" -> "2.0",
                "method" -> "textDocument/publishHTMLSnippet",
                "params" -> <| "uri" -> uri,
                               "lines" -> {},
                               "actions" -> {} |> |>}]
    ];

    data = Lookup[entry, "BracketEditsData", Null];

    (*
    Possibly cleared
    *)
    If[data === Null,
      Throw[{<| "jsonrpc" -> "2.0",
                "method" -> "textDocument/publishHTMLSnippet",
                "params" -> <| "uri" -> uri,
                               "lines" -> {},
                               "actions" -> {} |> |>}]
    ];

    lines = {};
    actions = {};

    textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];

    line1Map = <||>;
    line2Map = <||>;
    line3Map = <||>;
    line4Map = <||>;

    Function[{datum},

      {badChunkLineNums, badChunkLines, suggestions} = datum;

      suggestions = convertSuggestionToLineColumn[#, badChunkLines]& /@ suggestions;
      If[AnyTrue[suggestions, FailureQ],
        (*
        If any of the suggestions are malformed, then just give up. There is something wrong with ML4Code
        *)
        suggestions = {}
      ];

      confidenceMap = Association[MapIndexed[#1 -> #2[[1]] &, Reverse[Union[suggestions[[All, 3]]]]]];
      Function[{suggestion},
        rank = confidenceMap[suggestion[[3]]];

        (*
        offset to add to relative line numbers inside suggestions to obtain line numbers of text
        *)
        chunkOffset = badChunkLineNums[[1]] - 1;
        originalColumnCount = StringLength[textLines[[suggestion[[1, 3, 1]] + chunkOffset]]];

        If[$Debug2,
          log["rank: ", rank];
          log["chunkOffset: ", chunkOffset];
          log["originalColumnCount: ", originalColumnCount]
        ];

        {line1, line2, line3, line4, action} = suggestionToLinesAndAction[suggestion, chunkOffset, originalColumnCount, rank];
        If[TrueQ[$DebugBracketMatcher],
          (*
          if debug, then keep all lines separated
          *)
          AppendTo[lines, line1];
          (* AppendTo[lines, line2];
          AppendTo[lines, line3];
          AppendTo[lines, line4]; *)
          ,
          (*
          if not debug, then merge lines together
          *)
          If[KeyExistsQ[line1Map, line1["line"]],
            line1Map[line1["line"]] = merge[line1Map[line1["line"]], line1]
            ,
            line1Map[line1["line"]] = line1
          ];
          If[KeyExistsQ[line2Map, line2["line"]],
            line2Map[line2["line"]] = merge[line2Map[line2["line"]], line2]
            ,
            line2Map[line2["line"]] = line2
          ];
          If[KeyExistsQ[line3Map, line3["line"]],
            line3Map[line3["line"]] = merge[line3Map[line3["line"]], line3]
            ,
            line3Map[line3["line"]] = line3
          ];
          If[KeyExistsQ[line4Map, line4["line"]],
            line4Map[line4["line"]] = merge[line4Map[line4["line"]], line4]
            ,
            line4Map[line4["line"]] = line4
          ]
        ];

        AppendTo[actions, action]

      ] /@ suggestions
    ] /@ data;

    If[TrueQ[$DebugBracketMatcher],

      lines = <|#, "content" -> StringJoin[#["characters"]], "characterCount" -> Length[#["characters"]]|>& /@ lines
      ,

      lines = Values[line1Map] ~Join~ Values[line2Map] ~Join~ Values[line3Map] ~Join~ Values[line4Map];

      lines = <|#, "content" -> StringJoin[#["characters"]], "characterCount" -> Length[#["characters"]]|>& /@ lines;

      lines = Merge[<|#["line"] -> #|>& /@ lines, ({StringJoin["<div style=\"" <> "margin: 0;border: 0;padding: 0;\">", Riffle[(#["content"])& /@ #, "<br>"], "</div>"], #[[1]]["characterCount"]})&];
      
      lines = KeyValueMap[<|"line" -> #1, "content" -> #2[[1]], "characterCount" -> #2[[2]]|> &, lines]
    ];

    If[$Debug2,
      log["lines: ", lines];
      log["textDocument/publishBracketMismatches: exit"]
    ];

    {<| "jsonrpc" -> "2.0",
        "method" -> "textDocument/publishHTMLSnippet",
        "params" -> <| "uri" -> uri,
                       "lines" -> lines,
                       "actions" -> actions |> |>}
  ]]


suggestionToLinesAndAction[{{Insert, insertionText_String, {line_Integer, column_Integer}}, completed_String, prob_}, chunkOffset_Integer, originalColumnCount_Integer, rank_Integer] :=
  Module[{escaped, probStr},
    escaped = StringReplace[Characters[insertionText], {"<" -> "&lt;", ">" -> "&gt;"}];
    probStr = " probability: " <> ToString[PercentForm[prob]];

    $hrefIdCounter++;
    
    {
      (*
      top most line
      *)
      <|
        "line" -> line + chunkOffset,
        "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], {column -> makeHTML[Switch[rank, 1|2|3, blueRank[rank], _, gray[]], $upArrow, ToString[$hrefIdCounter], "Insert " <> escaped <> " " <> probStr]}]
      |>
      ,
      <|
        "line" -> line + chunkOffset,
        "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], {column -> makeHTML[Switch[rank, 1|2, blueRank[rank], _, gray[]], $upArrow, ToString[$hrefIdCounter], "Insert " <> escaped <> " " <> probStr]}]
      |>
      ,
      <|
        "line" -> line + chunkOffset,
        "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], {column -> makeHTML[Switch[rank, 1, blueRank[rank], _, gray[]], $upArrow, ToString[$hrefIdCounter], "Insert " <> escaped <> " " <> probStr]}]
      |>
      ,
      (*
      bottom most line
      *)
      <|
        "line" -> line + chunkOffset,
        "characters" ->
          If[TrueQ[$DebugBracketMatcher],
            ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], {
              column -> makeHTML[blueRank[rank], escaped[[1]], ToString[$hrefIdCounter], ""],
              column + 1 -> If[Length[escaped] == 2, makeHTML[blueRank[rank], escaped[[2]], ToString[$hrefIdCounter], ""], "&nbsp;"]
            }]
            ,
            Table["&nbsp;", {originalColumnCount + 1}]
          ]
      |>
      ,
      (*
      action
      *)
      <|"command" -> "insert",
        "insertionText" -> insertionText,
        "line" -> line + chunkOffset,
        "column" -> column,
        "href" -> ToString[$hrefIdCounter]
      |>
    }
  ]

suggestionToLinesAndAction[{{Delete, deletionText_String, {line_Integer, column_Integer}}, completed_String, prob_}, chunkOffset_Integer, originalColumnCount_Integer, rank_Integer] :=
  Module[{escaped, probStr},
    escaped = StringReplace[deletionText, {"<" -> "&lt;", ">" -> "&gt;"}];
    probStr = " probability: " <> ToString[PercentForm[prob]];

    $hrefIdCounter++;
    
    {
      (*
      top most line
      *)
      <|
        "line" -> line + chunkOffset,
        "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], column -> makeHTML[Switch[rank, 1|2|3, redRank[rank], _, gray[]], $downArrow, ToString[$hrefIdCounter], "Delete " <> escaped <> " prob: " <> probStr]]
      |>
      ,
      <|
        "line" -> line + chunkOffset,
        "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], column -> makeHTML[Switch[rank, 1|2, redRank[rank], _, gray[]], $downArrow, ToString[$hrefIdCounter], "Delete " <> escaped <> " prob: " <> probStr]]
      |>
      ,
      <|
        "line" -> line + chunkOffset,
        "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], column -> makeHTML[Switch[rank, 1, redRank[rank], _, gray[]], $downArrow, ToString[$hrefIdCounter], "Delete " <> escaped <> " prob: " <> probStr]]
      |>
      ,
      (*
      bottom most line
      *)
      <|
        "line" -> line + chunkOffset,
        "characters" -> Table["&nbsp;", {originalColumnCount + 1}]
      |>
      ,
      (*
      action
      *)
      <|
        "command" -> "delete",
        "deletionText" -> deletionText,
        "line" -> line + chunkOffset,
        "column" -> column,
        "href" -> ToString[$hrefIdCounter]
      |>
    }
  ]


gray[] :=
  RGBColor[0.74902, 0.74902, 0.74902]

redRank[rank_] /; TrueQ[$BracketMatcherUseDesignColors] :=
  RGBColor[0.984314, 0.0509804, 0.105882]

blueRank[rank_] /; TrueQ[$BracketMatcherUseDesignColors] :=
  RGBColor[0.27451, 0.662745, 0.988235]

(*
FIXME: designed for a white background color scheme, need to get current background from client
*)
redRank[rank_] /; !TrueQ[$BracketMatcherUseDesignColors] :=
  If[TrueQ[$DebugBracketMatcher],
    RGBColor[1, 0, 0]
    ,
    Switch[rank,
      1,
        RGBColor[1, 0, 0]
      ,
      2,
        RGBColor[1, 0.55, 0.55]
      ,
      3,
        RGBColor[1, 0.75, 0.75]
    ]
  ]

(*
FIXME: designed for a white background color scheme, need to get current background from client
*)
blueRank[rank_] /; !TrueQ[$BracketMatcherUseDesignColors] :=
  If[TrueQ[$DebugBracketMatcher],
    RGBColor[0, 0, 1]
    ,
    Switch[rank,
      1,
        RGBColor[0, 0, 1]
      ,
      2,
        RGBColor[0.55, 0.55, 1]
      ,
      3,
        RGBColor[0.75, 0.75, 1]
    ]
  ]


$upArrow = "\:25b2"

$downArrow = "\:25bc"

(*
FIXME: Use the same font as the editor
style = font-family: xxx;
*)
makeHTML[color_RGBColor, arrow_String, href_String, debugStr_String] /; TrueQ[$DebugBracketMatcher] :=
  Module[{colorHex},
    colorHex = StringJoin[IntegerString[Round[255 List @@ color], 16, 2]];
"\
<span style=\"" <> "margin: 0;border: 0;padding: 0;" <> "\">\
<a style=\"" <> "margin: 0;border: 0;padding: 0;text-decoration: none;" <> "color:#" <> colorHex <> ";" <> "\" href=" <> "\"" <> href <> "\"" <> ">" <> arrow <> "</a><br>\
<a style=\"" <> "margin: 0;border: 0;padding: 0;text-decoration: none;" <> "color:#" <> colorHex <> ";" <> "\" href=" <> "\"" <> href <> "\"" <> ">" <> debugStr <> "</a>\
</span>\
"
  ]

makeHTML[color_RGBColor, arrow_String, href_String, debugStr_String] /; !TrueQ[$DebugBracketMatcher] :=
  Module[{colorHex},
    colorHex = StringJoin[IntegerString[Round[255 List @@ color], 16, 2]];
"\
<span style=\"" <> "margin: 0;border: 0;padding: 0;" <> "\">\
<a style=\"" <> "margin: 0;border: 0;padding: 0;text-decoration: none;" <> "color:#" <> colorHex <> ";" <> "\" href=" <> "\"" <> href <> "\"" <> ">" <> arrow <> "</a>\
</span>\
"
  ]



convertSuggestionToLineColumn[{{command_Symbol, text_String, index_Integer}, completed_String, prob_}, badChunkLines_] :=
  Module[{line, column},
    {line, column} = indexToLineColumn[index, badChunkLines];
    {{command, text, {line, column}}, completed, prob}
  ]

(*
ML4Code`SuggestBracketEdits may be broken and return crazy unevaluated stuff

Make sure to filter this out
*)
convertSuggestionToLineColumn[___] :=
  $Failed


indexToLineColumn[index_, badChunkLines_] :=
  Module[{indexs, line, taken, lineStartIndex, column},
    indexs = FoldList[#1 + StringLength[#2] + 1 &, 1, badChunkLines];
    taken = TakeWhile[indexs, (# <= index) &];
    line = Length[taken];
    lineStartIndex = Last[taken];
    column = index - lineStartIndex + 1;
    {line, column}
  ]


End[]

EndPackage[]
