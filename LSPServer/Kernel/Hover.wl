BeginPackage["LSPServer`Hover`"]

Begin["`Private`"]


Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


handleContent[content:KeyValuePattern["method" -> "textDocument/hover"]] :=
Catch[
Module[{id, params, doc, uri, position, lines, entry, cst, text, textLines, strs, lineMap, originalLineNumber, line,
  originalColumn, rules, char, decoded, rule, positionLine, positionColumn, segment, pre, index, result, cstTabs},

  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  position = params["position"];

  entry = $OpenFilesMap[uri];

  positionLine = position["line"];
  positionColumn = position["character"];
  
  (*
  Convert from 0-based to 1-based
  *)
  positionLine++;
  positionColumn++;

  text = entry[[1]];
  cstTabs = entry[[3]];

  If[cstTabs === Null,
    (*
    Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
    FIXME: Must use the tab width from the editor
    *)
    cstTabs = CodeConcreteParse[text, "TabWidth" -> 4];
    
    $OpenFilesMap[[Key[uri], 3]] = cstTabs;
  ];

  If[StringContainsQ[text, "\t"],
    (*
    Adjust the hover position to accommodate tab stops
    FIXME: Must use the tab width from the editor
    *)
    textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
    pre = StringTake[textLines[[positionLine]], positionColumn-1];
    positionColumn = 1;
    Scan[(If[# == "\t", positionColumn = (4 * Quotient[positionColumn, 4] + 1) + 4, positionColumn++])&, Characters[pre]];
  ];

  (*
  Find strings with multi-SourceCharacter WLCharacters
  *)
  strs = Cases[cstTabs,
    LeafNode[String, str_ /; containsUnicodeCharacterQ[str],
      KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {positionLine, positionColumn}]]], Infinity];

  lineMap = <||>;

  Function[{str},

    {originalLineNumber, originalColumn} = str[[3, Key[Source], 1]];

    segments = StringSplit[str[[2]], {"\r\n", "\n", "\r"}, All];

    If[Length[segments] == 1,

      segment = segments[[1]];

      rules = {};
      
      decoded = convertSegment[segment];

      (*
      Handle tab stops
      FIXME: Must use the tab width from the editor
      *)
      originalColumnCount = 1;
      Scan[(
        If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
        ,
        Characters[decoded]
      ];

      If[!FailureQ[decoded],
        index = 1;
        Function[{char},
          Switch[char,
            "\t",
              index = (4 * Quotient[index, 4] + 1) + 4
            ,
            " ",
              index++
            ,
            "`",
              rule = index -> "`` ` ``";
              AppendTo[rules, rule];
              index++
            ,
            _,
              rule = index -> char;
              AppendTo[rules, rule];
              index++
          ]
        ] /@ Characters[decoded]
      ];

      If[rules != {},

        line = <| "line" -> originalLineNumber, "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], rules]|>;

        If[KeyExistsQ[lineMap, line["line"]],
          lineMap[line["line"]] = merge[lineMap[line["line"]], line]
          ,
          lineMap[line["line"]] = line
        ];
      ]

      ,

      MapIndexed[Function[{segment, segmentIndex},

        rules = {};
        Which[
          (positionLine == (originalLineNumber + segmentIndex[[1]] - 1)) && containsUnicodeCharacterQ[segment] && segmentIndex == {1},
            decoded = convertStartingSegment[segment];
            If[!FailureQ[decoded],

              (*
              Handle tab stops
              FIXME: Must use the tab width from the editor
              *)
              originalColumnCount = 1;
              Scan[(
                If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
                ,
                Characters[decoded]
              ];

              index = 1;
              Function[{char},
                Switch[char,
                  "\t",
                    index = (4 * Quotient[index, 4] + 1) + 4
                  ,
                  " ",
                    index++
                  ,
                  "`",
                    rule = index -> "`` ` ``";
                    AppendTo[rules, rule];
                    index++
                  ,
                  _,
                    rule = index -> char;
                    AppendTo[rules, rule];
                    index++
                ];
              ] /@ Characters[decoded]
            ];
          ,
          (positionLine == (originalLineNumber + segmentIndex[[1]] - 1)) && containsUnicodeCharacterQ[segment] && segmentIndex == {Length[segments]},
            decoded = convertEndingSegment[segment];
            If[!FailureQ[decoded],

              (*
              Handle tab stops
              FIXME: Must use the tab width from the editor
              *)
              originalColumnCount = 1;
              Scan[(
                If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
                ,
                Characters[decoded]
              ];

              index = 1;
              Function[{char},
                Switch[char,
                  "\t",
                    index = (4 * Quotient[index, 4] + 1) + 4
                  ,
                  " ",
                    index++
                  ,
                  "`",
                    rule = index -> "`` ` ``";
                    AppendTo[rules, rule];
                    index++
                  ,
                  _,
                    rule = index -> char;
                    AppendTo[rules, rule];
                    index++
                ];
              ] /@ Characters[decoded]
            ];
          ,
          (positionLine == (originalLineNumber + segmentIndex[[1]] - 1)) && containsUnicodeCharacterQ[segment],
            decoded = convertMiddleSegment[segment];
            If[!FailureQ[decoded],

              (*
              Handle tab stops
              FIXME: Must use the tab width from the editor
              *)
              originalColumnCount = 1;
              Scan[(
                If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
                ,
                Characters[decoded]
              ];

              index = 1;
              Function[{char},
                Switch[char,
                  "\t",
                    index = (4 * Quotient[index, 4] + 1) + 4
                  ,
                  " ",
                    index++
                  ,
                  "`",
                    rule = index -> "`` ` ``";
                    AppendTo[rules, rule];
                    index++
                  ,
                  _,
                    rule = index -> char;
                    AppendTo[rules, rule];
                    index++
                ];
              ] /@ Characters[decoded]
            ];
        ];

        If[rules != {},

          line = <| "line" -> originalLineNumber + segmentIndex[[1]] - 1, "characters" -> ReplacePart[Table["&nbsp;", {originalColumnCount + 1}], rules]|>;

          If[KeyExistsQ[lineMap, line["line"]],
            lineMap[line["line"]] = merge[lineMap[line["line"]], line]
            ,
            lineMap[line["line"]] = line
          ];
        ]

      ], segments]

    ];

  ] /@ strs;

  lines = Values[lineMap];

  lines = StringJoin[#["characters"]]& /@ lines;

  Which[
    Length[line] == 0,
      result = Null
    ,
    Length[lines] == 1,
      result = <| "contents" -> lines[[1]] |>
    ,
    True,
      result = <| "contents" -> "BAD!!!" |>
  ];

  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> result |>}
]]



endsWithOddBackslashesQ[str_String] := 
  StringMatchQ[str, RegularExpression[".*(?<!\\\\)\\\\(\\\\\\\\)*"]]

convertSegment[segment_] :=
  Quiet[Check[ToExpression[segment], $Failed]]

convertStartingSegment[segment_] :=
  Quiet[Check[ToExpression[segment <> If[endsWithOddBackslashesQ[segment], "\\\"", "\""]], $Failed]]

convertMiddleSegment[segment_] :=
  Quiet[Check[ToExpression["\"" <> segment <> If[endsWithOddBackslashesQ[segment], "\\\"", "\""]], $Failed]]

convertEndingSegment[segment_] :=
  Quiet[Check[ToExpression["\"" <> segment], $Failed]]



containsUnicodeCharacterQ[str_String] :=
  StringContainsQ[str, RegularExpression[
        "(?<!\\\\)\\\\(?:\\\\\\\\)*(?# odd number of leading backslashes)(?:\
(?:\\[[a-zA-Z0-9]+\\])|(?# \\[Alpha] long name)\
(?::[0-9a-fA-F]{4})|(?# \\:xxxx hex)\
(?:\\.[0-9a-fA-F]{2})|(?# \\.xx hex)\
(?:[0-7]{3})|(?# \\xxx octal)\
(?:\\|[0-9a-fA-F]{6})(?# \\|xxxxxx hex)\
)"]]



End[]

EndPackage[]
