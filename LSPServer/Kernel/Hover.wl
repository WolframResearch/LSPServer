BeginPackage["LSPServer`Hover`"]

Begin["`Private`"]


Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


handleContent[content:KeyValuePattern["method" -> "textDocument/hover"]] :=
Catch[
Module[{id, params, doc, uri, position},

  id = content["id"];
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];
  position = params["position"];

  (*
  give a null response, and then send a multisource string notification
  *)
  {<|"jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>} ~Join~
    publishMultisourceStringNotification[uri, position]
]]



publishMultisourceStringNotification[uri_String, position_] :=
Catch[
Module[{lines, entry, cst, text, textLines, strs, lineMap, originalLineNumber, line, originalColumn, rules,
  char, decoded, rule, positionLine, positionColumn, segment, pre, index},

  entry = $OpenFilesMap[uri];

  text = entry[[1]];

  textLines = StringSplit[text, {"\r\n", "\n", "\r"}, All];

  positionLine = position["line"];
  positionColumn = position["character"];
  
  (*
  Convert from 0-based to 1-based
  *)
  positionLine++;
  positionColumn++;

  (*
  Adjust the hover position to accommodate tab stops
  FIXME: Must use the tab width from the editor
  *)
  pre = StringTake[textLines[[positionLine]], positionColumn-1];
  positionColumn = 1;
  Scan[(If[# == "\t", positionColumn = (4 * Quotient[positionColumn, 4] + 1) + 4, positionColumn++])&, Characters[pre]];

  (*
  Using "TabWidth" -> 4 here because the notification is rendered down to HTML and tabs need to be expanded in HTML
  FIXME: Must use the tab width from the editor
  *)
  cst = CodeConcreteParse[text, "TabWidth" -> 4];

  strs = Cases[cst,
    LeafNode[String, str_ /;
      StringContainsQ[str, RegularExpression[
        "\\\\(?:\
(?:\\[[a-zA-Z0-9]+\\])|\
(?::[0-9a-fA-F]{4})|\
(?:\\.[0-9a-fA-F]{2})|\
(?:[0-7]{3})|\
(?:\\|[0-9a-fA-F]{6})\
)"]], KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {positionLine, positionColumn}]]], Infinity];

  lineMap = <||>;
  If[!empty[strs],

    Function[{str},

      {originalLineNumber, originalColumn} = str[[3, Key[Source], 1]];

      segments = StringSplit[str[[2]], {"\r\n", "\n", "\r"}, All];

      If[Length[segments] == 1,

        segment = segments[[1]];

        (*
        Handle tab stops
        FIXME: Must use the tab width from the editor
        *)
        originalColumnCount = 1;
        Scan[(
          If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
          ,
          Characters[textLines[[originalLineNumber]]]
        ];

        rules = {};
        
        decoded = Quiet[Check[ToExpression[segment], $Failed]];

        If[!FailureQ[decoded],
          index = originalColumn;
          Function[{char},
            Switch[char,
              "\t",
                index = (4 * Quotient[index, 4] + 1) + 4
              ,
              " ",
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

          (*
          Handle tab stops
          FIXME: Must use the tab width from the editor
          *)
          originalColumnCount = 1;
          Scan[(
            If[# == "\t", originalColumnCount = (4 * Quotient[originalColumnCount, 4] + 1) + 4, originalColumnCount++];)&
            ,
            Characters[textLines[[originalLineNumber + segmentIndex[[1]] - 1]]]
          ];

          rules = {};
          Switch[segmentIndex,
            {1},
              decoded = Quiet[Check[ToExpression[segment <> "\""], $Failed]];
              If[!FailureQ[decoded],
                index = originalColumn;
                Function[{char},
                  Switch[char,
                    "\t",
                      index = (4 * Quotient[index, 4] + 1) + 4
                    ,
                    " ",
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
            {Length[segments]},
              decoded = Quiet[Check[ToExpression["\"" <> segment], $Failed]];
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
                    _,
                      rule = index -> char;
                      AppendTo[rules, rule];
                      index++
                  ];
                ] /@ Characters[decoded]
              ];
            ,
            _,
              decoded = Quiet[Check[ToExpression["\"" <> segment <> "\""], $Failed]];
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
  ];

  lines = Values[lineMap];

  lines = <|#, "content" -> StringJoin[#["characters"]], "characterCount" -> Length[#["characters"]]|>& /@ lines;

  {<| "jsonrpc" -> "2.0",
      "method" -> "textDocument/publishHTMLSnippet",
      "params" -> <|   "uri" -> uri,
                     "lines" -> lines,
                     "actions" -> {} |> |>}
]]





End[]

EndPackage[]
