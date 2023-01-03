BeginPackage["LSPServer`Formatting`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeFormatter`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


handleContent[content:KeyValuePattern["method" -> "textDocument/formatting"]] :=
Catch[
Module[{params, doc, uri, id, cst, formatted, startLineCol, endLineCol, textEdit, options, tabSize, insertSpaces,
  indentationString, entry, text},

  log[1, "textDocument/formatting: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "canceled"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];

  options = params["options"];
  tabSize = options["tabSize"];
  insertSpaces = options["insertSpaces"];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];

  text = entry["Text"];

  log[2, "before CodeConcreteParse"];

  cst = CodeConcreteParse[text, "TabWidth" -> tabSize];

  log[2, "after CodeConcreteParse"];

  startLineCol = cst[[2, 1, 3, Key[Source], 1]];
  endLineCol = cst[[2, -1, 3, Key[Source], 2]];

  (*
  convert from 1-based to 0-based
  *)
  startLineCol--;
  endLineCol--;

  If[insertSpaces,
    indentationString = StringJoin[Table[" ", {tabSize}]]
    ,
    indentationString = "\t"
  ];

  formatted = CodeFormatCST[cst, "TabWidth" -> tabSize, "IndentationString" -> indentationString];

  If[FailureQ[formatted],
    Throw[formatted]
  ];

  textEdit = <| "range" -> <| "start" -> <| "line" -> startLineCol[[1]], "character" -> startLineCol[[2]] |>,
                              "end" ->   <| "line" -> endLineCol[[1]], "character" -> endLineCol[[2]] |> |>,
                "newText" -> formatted |>;

  log[1, "textDocument/formatting: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]


handleContent[content:KeyValuePattern["method" -> "textDocument/rangeFormatting"]] :=
Catch[
Module[{params, doc, uri, id, formatted, textEdit, entry, text, options, tabSize,
  insertSpaces, rangeSource, lines, range, indentationString},

  log[1, "textDocument/rangeFormatting: enter"];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    log[2, "$CancelMap: ", $CancelMap];
    
    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  doc = params["textDocument"];
  uri = doc["uri"];

  If[isStale[$ContentQueue, uri],
    
    log[2, "stale"];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  range = params["range"];
  
  rangeSource = { { range["start"]["line"], range["start"]["character"] },
                  { range["end"]["line"], range["end"]["character"] } };

  (* convert from 0-based to 1-based *)
  rangeSource+=1;

  options = params["options"];
  tabSize = options["tabSize"];
  insertSpaces = options["insertSpaces"];

  entry = Lookup[$OpenFilesMap, uri, Null];
  
  If[entry === Null,
    Throw[Failure["URINotFound", <| "URI" -> uri, "OpenFilesMapKeys" -> Keys[$OpenFilesMap] |>]]
  ];
  
  text = entry["Text"];

  lines = StringSplit[text, {"\r\n", "\n", "\r"}, All];
  lines = lines[[rangeSource[[1, 1]];;rangeSource[[2, 1]]]];
  If[rangeSource[[1, 1]] == rangeSource[[2, 1]],
    (*
    single line selection
    *)
    text = StringTake[lines[[1]], {rangeSource[[1, 2]], rangeSource[[2, 2]] - 1}]
    ,
    (*
    multiple line selection
    *)
    lines[[1]] = StringDrop[lines[[1]], rangeSource[[1, 2]] - 1];
    lines[[-1]] = StringTake[lines[[-1]], rangeSource[[2, 2]] - 1];
    (*
    FIXME: use the correct newline
    *)
    text = StringJoin[Riffle[lines, "\n"]]
  ];

  If[insertSpaces,
    indentationString = StringJoin[Table[" ", {tabSize}]]
    ,
    indentationString = "\t"
  ];

  formatted = CodeFormat[text, "TabWidth" -> tabSize, "IndentationString" -> indentationString];

  If[FailureQ[formatted],
    Throw[formatted]
  ];

  textEdit = <| "range" -> range,
                "newText" -> formatted |>;

  log[1, "textDocument/rangeFormatting: exit"];

  {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> { textEdit } |>}
]]

End[]

EndPackage[]
