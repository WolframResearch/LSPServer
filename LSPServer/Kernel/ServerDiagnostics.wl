BeginPackage["LSPServer`ServerDiagnostics`"]


ServerDiagnosticWarningMessages


Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]


$MinimumRecommendedCodeToolsVersion = 1.2

$MinimumRecommendedKernelVersion = 12.1



RunServerDiagnostic[command:{_String...}] :=
  Catch[
  Module[{proc, stdIn, stdOut, assoc, bytes, str, cases, case, len, content, lenStr, runPosition, run, toTest,
    lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion,
    lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate,
    contentStr, res, kernelVersion},

    Print["Running Language Server diagnostic..."];

    If[!StringStartsQ[ToLowerCase[FileBaseName[command[[1]]]], "wolframkernel"],
      Print["WARNING: Command for Wolfram Language Server does not start with 'WolframKernel': ", command[[1]]];
    ];
    If[!MemberQ[command, "-noinit"],
      Print["WARNING: -noinit is not in command"];
    ];
    If[!MemberQ[command, "-noprompt"],
      Print["ERROR: -noprompt is not in command"];
    ];
    If[!MemberQ[command, "-nopaclet"],
      Print["WARNING: -nopaclet is not in command"];
    ];
    If[!MemberQ[command, "-noicon"],
      Print["ERROR: -noicon is not in command"];
    ];
    If[!MemberQ[command, "-run"],
      Print["ERROR: -run is not in command"];
    ];


    runPosition = FirstPosition[command, "-run"];

    If[!MissingQ[runPosition],
      run = command[[runPosition[[1]] + 1]];

      If[StringQ[run],
        If[!SyntaxQ[run],
          Print["ERROR: code is not SyntaxQ: ", run];
          Throw[False]
        ];
        ,
        Print["ERROR: code is not a string: ", run];
        Throw[False]
      ];

      If[$OperatingSystem == "Windows" && StringContainsQ[run, "\""],
        (*
        work around bug 410895, all quotes are stripped from StartProcess on Windows

        convert e.g., Print["Foo`"] into ToExpression[FromCharacterCode[{80, 114, 105, 110, 116, 91, 34, 70, 111, 111, 96, 34, 93}]]

        evaluates the same expr, except that the only characters passed on command-line are letters, digits, space, comma, [] and {}
        *)
        run = "ToExpression[FromCharacterCode[" <> ToString[ToCharacterCode[run]] <> "]]"
      ];
      ,
      (*
      already WARNED about missing -run
      *)
      Throw[False]
    ];


    toTest = command;
    toTest = Delete[toTest, runPosition[[1]] + 1];
    toTest = Delete[toTest, 1];
    toTest = DeleteCases[toTest, "-noinit" | "-noprompt" | "-nopaclet" | "-noicon" | "-run"];

    If[toTest != {},
      Print["WARNING: There are unrecognized arguments to Language Server kernel: ", toTest];
    ];


    Print["Starting Language Server kernel with command: ", command];
    proc = StartProcess[command];

    Print[];
    Print["If any messages are printed below, they must be fixed."];
    Print[];
    
    If[FailureQ[proc],
      Throw[proc]
    ];

    stdIn = ProcessConnection[proc, "StandardInput"];
    stdOut = ProcessConnection[proc, "StandardOutput"];


    (*
    initialize
    *)
    assoc = <|"method" -> "initialize", "id" -> 1, "params" -> <|"capabilities" -> <|"textDocument" -> <|"codeAction" -> <||>|>|>|>|>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing initialize..."];
    BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];
    BinaryWrite[stdIn, bytes];
    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      Print["ERROR: Language Server kernel is not running after writing initialize; exiting hard"];
      Throw[exitHard[proc]]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      Throw[exitHard[proc]]
    ];
    While[bytes === {},
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
      If[bytes === EndOfFile,
        Print["ERROR: Unexpected EndOfFile; exiting hard"];
        Throw[exitHard[proc]]
      ];
    ];
    str = "";
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      Throw[exitHard[proc]]
    ];
    str = str <> ByteArrayToString[bytes];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from language server kernel:"];
      Print[str];
      diagnoseStdOut[str];

      Print["ERROR: Unrecognized header; exiting hard"];
      Throw[exitHard[proc]]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[len != StringLength[contentStr],

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Bad Content-Length; exiting hard"];
      Throw[exitHard[proc]]
    ];

    Print["initialize was successful."];


    (*
    diagnostics
    *)
    assoc = <|"method" -> "diagnostics", "id" -> 2|>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing diagnostics..."];
    BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];
    BinaryWrite[stdIn, bytes];
    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      Print["ERROR: Language Server kernel is not running after writing diagnostics; exiting hard"];
      Throw[exitHard[proc]]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      Throw[exitHard[proc]]
    ];
    While[bytes === {},
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
      If[bytes === EndOfFile,
        Print["ERROR: Unexpected EndOfFile; exiting hard"];
        Throw[exitHard[proc]]
      ];
    ];
    str = "";
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      Throw[exitHard[proc]]
    ];
    str = str <> ByteArrayToString[bytes];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from language server kernel:"];
      Print[str];
      diagnoseStdOut[str];

      Print["ERROR: Unrecognized header; exiting hard"];
      Throw[exitHard[proc]]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[len != StringLength[contentStr],

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Bad Content-Length; exiting hard"];
      Throw[exitHard[proc]]
    ];

    content = ImportString[contentStr, "RawJSON"];

    res = content["result"];

    kernelVersion = res["kernelVersion"];
    lspServerVersion = res["lspServerVersion"];
    codeParserVersion = res["codeParserVersion"];
    codeInspectorVersion = res["codeInspectorVersion"];
    codeFormatterVersion = res["codeFormatterVersion"];

    lspServerBuildDate = res["lspServerBuildDate"];
    codeParserBuildDate = res["codeParserBuildDate"];
    codeInspectorBuildDate = res["codeInspectorBuildDate"];
    codeFormatterBuildDate = res["codeFormatterBuildDate"];

    Print["INFORMATION: Kernel version: ", kernelVersion];
    Print["INFORMATION: LSPServer version: ", lspServerVersion];
    Print["INFORMATION: CodeParser version: ", codeParserVersion];
    Print["INFORMATION: CodeInspector version: ", codeInspectorVersion];
    Print["INFORMATION: CodeFormatter version: ", codeFormatterVersion];

    Print["INFORMATION: LSPServer build date: ", lspServerBuildDate];
    Print["INFORMATION: CodeParser build date: ", codeParserBuildDate];
    Print["INFORMATION: CodeInspector build date: ", codeInspectorBuildDate];
    Print["INFORMATION: CodeFormatter build date: ", codeFormatterBuildDate];

    checkWarnings[
      {kernelVersion, lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion},
      {lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate}
      ,
      (Print["WARNING: ", #];)&
    ];

    Print["diagnostics was successful."];


    (*
    shutdown
    *)
    assoc = <|"method" -> "shutdown", "id" -> 3|>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing shutdown..."];
    BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];
    BinaryWrite[stdIn, bytes];
    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      Print["ERROR: Language Server kernel is not running after writing shutdown; exiting hard"];
      Throw[exitHard[proc]]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      Throw[exitHard[proc]]
    ];
    While[bytes === {},
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
      If[bytes === EndOfFile,
        Print["ERROR: Unexpected EndOfFile; exiting hard"];
        Throw[exitHard[proc]]
      ];
    ];
    str = "";
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      Throw[exitHard[proc]]
    ];
    str = str <> ByteArrayToString[bytes];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Unrecognized header; exiting hard"];
      Throw[exitHard[proc]]
    ];

    case = cases[[1]];
    {lenStr, content} = case;
    len = ToExpression[lenStr];

    If[len != StringLength[content],

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Bad Content-Length; exiting hard"];
      Throw[exitHard[proc]]
    ];

    Print["shutdown was successful."];


    (*
    exit
    *)
    assoc = <|"method" -> "exit"|>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing exit..."];
    BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];
    BinaryWrite[stdIn, bytes];

    Pause[2.0];

    If[ProcessStatus[proc] != "Finished",
      Print["ERROR: Language Server kernel is not finished; exiting hard"];
      Throw[exitHard[proc]]
    ];

    Print["exit was successful."];

    Print["No problems found."];

    True
  ]]


ServerDiagnosticWarningMessages[] :=
Module[{kernelVersion, lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion,
    lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate},

  kernelVersion = $VersionNumber;

  lspServerVersion = Information[PacletObject["LSPServer"], "Version"];

  codeParserVersion = Information[PacletObject["CodeParser"], "Version"];

  codeInspectorVersion = Information[PacletObject["CodeInspector"], "Version"];

  codeFormatterVersion = Information[PacletObject["CodeFormatter"], "Version"];

  lspServerBuildDate = PacletObject["LSPServer"]["BuildDate"];

  codeParserBuildDate = PacletObject["CodeParser"]["BuildDate"];

  codeInspectorBuildDate = PacletObject["CodeInspector"]["BuildDate"];

  codeFormatterBuildDate = PacletObject["CodeFormatter"]["BuildDate"];

  Reap[
    checkWarnings[
      {kernelVersion, lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion}
      ,
      {lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate}
      ,
      Sow
    ]
    ,
    _
    ,
    #2[[1]]&
  ][[2]]
]


checkWarnings[
  {kernelVersion_, lspServerVersion_, codeParserVersion_, codeInspectorVersion_, codeFormatterVersion_}
  ,
  {lspServerBuildDateStr_, codeParserBuildDateStr_, codeInspectorBuildDateStr_, codeFormatterBuildDateStr_}
  ,
  warningFunc_
] :=
Module[{lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate, quantity10Days},

  (*
  minimum version checking;
  *)

  If[kernelVersion < $MinimumRecommendedKernelVersion,
    warningFunc["Kernel is below recommended version. Minimum recommended kernel version is: " <>
      ToString[$MinimumRecommendedKernelVersion] <> ". Actual kernel version is: " <> ToString[kernelVersion]]
  ];

  If[lspServerVersion < $MinimumRecommendedCodeToolsVersion,
    warningFunc["LSPServer is below recommended version. Minimum recommended LSPServer version is: " <>
      ToString[$MinimumRecommendedCodeToolsVersion] <> ". Actual LSPServer version is: " <> ToString[lspServerVersion]];
  ];
  If[codeParserVersion < $MinimumRecommendedCodeToolsVersion,
    warningFunc["CodeParser is below recommended version. Minimum recommended CodeParser version is: " <>
      ToString[$MinimumRecommendedCodeToolsVersion] <> ". Actual CodeParser version is: " <> ToString[codeParserVersion]];
  ];
  If[codeInspectorVersion < $MinimumRecommendedCodeToolsVersion,
    warningFunc["CodeInspector is below recommended version. Minimum recommended CodeInspector version is: " <>
      ToString[$MinimumRecommendedCodeToolsVersion] <> ". Actual CodeInspector version is: " <> ToString[codeInspectorVersion]];
  ];
  If[codeFormatterVersion < $MinimumRecommendedCodeToolsVersion,
    warningFunc["CodeFormatter is below recommended version. Minimum recommended CodeFormatter version is: " <>
      ToString[$MinimumRecommendedCodeToolsVersion] <> ". Actual CodeFormatter version is: " <> ToString[codeFormatterVersion]];
  ];

  If[lspServerVersion =!= codeParserVersion,
    warningFunc["LSPServer and CodeParser do not have the same version. LSPServer version: " <>
      ToString[lspServerVersion] <> ". CodeParser version: " <> ToString[codeParserVersion]];
  ];
  If[lspServerVersion =!= codeInspectorVersion,
    warningFunc["LSPServer and CodeInspector do not have the same version. LSPServer version: " <>
      ToString[lspServerVersion] <> ". CodeInspector version: " <> ToString[codeInspectorVersion]];
  ];
  If[lspServerVersion =!= codeFormatterVersion,
    warningFunc["LSPServer and CodeFormatter do not have the same version. LSPServer version: " <>
      ToString[lspServerVersion] <> ". CodeFormatter version: " <> ToString[codeFormatterVersion]];
  ];

  (*
  build date checking
  *)

  lspServerBuildDate = DateObject[{lspServerBuildDateStr, {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"}}];
  codeParserBuildDate = DateObject[{codeParserBuildDateStr, {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"}}];
  codeInspectorBuildDate = DateObject[{codeInspectorBuildDateStr, {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"}}];
  codeFormatterBuildDate = DateObject[{codeFormatterBuildDateStr, {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"}}];

  If[!DateObjectQ[lspServerBuildDate],
    warningFunc["LSPServer BuildDate cannot be parsed: " <> ToString[lspServerBuildDate]]
  ];
  If[!DateObjectQ[codeParserBuildDate],
    warningFunc["CodeParser BuildDate cannot be parsed: " <> ToString[codeParserBuildDate]]
  ];
  If[!DateObjectQ[codeInspectorBuildDate],
    warningFunc["CodeInspector BuildDate cannot be parsed: " <> ToString[codeInspectorBuildDate]]
  ];
  If[!DateObjectQ[codeFormatterBuildDate],
    warningFunc["CodeFormatter BuildDate cannot be parsed: " <> ToString[codeFormatterBuildDate]]
  ];

  quantity10Days = Quantity[10, "Days"];

  If[DateObjectQ[lspServerBuildDate] && DateObjectQ[codeParserBuildDate] && Abs[lspServerBuildDate - codeParserBuildDate] > quantity10Days,
    warningFunc["LSPServer and CodeParser were built too far apart. LSPServer build date: " <>
      lspServerBuildDateStr <> ". CodeParser build date: " <> codeParserBuildDateStr]
  ];
  If[DateObjectQ[lspServerBuildDate] && DateObjectQ[codeInspectorBuildDate] && Abs[lspServerBuildDate - codeInspectorBuildDate] > quantity10Days,
    warningFunc["LSPServer and CodeInspector were built too far apart. LSPServer build date: " <>
      lspServerBuildDateStr <> ". CodeInspector build date: " <> codeInspectorBuildDateStr]
  ];
  If[DateObjectQ[lspServerBuildDate] && DateObjectQ[codeFormatterBuildDate] && Abs[lspServerBuildDate - codeFormatterBuildDate] > quantity10Days,
    warningFunc["LSPServer and CodeFormatter were built too far apart. LSPServer build date: " <>
      lspServerBuildDateStr <> ". CodeFormatter build date: " <> codeFormatterBuildDateStr]
  ];
  If[DateObjectQ[codeParserBuildDate] && DateObjectQ[codeInspectorBuildDate] && Abs[codeParserBuildDate - codeInspectorBuildDate] > quantity10Days,
    warningFunc["CodeParser and CodeInspector were built too far apart. CodeParser build date: " <>
      codeParserBuildDateStr <> ". CodeInspector build date: " <> codeInspectorBuildDateStr]
  ];
  If[DateObjectQ[codeParserBuildDate] && DateObjectQ[codeFormatterBuildDate] && Abs[codeParserBuildDate - codeFormatterBuildDate] > quantity10Days,
    warningFunc["CodeParser and CodeFormatter were built too far apart. CodeParser build date: " <>
      codeParserBuildDateStr <> ". CodeFormatter build date: " <> codeFormatterBuildDateStr]
  ];
  If[DateObjectQ[codeInspectorBuildDate] && DateObjectQ[codeFormatterBuildDate] && Abs[codeInspectorBuildDate - codeFormatterBuildDate] > quantity10Days,
    warningFunc["CodeInspector and CodeFormatter were built too far apart. CodeInspector build date: " <>
      codeInspectorBuildDateStr <> ". CodeFormatter build date: " <> codeFormatterBuildDateStr]
  ];
]


handleContent[content:KeyValuePattern["method" -> "diagnostics"]] :=
  Catch[
  Module[{id, lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion, diags,
    lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate,
    kernelVersion},

    If[$Debug2,
      log["diagnostics: enter"]
    ];

    id = content["id"];

    kernelVersion = $VersionNumber;

    lspServerVersion = Information[PacletObject["LSPServer"], "Version"];

    codeParserVersion = Information[PacletObject["CodeParser"], "Version"];

    codeInspectorVersion = Information[PacletObject["CodeInspector"], "Version"];

    codeFormatterVersion = Information[PacletObject["CodeFormatter"], "Version"];

    lspServerBuildDate = PacletObject["LSPServer"]["BuildDate"];

    codeParserBuildDate = PacletObject["CodeParser"]["BuildDate"];

    codeInspectorBuildDate = PacletObject["CodeInspector"]["BuildDate"];

    codeFormatterBuildDate = PacletObject["CodeFormatter"]["BuildDate"];

    diags = <|
      "kernelVersion" -> kernelVersion,
      "lspServerVersion" -> lspServerVersion,
      "codeParserVersion" -> codeParserVersion,
      "codeInspectorVersion" -> codeInspectorVersion,
      "codeFormatterVersion" -> codeFormatterVersion,
      "lspServerBuildDate" -> lspServerBuildDate,
      "codeParserBuildDate" -> codeParserBuildDate,
      "codeInspectorBuildDate" -> codeInspectorBuildDate,
      "codeFormatterBuildDate" -> codeFormatterBuildDate
    |>;

    {<| "jsonrpc" -> "2.0",
        "id" -> id,
        "result" -> diags |>}
  ]]


exitHard[proc_] :=
  Module[{code},

    reportStdOut[proc];
    reportStdErr[proc];

    If[ProcessStatus[proc] != "Finished",
      Print["Killing process"];
      KillProcess[proc];
    ];

    code = ProcessInformation[proc, "ExitCode"];

    Print["Exit code: ", code];

    False
  ]


(*
This reports license errors
*)
reportStdOut[proc_] :=
  Module[{stdOut, arr, str},
    stdOut = ProcessConnection[proc, "StandardOutput"];

    While[True,

      arr = ReadByteArray[stdOut, EndOfBuffer];

      Which[
        ByteArrayQ[arr],
          str = ByteArrayToString[arr];
          Print["stdout from language server:"];
          Print[str]
        ,
        arr === EndOfFile,
          Break[]
        ,
        True,
          Print["error reading stdout from language server: ", arr];
          Break[]
      ]
    ]
  ]

reportStdErr[proc_] :=
  Module[{stdErr, arr, str},
    stdErr = ProcessConnection[proc, "StandardError"];

    While[True,

      arr = ReadByteArray[stdErr, EndOfBuffer];

      Which[
        ByteArrayQ[arr],
          str = ByteArrayToString[arr];
          Print["stderr from language server:"];
          Print[str]
        ,
        arr === EndOfFile,
          Break[]
        ,
        True,
          Print["error reading stderr from language server: ", arr];
          Break[]
      ]
    ]
  ]


diagnoseStdOut[str_String] :=
  Module[{},

    Which[
      StringMatchQ[str, RegularExpression["Mathematica .* Kernel for .*(\r?\n)Copyright 1988-.* Wolfram Research, Inc.(\r?\n)"]],
        Print["The startup banner was written to stdout. This breaks the Language Server Protocol."]
      ,
      StringMatchQ[str, RegularExpression["Content - \\(Length:\\d+\\)(\r?)\n"]],
        Print["The header was evaluated as code. LSPServer`StartServer[] is not intercepting stdin."]
    ]
  ]


End[]

EndPackage[]
