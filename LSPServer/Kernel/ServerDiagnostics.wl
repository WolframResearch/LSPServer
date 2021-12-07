(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>, "SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

BeginPackage["LSPServer`ServerDiagnostics`"]


ServerDiagnosticWarningMessages


Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]


$MinimumRecommendedCodeToolsVersion = 1.2

$MinimumRecommendedKernelVersion = 12.1

Options[RunServerDiagnostic] = {
  ProcessDirectory -> Inherited
}

RunServerDiagnostic[command:{_String...}, OptionsPattern[]] :=
  Catch[
  Module[{cwd, proc, stdIn, stdOut, assoc, bytes, str, cases, case, len, content, lenStr, runPosition, run, toTest,
    lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion,
    lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate,
    contentStr, res, kernelVersion, startServerString, startServer, startServerArgs, serverStartTime, serverInitializeTime},

    cwd = OptionValue[ProcessDirectory];

    Print["Running Language Server diagnostic..."];

    If[$startupMessagesText =!= "",
      Print["There were messages when loading LSPServer` package: ", $startupMessagesText];
      Throw[False]
    ];

    Print[];
    Print["Kernel that is running RunServerDiagnostic[] ($CommandLine[[1]]): ", $CommandLine[[1]]];
    Print["Kernel that RunServerDiagnostic[] will start (RunServerDiagnostic[{kernel, ...}]): ", command[[1]]];
    If[command[[1]] =!= $CommandLine[[1]],
      Print["WARNING: RunServerDiagnostic[] should be run with same kernel that RunServerDiagnostic[] will start."];
    ];
    If[!StringStartsQ[ToLowerCase[FileBaseName[command[[1]]]], "wolframkernel"],
      Print["WARNING: Command for Wolfram Language Server does not start with 'WolframKernel': ", command[[1]]];
    ];
    Print[];

    Print["Current directory (Directory[]): ", Directory[]];
    Print[];

    If[!MemberQ[command, "-noinit"],
      Print["WARNING: -noinit is not in command"];
    ];
    If[!MemberQ[command, "-noprompt"],
      Print["ERROR: -noprompt is not in command"];
      Throw[False]
    ];
    If[!MemberQ[command, "-nopaclet"],
      Print["WARNING: -nopaclet is not in command"];
    ];
    If[!MemberQ[command, "-noicon"],
      Print["ERROR: -noicon is not in command"];
      Throw[False]
    ];
    If[!MemberQ[command, "-nostartuppaclets"],
      Print["WARNING: -nostartuppaclets is not in command"];
    ];
    If[!MemberQ[command, "-run"],
      Print["ERROR: -run is not in command"];
      Throw[False]
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

      startServerString = StringCases[run, ss:("StartServer[" ~~ ___ ~~ "]" ~~ EndOfString) :> ss];
      If[Length[startServerString] == 1,
        startServerString = startServerString[[1]];
        
        Block[{StartServer},
          startServer = ToExpression[startServerString];
          startServerArgs = startServer /. StartServer[logDir_String : "", opts:OptionsPattern[]] :> {logDir, {opts}};
          
          If[(Global`CommunicationMethod /. startServerArgs[[2]]) == "Socket",
            Print["ERROR: CommunicationMethod \"Socket\" not implemented for RunServerDiagnostic", run];
            Throw[False]
          ]
        ]

      ];

      If[TrueQ[$WorkaroundBug410895],
        (*
        work around bug 410895, all quotes are stripped from StartProcess on Windows

        this was fixed in 13
        
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
    toTest = DeleteCases[toTest, "-noinit" | "-noprompt" | "-nopaclet" | "-nostartuppaclets" | "-noicon" | "-run"];

    If[toTest != {},
      Print["WARNING: There are unrecognized arguments to Language Server kernel: ", toTest];
    ];


    serverStartTime = Now;

    Print["Starting Language Server kernel with command: ", command];
    proc = StartProcess[command, ProcessDirectory -> cwd];

    (*
    Only kill process here
    Do not Print anything, Print output in a task will go to Messages Window
    *)
    $timeoutTask = SessionSubmit[ScheduledTask[$timeout = True; KillProcess[proc], {Quantity[30, "Seconds"], 1}]];

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
    assoc = <|"method" -> "initialize", "id" -> 1, "params" -> <|
        "initializationOptions" -> <||>,
        "capabilities" -> <|
          "textDocument" -> <|
            "codeAction" -> <||>
          |>
        |>
      |>
    |>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    str = "";

    Print["Writing initialize..."];
    res = Quiet[BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    res = Quiet[BinaryWrite[stdIn, bytes], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      Print["ERROR: Language Server kernel is not running after writing initialize; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    If[MatchQ[bytes, _ReadByteArray],
      Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    While[bytes === {} && str == "",
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
      If[bytes === EndOfFile,
        Print["ERROR: Unexpected EndOfFile; exiting hard"];
        exitHard[proc];
        Throw[False]
      ];
      If[MatchQ[bytes, _ReadByteArray],
        Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
        exitHard[proc];
        Throw[False]
      ];
    ];
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    If[MatchQ[bytes, _ReadByteArray],
      Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    str = str <> ByteArrayToString[bytes];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from language server kernel:"];
      Print[str];
      diagnoseStdOut[str];

      Print["ERROR: Unrecognized header; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[!TrueQ[len <= StringLength[contentStr]],

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Bad Content-Length; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    str = StringDrop[contentStr, len];

    serverInitializeTime = Now;

    Print["initialize was successful."];


    (*
    diagnostics
    *)
    assoc = <|"method" -> "diagnostics", "id" -> 2|>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing diagnostics..."];
    res = Quiet[BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    res = Quiet[BinaryWrite[stdIn, bytes], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    
    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      Print["ERROR: Language Server kernel is not running after writing diagnostics; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    If[MatchQ[bytes, _ReadByteArray],
      Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    While[bytes === {} && str == "",
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
      If[bytes === EndOfFile,
        Print["ERROR: Unexpected EndOfFile; exiting hard"];
        exitHard[proc];
        Throw[False]
      ];
      If[MatchQ[bytes, _ReadByteArray],
        Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
        exitHard[proc];
        Throw[False]
      ];
    ];
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    If[MatchQ[bytes, _ReadByteArray],
      Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    str = str <> ByteArrayToString[bytes];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from language server kernel:"];
      Print[str];
      diagnoseStdOut[str];

      Print["ERROR: Unrecognized header; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[!TrueQ[len <= StringLength[contentStr]],

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Bad Content-Length; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    str = StringDrop[contentStr, len];
    contentStr = StringTake[contentStr, len];

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
    res = Quiet[BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    res = Quiet[BinaryWrite[stdIn, bytes], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      Print["ERROR: Language Server kernel is not running after writing shutdown; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    If[MatchQ[bytes, _ReadByteArray],
      Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    While[bytes === {} && str == "",
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
      If[bytes === EndOfFile,
        Print["ERROR: Unexpected EndOfFile; exiting hard"];
        exitHard[proc];
        Throw[False]
      ];
      If[MatchQ[bytes, _ReadByteArray],
        Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
        exitHard[proc];
        Throw[False]
      ];
    ];
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    If[bytes === EndOfFile,
      Print["ERROR: Unexpected EndOfFile; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    If[MatchQ[bytes, _ReadByteArray],
      Print["ERROR: ReadByteArray returned unevaluated; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    str = str <> ByteArrayToString[bytes];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Unrecognized header; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[!TrueQ[len <= StringLength[contentStr]],

      Print["Read from language server kernel:"];
      Print[str];

      Print["ERROR: Bad Content-Length; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    
    str = StringDrop[contentStr, len];
    contentStr = StringTake[contentStr, len];

    Print["shutdown was successful."];


    (*
    exit
    *)
    assoc = <|"method" -> "exit"|>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing exit..."];
    res = Quiet[BinaryWrite[stdIn, "Content-Length: " <> ToString[len] <> "\r\n\r\n"], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];
    res = Quiet[BinaryWrite[stdIn, bytes], {BinaryWrite::errfile}];
    If[FailureQ[res],
      Print["ERROR: BinaryWrite failed; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    Pause[2.0];

    If[ProcessStatus[proc] != "Finished",
      Print["ERROR: Language Server kernel is not finished; exiting hard"];
      exitHard[proc];
      Throw[False]
    ];

    Print["exit was successful."];

    TaskRemove[$timeoutTask];

    Print["INFO: Time to initialize server: ", (serverInitializeTime - serverStartTime)];

    If[(serverInitializeTime - serverStartTime) > Quantity[10, "Seconds"],
      Print["ERROR: Time to initialize server was greater than 10 seconds"];
      Throw[False]
    ];

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

  Quiet[
  Check[
  (*
  Was:
  lspServerBuildDate = DateObject[{lspServerBuildDateStr, {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"}}];
  
  but as discussed here:
  https://github.com/WolframResearch/vscode-wolfram/issues/2

  it is more robust to use FromDateString with "Language" -> "en" option
  *)
  lspServerBuildDate = FromDateString[lspServerBuildDateStr, <| "Language" -> "en", "Elements" -> {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"} |>]
  ,
  warningFunc["Messages while parsing LSPServer BuildDate: " <> ToString[$MessageList]]
  ]];

  Quiet[
  Check[
  codeParserBuildDate = FromDateString[codeParserBuildDateStr, <| "Language" -> "en", "Elements" -> {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"} |>]
  ,
  warningFunc["Messages while parsing CodeParser BuildDate: " <> ToString[$MessageList]]
  ]];

  Quiet[
  Check[
  codeInspectorBuildDate = FromDateString[codeInspectorBuildDateStr, <| "Language" -> "en", "Elements" -> {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"} |>]
  ,
  warningFunc["Messages while parsing CodeInspector BuildDate: " <> ToString[$MessageList]]
  ]];

  Quiet[
  Check[
  codeFormatterBuildDate = FromDateString[codeFormatterBuildDateStr, <| "Language" -> "en", "Elements" -> {"DayName", " ", "Day", " ", "MonthName", " ", "Year", " ", "Hour", ":", "Minute", ":", "Second"} |>]
  ,
  warningFunc["Messages while parsing CodeFormatter BuildDate: " <> ToString[$MessageList]]
  ]];

  If[!DateObjectQ[lspServerBuildDate],
    warningFunc["LSPServer BuildDate cannot be parsed: " <> ToString[lspServerBuildDate, InputForm]]
  ];
  If[!DateObjectQ[codeParserBuildDate],
    warningFunc["CodeParser BuildDate cannot be parsed: " <> ToString[codeParserBuildDate, InputForm]]
  ];
  If[!DateObjectQ[codeInspectorBuildDate],
    warningFunc["CodeInspector BuildDate cannot be parsed: " <> ToString[codeInspectorBuildDate, InputForm]]
  ];
  If[!DateObjectQ[codeFormatterBuildDate],
    warningFunc["CodeFormatter BuildDate cannot be parsed: " <> ToString[codeFormatterBuildDate, InputForm]]
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
Catch[
Module[{code},

  If[$timeout,
    Print["Process timed out after 30 seconds."];

    code = ProcessInformation[proc, "ExitCode"];
    Print["INFO: Exit code: ", code];

    Throw[Null]
  ];

  TaskRemove[$timeoutTask];

  reportStdOut[proc];
  reportStdErr[proc];

  If[ProcessStatus[proc] == "Finished",
    
    Print["Process already finished."];

    code = ProcessInformation[proc, "ExitCode"];
    Print["INFO: Exit code: ", code];
    
    Throw[Null]
  ];

  Print["Killing process."];
  KillProcess[proc];

  code = ProcessInformation[proc, "ExitCode"];
  Print["INFO: Exit code: ", code];
]]


(*
This reports license errors
*)
reportStdOut[proc_] :=
  Module[{stdOut, arr, str},
    stdOut = ProcessConnection[proc, "StandardOutput"];

    While[True,
      Pause[0.1];
      
      arr = ReadByteArray[stdOut, EndOfBuffer];

      Which[
        arr === {},
          Print["INFO: stdout from language server: (empty)"];
        ,
        ByteArrayQ[arr],
          str = ByteArrayToString[arr];
          Print["INFO: stdout from language server: ", str];
        ,
        arr === EndOfFile,
          Break[]
        ,
        MatchQ[arr, _ReadByteArray],
          Print["ERROR: stdout from language server: << ReadByteArray returned unevaluated >>"];
          Break[]
        ,
        True,
          Print["ERROR: stdout from language server: ", arr];
          Break[]
      ]
    ]
  ]

reportStdErr[proc_] :=
  Module[{stdErr, arr, str},
    stdErr = ProcessConnection[proc, "StandardError"];

    While[True,
      Pause[0.1];

      arr = ReadByteArray[stdErr, EndOfBuffer];

      Which[
        arr === {},
          Print["INFO: stderr from language server: (empty)"];
        ,
        ByteArrayQ[arr],
          str = ByteArrayToString[arr];
          Print["INFO: stderr from language server: ", str];
        ,
        arr === EndOfFile,
          Break[]
        ,
        MatchQ[arr, _ReadByteArray],
          Print["ERROR: stderr from language server: << ReadByteArray returned unevaluated >>"];
          Break[]
        ,
        True,
          Print["ERROR: stderr from language server: ", arr];
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
