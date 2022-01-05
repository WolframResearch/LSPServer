(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>, "SuspiciousSessionSymbol" -> <|Enabled -> False|>|>:: *)

BeginPackage["LSPServer`ServerDiagnostics`"]


ServerDiagnosticWarningMessages


StartMiniServer


Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Library`"]
Needs["LSPServer`Utils`"]


$MinimumRecommendedCodeToolsVersion = 1.2

$MinimumRecommendedKernelVersion = 12.1

Options[RunServerDiagnostic] = {
  ProcessDirectory -> Inherited
}

RunServerDiagnostic[command:{_String...}, OptionsPattern[]] :=
  Catch[
  Module[{cwd, lspServerVersion, codeParserVersion,
    codeInspectorVersion, codeFormatterVersion, lspServerBuildDate,
    codeParserBuildDate, codeInspectorBuildDate,
    codeFormatterBuildDate, serverKernel, baseName, res},

    cwd = OptionValue[ProcessDirectory];

    Print["Running Language Server diagnostic..."];
    Print[];
    Print["IMPORTANT: Make sure that all other kernels are shut down before running this diagnostic."];
    Print["Transient license limit errors may give false negatives."];
    Print[];

    lspServerVersion = Information[PacletObject["LSPServer"], "Version"];
    codeParserVersion = Information[PacletObject["CodeParser"], "Version"];
    codeInspectorVersion = Information[PacletObject["CodeInspector"], "Version"];
    codeFormatterVersion = Information[PacletObject["CodeFormatter"], "Version"];

    lspServerBuildDate = PacletObject["LSPServer"]["BuildDate"];
    codeParserBuildDate = PacletObject["CodeParser"]["BuildDate"];
    codeInspectorBuildDate = PacletObject["CodeInspector"]["BuildDate"];
    codeFormatterBuildDate = PacletObject["CodeFormatter"]["BuildDate"];

    (*
    Do not actually use hostKernel for anything

    It could just be "WolframKernel" and not useful
    *)
    Print["INFORMATION: Host Kernel: ", $CommandLine[[1]]];
    Print["INFORMATION: Host Kernel version: ", $VersionNumber];
    Print["INFORMATION: Host Kernel Directory[]: ", Directory[]];
    Print["INFORMATION: Host Kernel $MaxLicenseProcesses: ", $MaxLicenseProcesses];
    Print["INFORMATION: Host Kernel LSPServer version: ", lspServerVersion];
    Print["INFORMATION: Host Kernel CodeParser version: ", codeParserVersion];
    Print["INFORMATION: Host Kernel CodeInspector version: ", codeInspectorVersion];
    Print["INFORMATION: Host Kernel CodeFormatter version: ", codeFormatterVersion];
    Print["INFORMATION: Host Kernel LSPServer build date: ", lspServerBuildDate];
    Print["INFORMATION: Host Kernel CodeParser build date: ", codeParserBuildDate];
    Print["INFORMATION: Host Kernel CodeInspector build date: ", codeInspectorBuildDate];
    Print["INFORMATION: Host Kernel CodeFormatter build date: ", codeFormatterBuildDate];

    If[$startupMessagesText =!= "",
      Print["There were messages when loading LSPServer` package: ", $startupMessagesText];
      Throw[$Failed]
    ];

    serverKernel = command[[1]];

    baseName = FileBaseName[serverKernel];

    If[!StringStartsQ[ToLowerCase[baseName], "wolframkernel"],
      Print["ERROR: Command for Wolfram Language Server does not start with 'WolframKernel': ", serverKernel];
      Throw[$Failed]
    ];

    Block[{$WorkaroundBug410895},

      $WorkaroundBug410895 = False;
      
      res = doStage1[command, cwd];

      If[FailureQ[res],
        Throw[res]
      ];

      res = doStage2[command, cwd];

      If[FailureQ[res],
        Throw[res]
      ];
    ];

    Print[];
    Print["No problems found."];
  ]]


doStage1[command_, cwd_] :=
  Catch[
  Module[{serverKernel, miniRun, miniCommand, proc, str, res, cases,
    case, code},

    Print[];
    Print["Start Stage 1"];
    Print["Checking whether bug 410895 is present and work-around is needed..."];

    serverKernel = command[[1]];

    (*
    Test bug 410895
    Check whether StartProcess strips double-quote characters U+0022

    Need to make sure to definitely NOT have any double-quote characters in this command (except for the test itself)

    So effectively build a command that looks like: -run Test410895[(*"*)];ToExpression[FromCharacterCode[{1, 2, 3, ...}]]
    where there are no double-quote characters (except for the test itself)
    *)
    miniRun = "Test410895[(*\"*)];ToExpression[FromCharacterCode["<>ToString[ToCharacterCode["Needs[\"LSPServer`\"];LSPServer`ServerDiagnostics`StartMiniServer[]"]]<>"]]";

    miniCommand = {serverKernel, "-noinit", "-noprompt", "-nopaclet", "-noicon", "-nostartuppaclets", "-run", miniRun};

    Print["Starting Mini Server kernel with command: ", miniCommand];
    proc = Quiet[StartProcess[miniCommand, ProcessDirectory -> cwd], {StartProcess::pnfd}];

    If[FailureQ[proc],
      Print["ERROR: StartProcess failed"];
      Throw[proc]
    ];
    
    $timeoutTask = SessionSubmit[ScheduledTask[$timeout = True; KillProcess[proc], {Quantity[30, "Seconds"], 1}]];

    Print["Waiting maximum of 30 seconds for any hangs."];

    str = "";

    (*
    N[Pi]
    *)
    Print["Sanity checking..."];
    res = binaryWrite[proc, "N[Pi]\r\n"];

    If[FailureQ[res],
      Throw[res]
    ];

    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      exitHard[proc, "ERROR: Mini Server kernel is not running after writing N[Pi]; exiting hard"];
      Throw[$Failed]
    ];

    str = readByteArray[proc, str];

    If[FailureQ[str],
      Throw[str]
    ];
   
    If[str != "3.14159\r\n",

      Print["Read from Mini Server kernel:"];
      Print[str];
      diagnoseStdOut[str];

      exitHard[proc, "ERROR: Unrecognized response; exiting hard"];
      Throw[$Failed]
    ];

    Print["Sanity check was successful."];

    str = "";

    (*
    $CommandLine
    *)
    Print["Testing bug 410895..."];
    res = binaryWrite[proc, "$CommandLine\r\n"];

    If[FailureQ[res],
      Throw[res]
    ];

    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      exitHard[proc, "ERROR: Mini Server kernel is not running after writing $CommandLine; exiting hard"];
      Throw[$Failed]
    ];

    str = readByteArray[proc, str];

    If[FailureQ[str],
      Throw[str]
    ];

    If[(cases = StringCases[str, "Test410895[(*" ~~ ("\"" | "") ~~ "*)]"]) == {},

      Print["Read from Mini Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Unrecognized response; exiting hard"];
      Throw[$Failed]
    ];

    case = cases[[1]];
      
    Which[
      StringMatchQ[case, "Test410895[(*\"*)]"],
        Print["double-quotes are kept; bug 410895 is NOT present; NO work-around needed"];
      ,
      StringMatchQ[case, "Test410895[(**)]"],
        Print["double-quotes are stripped; bug 410895 IS present; work-around IS needed"];

        $WorkaroundBug410895 = True;
    ];

    Print["Testing bug 410895 was successful."];

    (*
    Exit[]
    *)
    Print["Exiting..."];
    res = binaryWrite[proc, "Exit[0]\r\n"];

    If[FailureQ[res],
      Throw[res]
    ];

    Pause[2.0];

    If[ProcessStatus[proc] != "Finished",
      exitHard[proc, "ERROR: Mini Server kernel is not finished; exiting hard"];
      Throw[$Failed]
    ];

    Print["Exiting was successful."];

    TaskRemove[$timeoutTask];
    
    code = ProcessInformation[proc, "ExitCode"];

    diagnoseExitCode[code];

    Print["Done Stage 1"];
  ]]


doStage2[commandIn_, cwd_] :=
  Catch[
  Module[{command, runPosition, run, startServerString, startServer,
    startServerArgs, toTest, serverStartTime, proc, assoc, bytes, len,
    str, cases, case, lenStr, contentStr, content,
    serverInitializeTime, res, serverKernel, kernelVersion,
    commandLine, directory, maxLicenseProcesses, lspServerVersion,
    codeParserVersion, codeInspectorVersion, codeFormatterVersion,
    lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate,
    codeFormatterBuildDate, code},

    command = commandIn;

    Print[];
    Print["Start Stage 2"];
    Print["Now running actual diagnostics..."];
    Print["$WorkaroundBug410895: ", $WorkaroundBug410895];

    If[!MemberQ[command, "-noinit"],
      Print["WARNING: -noinit is not in command"];
    ];
    If[!MemberQ[command, "-noprompt"],
      Print["ERROR: -noprompt is not in command"];
      Throw[$Failed]
    ];
    If[!MemberQ[command, "-nopaclet"],
      Print["WARNING: -nopaclet is not in command"];
    ];
    If[!MemberQ[command, "-noicon"],
      Print["ERROR: -noicon is not in command"];
      Throw[$Failed]
    ];
    If[!MemberQ[command, "-nostartuppaclets"],
      Print["WARNING: -nostartuppaclets is not in command"];
    ];
    If[!MemberQ[command, "-run"],
      Print["ERROR: -run is not in command"];
      Throw[$Failed]
    ];


    runPosition = FirstPosition[command, "-run"];

    If[!MissingQ[runPosition],
      run = command[[runPosition[[1]] + 1]];

      If[StringQ[run],
        If[!SyntaxQ[run],
          Print["ERROR: code is not SyntaxQ: ", run];
          Throw[$Failed]
        ];
        ,
        Print["ERROR: code is not a string: ", run];
        Throw[$Failed]
      ];

      startServerString = StringCases[run, ss:("StartServer[" ~~ ___ ~~ "]" ~~ EndOfString) :> ss];
      If[Length[startServerString] == 1,
        startServerString = startServerString[[1]];
        
        Block[{StartServer},
          startServer = ToExpression[startServerString];
          startServerArgs = startServer /. StartServer[logDir_String : "", opts:OptionsPattern[]] :> {logDir, {opts}};
          
          If[(Global`CommunicationMethod /. startServerArgs[[2]]) == "Socket",
            Print["ERROR: CommunicationMethod \"Socket\" not implemented for RunServerDiagnostic", run];
            Throw[$Failed]
          ]
        ]

      ];

      If[TrueQ[$WorkaroundBug410895],
        (*
        work around bug 410895, all quotes are stripped from StartProcess on Windows

        this was fixed in 13.0
        
        convert e.g., Print["Foo`"] into ToExpression[FromCharacterCode[{80, 114, 105, 110, 116, 91, 34, 70, 111, 111, 96, 34, 93}]]

        evaluates the same expr, except that the only characters passed on command-line are letters, digits, space, comma, [] and {}
        *)
        run = "ToExpression[FromCharacterCode[" <> ToString[ToCharacterCode[run]] <> "]]";

        command[[runPosition[[1]] + 1]] = run;
      ];
      ,
      (*
      already WARNED about missing -run
      *)
      Throw[$Failed]
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
    proc = Quiet[StartProcess[command, ProcessDirectory -> cwd], {StartProcess::pnfd}];

    If[FailureQ[proc],
      Print["ERROR: StartProcess failed"];
      Throw[proc]
    ];

    (*
    Only kill process here
    Do not Print anything, Print output in a task will go to Messages Window
    *)
    $timeoutTask = SessionSubmit[ScheduledTask[$timeout = True; KillProcess[proc], {Quantity[30, "Seconds"], 1}]];

    Print["Waiting maximum of 30 seconds for any hangs."];


    (*
    initialize
    *)
    assoc = <| "method" -> "initialize", "id" -> 1, "params" -> <|
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

    Print["Writing initialize message..."];
    res = binaryWrite[proc, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];
   
    If[FailureQ[res],
      Throw[res]
    ];

    res = binaryWrite[proc, bytes];

    If[FailureQ[res],
      Throw[res]
    ];

    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      exitHard[proc, "ERROR: Language Server kernel is not running after writing initialize; exiting hard"];
      Throw[$Failed]
    ];

    str = readByteArray[proc, str];

    If[FailureQ[str],
      Throw[str]
    ];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from Language Server kernel:"];
      Print[str];
      diagnoseStdOut[str];

      exitHard[proc, "ERROR: Unrecognized header; exiting hard"];
      Throw[$Failed]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[!IntegerQ[len],
      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Bad Content-Length; exiting hard"];
      Throw[$Failed]
    ];

    If[!(len <= StringLength[contentStr]),

      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Bad Content-Length; exiting hard"];
      Throw[$Failed]
    ];

    str = StringDrop[contentStr, len];

    serverInitializeTime = Now;

    Print["initialize message was successful."];


    (*
    diagnostics
    *)
    assoc = <| "method" -> "diagnostics", "id" -> 2 |>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing diagnostics message..."];
    res = binaryWrite[proc, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];

    If[FailureQ[res],
      Throw[res]
    ];

    res = binaryWrite[proc, bytes];

    If[FailureQ[res],
      Throw[res]
    ];
    
    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      exitHard[proc, "ERROR: Language Server kernel is not running after writing diagnostics; exiting hard"];
      Throw[$Failed]
    ];

    str = readByteArray[proc, str];

    If[FailureQ[str],
      Throw[str]
    ];

    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Unrecognized header; exiting hard"];
      Throw[$Failed]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[!IntegerQ[len],

      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Bad Content-Length; exiting hard"];
      Throw[$Failed]
    ];

    If[!(len <= StringLength[contentStr]),

      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Bad Content-Length; exiting hard"];
      Throw[$Failed]
    ];

    str = StringDrop[contentStr, len];
    contentStr = StringTake[contentStr, len];

    content = ImportString[contentStr, "RawJSON"];

    res = content["result"];

    serverKernel = command[[1]];
    kernelVersion = res["kernelVersion"];
    commandLine = res["commandLine"];
    directory = res["directory"];
    maxLicenseProcesses = res["maxLicenseProcesses"];
    lspServerVersion = res["lspServerVersion"];
    codeParserVersion = res["codeParserVersion"];
    codeInspectorVersion = res["codeInspectorVersion"];
    codeFormatterVersion = res["codeFormatterVersion"];

    lspServerBuildDate = res["lspServerBuildDate"];
    codeParserBuildDate = res["codeParserBuildDate"];
    codeInspectorBuildDate = res["codeInspectorBuildDate"];
    codeFormatterBuildDate = res["codeFormatterBuildDate"];

    Print["INFORMATION: Server Kernel: ", serverKernel];
    If[!StringStartsQ[ToLowerCase[FileBaseName[serverKernel]], "wolframkernel"],
      Print["WARNING: Server Kernel does not start with 'WolframKernel': ", serverKernel];
    ];
    Print["INFORMATION: Server Kernel $CommandLine //InputForm: ", commandLine //InputForm];
    Print["INFORMATION: Server Kernel Directory[]: ", directory];
    Print["INFORMATION: Server Kernel $VersionNumber: ", kernelVersion];
    Print["INFORMATION: Server Kernel $MaxLicenseProcesses: ", maxLicenseProcesses];
    Print["INFORMATION: Server Kernel LSPServer version: ", lspServerVersion];
    Print["INFORMATION: Server Kernel CodeParser version: ", codeParserVersion];
    Print["INFORMATION: Server Kernel CodeInspector version: ", codeInspectorVersion];
    Print["INFORMATION: Server Kernel CodeFormatter version: ", codeFormatterVersion];
    Print["INFORMATION: Server Kernel LSPServer build date: ", lspServerBuildDate];
    Print["INFORMATION: Server Kernel CodeParser build date: ", codeParserBuildDate];
    Print["INFORMATION: Server Kernel CodeInspector build date: ", codeInspectorBuildDate];
    Print["INFORMATION: Server Kernel CodeFormatter build date: ", codeFormatterBuildDate];

    checkWarnings[
      {kernelVersion, lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion},
      {lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate}
      ,
      (Print["WARNING: ", #];)&
    ];

    Print["diagnostics message was successful."];


    (*
    shutdown
    *)
    assoc = <| "method" -> "shutdown", "id" -> 3 |>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing shutdown message..."];
    res = binaryWrite[proc, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];

    If[FailureQ[res],
      Throw[res]
    ];

    res = binaryWrite[proc, bytes];

    If[FailureQ[res],
      Throw[res]
    ];

    Pause[0.2];

    If[ProcessStatus[proc] != "Running",
      exitHard[proc, "ERROR: Language Server kernel is not running after writing shutdown; exiting hard"];
      Throw[$Failed]
    ];

    str = readByteArray[proc, str];

    If[FailureQ[str],
      Throw[str]
    ];
   
    If[(cases = StringCases[str, RegularExpression["(?s)^Content-Length: (\\d+)\r\n\r\n(.*)$"] :> {"$1", "$2"}]) == {},

      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Unrecognized header; exiting hard"];
      Throw[$Failed]
    ];

    case = cases[[1]];
    {lenStr, contentStr} = case;
    len = ToExpression[lenStr];

    If[!IntegerQ[len],

      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Bad Content-Length; exiting hard"];
      Throw[$Failed]
    ];

    If[!(len <= StringLength[contentStr]),

      Print["Read from Language Server kernel:"];
      Print[str];

      exitHard[proc, "ERROR: Bad Content-Length; exiting hard"];
      Throw[$Failed]
    ];
    
    str = StringDrop[contentStr, len];
    contentStr = StringTake[contentStr, len];

    Print["shutdown message was successful."];


    (*
    exit
    *)
    assoc = <| "method" -> "exit" |>;
    bytes = ExportByteArray[assoc, "JSON"];
    len = Length[bytes];

    Print["Writing exit message..."];
    res = binaryWrite[proc, "Content-Length: " <> ToString[len] <> "\r\n\r\n"];

    If[FailureQ[res],
      Throw[res]
    ];

    res = binaryWrite[proc, bytes];

    If[FailureQ[res],
      Throw[res]
    ];

    Pause[2.0];

    If[ProcessStatus[proc] != "Finished",
      exitHard[proc, "ERROR: Language Server kernel is not finished; exiting hard"];
      Throw[$Failed]
    ];

    Print["exit message was successful."];

    TaskRemove[$timeoutTask];

    Print["INFO: Time to initialize server: ", (serverInitializeTime - serverStartTime)];

    If[(serverInitializeTime - serverStartTime) > Quantity[15, "Seconds"],
      Print["ERROR: Time to initialize server was greater than 15 seconds"];
      Throw[$Failed]
    ];

    code = ProcessInformation[proc, "ExitCode"];

    diagnoseExitCode[code];

    Print["Done Stage 2"];
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
  Module[{id, kernelVersion, commandLine, directory, maxLicenseProcesses, lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion, diags,
    lspServerBuildDate, codeParserBuildDate, codeInspectorBuildDate, codeFormatterBuildDate},

    If[$Debug2,
      log["diagnostics: enter"]
    ];

    id = content["id"];

    kernelVersion = $VersionNumber;

    commandLine = $CommandLine;

    directory = Directory[];

    maxLicenseProcesses = $MaxLicenseProcesses;

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
      "commandLine" -> commandLine,
      "directory" -> directory,
      "maxLicenseProcesses" -> maxLicenseProcesses,
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


exitHard[proc_, msg_] :=
Catch[
Module[{code},

  If[$timeout,
    Print["INFO: Process timed out after 30 seconds."];

    Print[msg];

    code = ProcessInformation[proc, "ExitCode"];
    
    diagnoseExitCode[code];

    Throw[Null]
  ];

  TaskRemove[$timeoutTask];

  Print[msg];

  reportStdOut[proc];
  reportStdErr[proc];

  If[ProcessStatus[proc] == "Finished",
    
    Print["Process already finished."];

    code = ProcessInformation[proc, "ExitCode"];

    diagnoseExitCode[code];
    
    Throw[Null]
  ];

  Print["Killing process."];
  KillProcess[proc];

  code = ProcessInformation[proc, "ExitCode"];
  
  diagnoseExitCode[code];
]]


(*
This reports license errors
*)
reportStdOut[proc_] :=
  Module[{stdOut, arr, str},
    stdOut = ProcessConnection[proc, "StandardOutput"];

    While[True,
      Pause[0.1];
      
      arr = Quiet[ReadByteArray[stdOut, EndOfBuffer], {ReadByteArray::openx}];

      Which[
        arr === {},
          Print["INFO: stdout: (empty)"];
        ,
        ByteArrayQ[arr],
          str = ByteArrayToString[arr];
          Print["INFO: stdout: ", str];
        ,
        arr === EndOfFile,
          Break[]
        ,
        MatchQ[arr, _ReadByteArray],
          Print["ERROR: stdout: << ReadByteArray returned unevaluated >>"];
          Break[]
        ,
        True,
          Print["ERROR: stdout: ", arr];
          Break[]
      ]
    ]
  ]

reportStdErr[proc_] :=
  Module[{stdErr, arr, str},
    stdErr = ProcessConnection[proc, "StandardError"];

    While[True,
      Pause[0.1];

      arr = Quiet[ReadByteArray[stdErr, EndOfBuffer], {ReadByteArray::openx}];

      Which[
        arr === {},
          Print["INFO: stderr: (empty)"];
        ,
        ByteArrayQ[arr],
          str = ByteArrayToString[arr];
          Print["INFO: stderr: ", str];
        ,
        arr === EndOfFile,
          Break[]
        ,
        MatchQ[arr, _ReadByteArray],
          Print["ERROR: stderr: << ReadByteArray returned unevaluated >>"];
          Break[]
        ,
        True,
          Print["ERROR: stderr: ", arr];
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


diagnoseExitCode[code_] :=
Module[{},
  Switch[code,
    85,
      Print["INFO: Exit code: 85 (usually indicates kernel license error)"];
    ,
    137,
      Print["INFO: Exit code: 137 (usually indicates that the process was killed)"];
    ,
    _,
      Print["INFO: Exit code: ", code];
  ];
]


readByteArray[proc_, strIn_] :=
Catch[
Module[{str, stdOut, bytes},

  str = strIn;

  stdOut = ProcessConnection[proc, "StandardOutput"];

  (*
  it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
  *)
  bytes = Quiet[ReadByteArray[stdOut, EndOfBuffer], {ReadByteArray::openx}];

  If[bytes === EndOfFile,
    exitHard[proc, "ERROR: Unexpected EndOfFile; exiting hard"];
    Throw[$Failed]
  ];
  
  If[MatchQ[bytes, _ReadByteArray],
    exitHard[proc, "ERROR: ReadByteArray returned unevaluated; exiting hard"];
    Throw[$Failed]
  ];
  
  While[bytes === {} && str == "",

    Pause[0.1];
    
    bytes = Quiet[ReadByteArray[stdOut, EndOfBuffer], {ReadByteArray::openx}];
    
    If[bytes === EndOfFile,
      exitHard[proc, "ERROR: Unexpected EndOfFile; exiting hard"];
      Throw[$Failed]
    ];
    
    If[MatchQ[bytes, _ReadByteArray],
      exitHard[proc, "ERROR: ReadByteArray returned unevaluated; exiting hard"];
      Throw[$Failed]
    ];
  ];
  
  str = str <> ByteArrayToString[bytes];
  
  (*
  Do one more read after sufficient time
  *)
  
  Pause[0.2];
  
  bytes = Quiet[ReadByteArray[stdOut, EndOfBuffer], {ReadByteArray::openx}];
  
  If[bytes === EndOfFile,
    exitHard[proc, "ERROR: Unexpected EndOfFile; exiting hard"];
    Throw[$Failed]
  ];
  
  If[MatchQ[bytes, _ReadByteArray],
    exitHard[proc, "ERROR: ReadByteArray returned unevaluated; exiting hard"];
    Throw[$Failed]
  ];
  
  str = str <> ByteArrayToString[bytes];

  str
]]


binaryWrite[proc_, input_] :=
Catch[
Module[{stdIn, res},

  stdIn = ProcessConnection[proc, "StandardInput"];

  res = Quiet[BinaryWrite[stdIn, input], {BinaryWrite::errfile}];
  
  If[FailureQ[res],
    exitHard[proc, "ERROR: BinaryWrite failed; exiting hard"];
    Throw[$Failed]
  ];

  res
]]


StartMiniServer[] :=
Catch[
Module[{startupError, e, line},

  startupError = GetStartupError[];

  If[startupError =!= 0,
    Pause[1];Exit[1]
  ];

  While[True,

    line = ReadLineFromStdIn[];
 
    e = ToExpression[line];
 
    line = ToString[e];
 
    WriteLineToStdOut[line];
  ]
]]

End[]

EndPackage[]
