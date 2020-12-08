BeginPackage["LSPServer`ServerDiagnostics`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]


RunServerDiagnostic[command:{_String...}] :=
  Catch[
  Module[{proc, stdIn, stdOut, assoc, bytes, str, cases, case, len, content, lenStr, runPosition, run, toTest,
    lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion,
    contentStr, res},

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
      Print["ERROR: Language Server kernel is not running; exiting hard"];
      Throw[exitHard[proc]]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    While[bytes === {},
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
    ];
    str = "";
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
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
      Print["ERROR: Language Server kernel is not running; exiting hard"];
      Throw[exitHard[proc]]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    While[bytes === {},
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
    ];
    str = "";
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
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

    lspServerVersion = res["lspServerVersion"];
    codeParserVersion = res["codeParserVersion"];
    codeInspectorVersion = res["codeInspectorVersion"];
    codeFormatterVersion = res["codeFormatterVersion"];

    Print["INFORMATION: LSPServer version: ", lspServerVersion];
    Print["INFORMATION: CodeParser version: ", codeParserVersion];
    Print["INFORMATION: CodeInspector version: ", codeInspectorVersion];
    Print["INFORMATION: CodeFormatter version: ", codeFormatterVersion];

    If[lspServerVersion =!= codeParserVersion,
      Print["WARNING: LSPServer and CodeParser do not have the same version."];
    ];
    If[lspServerVersion =!= codeInspectorVersion,
      Print["WARNING: LSPServer and CodeInspector do not have the same version."];
    ];
    If[lspServerVersion =!= codeFormatterVersion,
      Print["WARNING: LSPServer and CodeFormatter do not have the same version."];
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
      Print["ERROR: Language Server kernel is not running; exiting hard"];
      Throw[exitHard[proc]]
    ];

    (*
    it is a property of ProcessLink that ReadByteArray[stdOut, EndOfBuffer] will return {} if there is no content yet
    *)
    bytes = ReadByteArray[stdOut, EndOfBuffer];
    While[bytes === {},
      Pause[0.1];
      bytes = ReadByteArray[stdOut, EndOfBuffer];
    ];
    str = "";
    str = str <> ByteArrayToString[bytes];
    (*
    Do one more read after sufficient time
    *)
    Pause[0.2];
    bytes = ReadByteArray[stdOut, EndOfBuffer];
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


handleContent[content:KeyValuePattern["method" -> "diagnostics"]] :=
  Catch[
  Module[{id, lspServerVersion, codeParserVersion, codeInspectorVersion, codeFormatterVersion, diags},

    If[$Debug2,
      log["diagnostics: enter"]
    ];

    id = content["id"];

    lspServerVersion = Information[PacletObject["LSPServer"], "Version"];

    codeParserVersion = Information[PacletObject["CodeParser"], "Version"];

    codeInspectorVersion = Information[PacletObject["CodeInspector"], "Version"];

    codeFormatterVersion = Information[PacletObject["CodeFormatter"], "Version"];

    diags = <|
      "lspServerVersion" -> lspServerVersion,
      "codeParserVersion" -> codeParserVersion,
      "codeInspectorVersion" -> codeInspectorVersion,
      "codeFormatterVersion" -> codeFormatterVersion
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
