BeginPackage["LSPServer`ServerDiagnostics`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]


RunServerDiagnostic[command:{_String...}] :=
  Catch[
  Module[{proc, stdIn, stdOut, assoc, bytes, str, cases, case, len, content, lenStr},

    Print["Running Language Server diagnostic..."];

    If[!StringStartsQ[ToLowerCase[FileBaseName[command[[1]]]], "wolframkernel"],
      Print["WARNING: Command for Wolfram Language Server does not start with 'WolframKernel': ", command[[1]]];
    ];
    If[!MemberQ[command, "-noinit"],
      Print["WARNING: -noinit is not in command"];
    ];
    If[!MemberQ[command, "-noprompt"],
      Print["WARNING: -noprompt is not in command"];
    ];
    If[!MemberQ[command, "-nopaclet"],
      Print["WARNING: -nopaclet is not in command"];
    ];
    If[!MemberQ[command, "-noicon"],
      Print["WARNING: -noicon is not in command"];
    ];


    If[!SyntaxQ[command[[-1]]],
      Print["ERROR: code is not SyntaxQ: ", command[[-1]]];
      Throw[False]
    ];


    Print["Starting Language Server kernel with command: ", command];
    proc = StartProcess[command];

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

    Print["initialize was successful."];


    (*
    shutdown
    *)
    assoc = <|"method" -> "shutdown", "id" -> 2|>;
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


exitHard[proc_] :=
  Module[{},

    reportStdErr[proc];

    Print["Killing process"];
    KillProcess[proc];

    False
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


End[]

EndPackage[]
