BeginPackage["LSPServer`Socket`"]



findMessageParts
lspMsgAssoc
writeLSPResult

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`Utils`"]
Needs["LSPServer`Library`"]

(* ========================================================== *)
(* ================   Socket functions   ==================== *)
(* ========================================================== *)

lspMsgAssoc = <|"lspMsg" -> "", "msgInQueue" -> ""|>;

(* =================   Initialize   ======================= *)

(* "tcp_mode": "host", "tcp_port": 5555 *)

initializeLSPComm["Socket"] := SocketConnect[5555, "TCP"]

(* ===============   Message validity check    =============== *)

checkContent[str_] := StringContainsQ[str, "Content-Length: "];

checkStartPosition[str_] := First @ Flatten @ StringPosition[str, "Content"] === 1;

checkMsgLength[str_] :=
  Module[{numStrs, pos, requiredLength, maxMsgLength},
    (* Position of the header *)
    numStrs = StringCases[str, "Content-Length: " ~~ length:NumberString :> length];
    requiredLength = ToExpression[First[numStrs]];
    (* Extract the header*)
    pos = StringPosition[str, "Content-Length: " <> # <> "\r\n\r\n"]& /@ numStrs // Flatten;
    maxMsgLength = StringLength @ StringTake[str, {pos[[2]] + 1, StringLength[str]}];

    maxMsgLength >= requiredLength
  ];

msgContainsQ[str_] :=
  Module[{res},
    If[checkContent[str],
      If[checkStartPosition[str] && checkMsgLength[str],
        res = True;
        ,
        res = False;
      ]
      ,
      res = False;
    ];
    res
  ]

findMessageParts[str_] :=
  Module[{numStrs, msgLength, headerPosition},
    If[checkContent[lspMsgAssoc["msgInQueue"] <> str],
      If[checkStartPosition[str] && checkMsgLength[str],
        numStrs = First @ StringCases[str, "Content-Length: " ~~ length:NumberString :> length];
        msgLength = ToExpression[numStrs];
        headerPosition = Flatten @ StringPosition[str, "Content-Length: " <> numStrs <> "\r\n\r\n"];

        lspMsgAssoc["lspMsg"]   = StringTake[str, {headerPosition[[2]] + 1, headerPosition[[2]] + msgLength}];
        lspMsgAssoc["msgInQueue"] = StringTake[str, {headerPosition[[2]] + 1 + msgLength, StringLength[str]}];
        ,
        lspMsgAssoc["msgInQueue"] = lspMsgAssoc["msgInQueue"] <> str;
      ]
      ,
      lspMsgAssoc["msgInQueue"] = lspMsgAssoc["msgInQueue"] <> str;
    ];
    lspMsgAssoc
  ];




(* ===================     Read     ========================= *)

queueEmptyQ["Socket"] :=
  Module[{getMethods},
    getMethods = #["method"]& /@ $ContentQueue;
    MatchQ[getMethods, {}]
  ];

readMessage["Socket", sockObj_] :=
  Module[{sockMessage, lspMsgEmptyQ},
    (* First check if a valid msg is available in the que *)
    (* When a valid message is available in the queue: No need to read new message from socket *)
    If[msgContainsQ[lspMsgAssoc["msgInQueue"]],

      If[$Debug2,
        log["Valid message is availble in the MessageQueue :> "];
        log["\nmsgInQueue :> \n"];
        log[InputForm[lspMsgAssoc["msgInQueue"]]];
      ];
      findMessageParts[lspMsgAssoc["msgInQueue"]];
      ,

      (* When no valid message is available in the queue: read from socket *)
      If[$Debug2,
        log["No valid message is availble in the MessageQueue :> "];
        log["\nmsgInQueue :> \n"];
        log[InputForm[lspMsgAssoc["msgInQueue"]]];
      ];

      lspMsgEmptyQ = True;
      While[lspMsgEmptyQ,
        sockMessage = SocketReadMessage[sockObj];

        If[FailureQ[sockMessage],
          If[$Debug2,
            log["Read failure from client.\n"]
          ];
          exitHard[]
        ];

        sockMessage = ByteArrayToString[sockMessage];
        lspMsgEmptyQ = Not @ msgContainsQ @ (lspMsgAssoc["msgInQueue"] <> sockMessage);
        (* Extract a valid message from the queue and update  lspMsgAssoc *)
        findMessageParts[lspMsgAssoc["msgInQueue"] <> sockMessage];
      ];

    ];

    {Developer`ReadRawJSONString[lspMsgAssoc["lspMsg"]]}
  ];

(* Read + Expand + Update *)

TryQueue["Socket", sockObj_] :=
  Catch[
    Module[{contentsIn},
      If[queueEmptyQ["Socket"],
        If[$Debug2,
          log["\n\n $ContentQueue is empty, No more messages to deliver, Go to Read messages Section.\n\n "];
        ]
        ,
        If[$Debug2,
          log["\n\n $ContentQueue is not empty, No Read messages.\n\n "];
        ];
        Throw[Null]
      ];
      contentsIn = readMessage["Socket", sockObj];
      expandUpdate[contentsIn];
      Null
    ]
  ]




(* ===================     Write     ======================== *)


writeSocket["Socket", socket_, header_, body_] :=
  Module[{headerWrite, bodyWrite},
  
    headerWrite = BinaryWrite[socket, StringToByteArray @ header];

    If[
      FailureQ[headerWrite],
      If[$Debug2, log["Message-header write failure to client."]];
      exitHard[]
    ];

    bodyWrite = BinaryWrite[socket, body];

    If[
      FailureQ[bodyWrite],
      If[$Debug2, log["Message-body write failure to client."]];
      exitHard[]
    ]
  ];

(* contents is a list of Associations *)
writeLSPResult["Socket", sockObject_, contents_] :=
  Module[{bytess, line, bytes},

    Check[
      bytess = StringToByteArray[Developer`WriteRawJSONString[#]]& /@ contents

      ,
      log["\n\n"];
      log["message generated by contents: ", contents];
      log["\n\n"]
      ,
      {Export::jsonstrictencoding}
    ];


    (*
    write out each byte array in bytess
    *)
    Do[
      If[!ByteArrayQ[bytes],
        log["\n\n"];
        log["invalid bytes: ", bytes];
        log["\n\n"];
        exitHard[]
      ];
      line = "Content-Length: " <> ToString[Length[bytes]] <> "\r\n\r\n";
      writeSocket["Socket", sockObject, line, bytes];
      ,
      {bytes, bytess}
    ](*Do bytess*)
  ];


  (* ================= Read Write Loop =============================== *)
readEvalWriteLoop["Socket", sock_]:= 
Module[{content, contents},
  While[True,

    TryQueue["Socket", sock];

    ProcessScheduledJobs[];

    If[empty[$ContentQueue],
      Pause[0.1];
      Continue[]
    ];

    content = $ContentQueue[[1]];
    $ContentQueue = Rest[$ContentQueue];

    If[$Debug2,
      log["taking first from $ContentQueue: ", #["method"]&[content]];
      log["rest of $ContentQueue (up to 20): ", Take[#["method"]& /@ $ContentQueue, UpTo[20]]];
      log["..."]
    ];

    contents = LSPEvaluate[content];

    (* write out evaluated results to the client *)
    writeLSPResult["Socket", sock, contents];

  ](*While*)
];

(* ============================ ShutDown ============================= *)
shutdownLSPComm["Socket", s_SocketObject]:= Close[s];
shutdownLSPComm["Socket", _]:= Null;

End[]

EndPackage[]