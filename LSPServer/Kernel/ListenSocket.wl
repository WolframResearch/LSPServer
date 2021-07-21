BeginPackage["LSPServer`ListenSocket`"]


Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`Utils`"]
Needs["LSPServer`Library`"]
Needs["LSPServer`Socket`"]

(* ========================================================== *)
(* ==============   SocketListen functions   ================ *)
(* ========================================================== *)


(* =================   Initialize   ======================= *)


initializeLSPComm["ListenSocket"] := SocketOpen[5555, "TCP"]




(* Call-back function for SocketListen *)

processData[dataByteArray_, sourceSocket_] := 
Module[{dataString, finalMsg, contentsIn, content, contents},

  If[!ByteArrayQ[dataByteArray],

    log["\n\n"];
    log["invalid ByteArray: ", dataByteArray];
    log["\n\n"];

    exitHard[]
  ];

  dataString = ByteArrayToString @ dataByteArray;

  finalMsg = findMessageParts[lspMsgAssoc["msgInQueue"] <> dataString];

  contentsIn = {Developer`ReadRawJSONString[lspMsgAssoc["lspMsg"]]};

  expandUpdate[contentsIn];

  ProcessScheduledJobs[];


  If[empty[$ContentQueue],
    Pause[0.1];
    Continue[]
  ];

  While[$ContentQueue =!= {},
    content = $ContentQueue[[1]];
    $ContentQueue = Rest[$ContentQueue];

    If[$Debug2,
      log["taking first from $ContentQueue: ", #["method"]&[content]];
      log["rest of $ContentQueue (up to 20): ", Take[#["method"]& /@ $ContentQueue, UpTo[20]]];
      log["..."]
    ];

    contents = LSPEvaluate[content];

    (* write out evaluated results to the client *)

    writeLSPResult["Socket", sourceSocket, contents];

  ];


];

readEvalWriteLoop["ListenSocket", sock_]:= SocketListen[sock, processData[#DataByteArray, #SourceSocket]&, HandlerFunctionsKeys -> {"DataByteArray", "SourceSocket"}];

(* ============================ ShutDown ============================= *)
shutdownLSPComm["ListenSocket", s_SocketObject]:= Close[s];
shutdownLSPComm["ListenSocket", _]:= Null;

End[]

EndPackage[]

