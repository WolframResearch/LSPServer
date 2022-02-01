(* ::Package::"Tags"-><|"NoVariables" -> <|"Module" -> <|Enabled -> False|>|>|>:: *)

BeginPackage["LSPServer`Workspace`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


handleContent[content:KeyValuePattern["method" -> "workspace/executeCommand"]] :=
Catch[
Module[{params, id, command},

  If[$Debug2,
    log["workspace/executeCommand: enter"]
  ];

  id = content["id"];

  If[Lookup[$CancelMap, id, False],

    $CancelMap[id] =.;

    If[$Debug2,
      log["$CancelMap: ", $CancelMap]
    ];

    Throw[{<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>}]
  ];
  
  params = content["params"];
  command = params["command"];

  Switch[command,
    (*
    enable_bracket_matcher_debug_mode is an undocumented debug command
    *)
    "enable_bracket_matcher_debug_mode",
      $DebugBracketMatcher = True;
      (*
      TODO: trigger bracket matcher here
      *)
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    disable_bracket_matcher_debug_mode is an undocumented debug command
    *)
    "disable_bracket_matcher_debug_mode",
      $DebugBracketMatcher = False;
      (*
      TODO: trigger bracket matcher here
      *)
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    enable_bracket_matcher_design_colors is an undocumented debug command
    *)
    "enable_bracket_matcher_design_colors",
      $BracketMatcherUseDesignColors = True;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    disable_bracket_matcher_design_colors is an undocumented debug command
    *)
    "disable_bracket_matcher_design_colors",
      $BracketMatcherUseDesignColors = False;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    enable_bracket_matcher_display_insertion_text is an undocumented debug command
    *)
    "enable_bracket_matcher_display_insertion_text",
      $BracketMatcherDisplayInsertionText = True;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    disable_bracket_matcher_display_insertion_text is an undocumented debug command
    *)
    "disable_bracket_matcher_display_insertion_text",
      $BracketMatcherDisplayInsertionText = False;
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (*
    roundtrip_responsiveness_test is an undocumented, debug command
    *)
    "roundtrip_responsiveness_test",

      If[$Debug2, log["roundtrip_responsiveness_test:> \n\n"]];
      If[$Debug2, log[DateString[Now, {"Year", "-", "Month", "-", "Day", "_", "Hour24", "-", "Minute", "-", "Second", "-", "Millisecond"}]]];

      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <|"method" -> "roundTripTest"|>}
    ,
    (*
    ping_pong_responsiveness_test is an undocumented, debug command
    *)
    "ping_pong_responsiveness_test",

      If[$Debug2, log["ping_pong_responsiveness_test:> \n\n"]];

      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <|"method" -> "pingPongTest"|>}    
    ,
    (*
    payload_responsiveness_test is an undocumented, debug command
    *)
    "payload_responsiveness_test",

      If[$Debug2, log["payload_responsiveness_test:> \n\n"]];

      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>, <|"method" -> "payloadTest", "payload" -> StringJoin@Flatten@Table[CharacterRange["a", "z"], 100000]|>}
    ,
    _,
      If[$Debug,
        log["UNSUPPORTED COMMAND: ", command]
      ];
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
  ]
]]


handleContent[content:KeyValuePattern["method" -> "workspace/didChangeWatchedFiles"]] :=
Module[{},

  If[$Debug2,
    log["workspace/didChangeWatchedFiles: enter"]
  ];

  {}
]

handleContent[content:KeyValuePattern["method" -> "workspace/didChangeConfiguration"]] :=
Module[{},

  If[$Debug2,
    log["workspace/didChangeConfiguration: enter"]
  ];

  {}
]


End[]

EndPackage[]
