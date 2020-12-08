BeginPackage["LSPServer`Workspace`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


handleContent[content:KeyValuePattern["method" -> "workspace/executeCommand"]] :=
Catch[
Module[{params, id, command},

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
    "enable_bracket_matcher_debug_mode",
      $DebugBracketMatcher = True;
      (*
      TODO: trigger bracket matcher here
      *)
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    "disable_bracket_matcher_debug_mode",
      $DebugBracketMatcher = False;
      (*
      TODO: trigger bracket matcher here
      *)
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
    ,
    (* "enable_bracket_matcher_design_colors",
      $BracketMatcherUseDesignColors = True
    ,
    "disable_bracket_matcher_design_colors",
      $BracketMatcherUseDesignColors = False
    , *)
    (* "enable_bracket_matcher_display_insertion_text",
      $BracketMatcherDisplayInsertionText = True
    ,
    "disable_bracket_matcher_display_insertion_text",
      $BracketMatcherDisplayInsertionText = False
    , *)
    _,
      If[$Debug,
        log["UNSUPPORTED COMMAND: ", command]
      ];
      {<| "jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>}
  ]
]]


handleContent[content:KeyValuePattern["method" -> "workspace/didChangeWatchedFiles"]] :=
  Module[{},
    {}
  ]


End[]

EndPackage[]
