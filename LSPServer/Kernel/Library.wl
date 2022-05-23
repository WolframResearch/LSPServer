BeginPackage["LSPServer`Library`"]

SetDebug

GetStartupError

StartBackgroundReaderThread

LockQueue

UnlockQueue

GetQueueSize

GetFrontMessageSize

PopQueue

GetStdInFEOF

GetStdInFError

GetStdOutFEOF

GetStdOutFError

ReadLineFromStdIn

WriteLineToStdOut

WriteBytesToStdOut


$LSPServerLibraryError


loadAllFuncs


Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]


$LSPServerLibraryError = <|
  "FREAD_FAILED" -> 1,
  "UNEXPECTED_LINEFEED" -> 2,
  "EXPECTED_LINEFEED" -> 3,
  "UNRECOGNIZED_HEADER" -> 4,
  "FWRITE_FAILED" -> 5,
  "FFLUSH_FAILED" -> 6
|>


SetDebug[level_] :=
Module[{res},
  res = libraryFunctionWrapper[setDebug, level];
  res
]

GetStartupError[] :=
Module[{res},
  res = libraryFunctionWrapper[getStartupError];
  res
]

StartBackgroundReaderThread[] :=
Module[{res},
  res = libraryFunctionWrapper[startBackgroundReaderThreadFunc];
  res
]

LockQueue[] :=
Module[{res},
  If[$Debug3,
    Print["toplevel: LockQueue before"];
  ];
  res = libraryFunctionWrapper[lockQueueFunc];
  If[$Debug3,
    Print["toplevel: LockQueue after"];
  ];
  res
]

UnlockQueue[] :=
Module[{res},
  If[$Debug3,
    Print["toplevel: UnlockQueue before"];
  ];
  res = libraryFunctionWrapper[unlockQueueFunc];
  If[$Debug3,
    Print["toplevel: UnlockQueue after"];
  ];
  res
]

GetQueueSize[] :=
Module[{res},
  res = libraryFunctionWrapper[getQueueSizeFunc];
  res
]

GetFrontMessageSize[] :=
Module[{res},
  res = libraryFunctionWrapper[getFrontMessageSizeFunc];
  res
]

PopQueue[0] :=
  Failure["PopQueue[0]", <||>]

PopQueue[numBytes_Integer] :=
Module[{bytes},
  bytes = ByteArray[Developer`AllocateNumericArray["UnsignedInteger8", {numBytes}]];
  libraryFunctionWrapper[popQueueFunc, bytes];
  bytes
]

GetStdInFEOF[] :=
Module[{res},
  res = libraryFunctionWrapper[getStdInFEOF];
  res
]

GetStdInFError[] :=
Module[{res},
  res = libraryFunctionWrapper[getStdInFError];
  res
]

GetStdOutFEOF[] :=
Module[{res},
  res = libraryFunctionWrapper[getStdOutFEOF];
  res
]

GetStdOutFError[] :=
Module[{res},
  res = libraryFunctionWrapper[getStdOutFError];
  res
]

ReadLineFromStdIn[] :=
Module[{res},
  res = libraryFunctionWrapper[readLineFromStdInFunc];
  res
]

WriteLineToStdOut[line_String] :=
Module[{res},
  res = libraryFunctionWrapper[writeLineToStdOutFunc, line];
  res
]

WriteBytesToStdOut[bytes_ByteArray] :=
Module[{res},
  res = libraryFunctionWrapper[writeBytesToStdOutFunc, bytes];
  res
]



$LSPServerLib := $LSPServerLib =
Catch[
Module[{res},

  res = FindLibrary["LSPServer"];
  If[FailureQ[res],
    Throw[Failure["LSPServerNativeLibraryNotFound", <||>]]
  ];
  res
]]



loadFunc[name_String, params_, ret_] :=
Catch[
Module[{res, loaded, linkObject},

  If[FailureQ[$LSPServerLib],
    Throw[$LSPServerLib]
  ];

  If[{params, ret} =!= {LinkObject, LinkObject},

    (*
    "regular" LibraryLink with no MathLink silliness
    *)
    loaded = LibraryFunctionLoad[$LSPServerLib, name, params, ret];

    If[Head[loaded] =!= LibraryFunction,
      Throw[Failure["LibraryFunctionLoad", <| "Result" -> loaded |>]]
    ];

    (*
    give a message and return a failure if called with arguments that do not match pattern
    *)
    With[{loaded = loaded},
      Throw[
        Function[
          Function[{res},
            If[MatchQ[res, HoldPattern[LibraryFunction[___]][___]],
              Message[LibraryFunction::unevaluated, loaded, {##}];
              Failure["Unevaluated", <| "Function" -> loaded, "Arguments" -> {##} |>]
              ,
              res
            ]
          ][loaded[##]]
        ]
      ]
    ]
  ];

  (*
  LibraryLink creates a separate loopback link for each library function
  *)
  res = newestLinkObject[LibraryFunctionLoad[$LSPServerLib, name, params, ret]];

  If[FailureQ[res],
    Throw[res]
  ];

  {loaded, linkObject} = res;

  If[FailureQ[loaded],
    Throw[loaded]
  ];

  If[Head[loaded] =!= LibraryFunction,
    Throw[Failure["LibraryFunctionLoad", <| "Result" -> loaded |>]]
  ];

  (*
  send fully-qualified symbol names over the wire
  library->kernel traffic has fully-qualified symbols.
  This allows LibraryLink traffic to work when LSPServer` is not on $ContextPath.
  And we want kernel->library traffic to match this behavior, to minimize surprises.
  Note: this still does not enable sending fully-qualified System` symbols
  bug 283291
  bug 284492
  *)
  MathLink`LinkSetPrintFullSymbols[linkObject, True];

  (*
  give a message and return a failure if called with arguments that do not match pattern
  *)
  With[{loaded = loaded},
    Function[
      Function[{res},
        If[MatchQ[res, HoldPattern[LibraryFunction[___]][___]],
          Message[LibraryFunction::unevaluated, loaded, {##}];
          Failure["Unevaluated", <| "Function" -> loaded, "Arguments" -> {##} |>]
          ,
          res
        ]
      ][loaded[##]]
    ]
  ]
]]

loadAllFuncs[] := (

setDebug := setDebug = loadFunc["SetDebug_LibraryLink", {Integer}, "Void"];

getStartupError := getStartupError = loadFunc["GetStartupError_LibraryLink", {}, Integer];

startBackgroundReaderThreadFunc := startBackgroundReaderThreadFunc = loadFunc["StartBackgroundReaderThread_LibraryLink", {}, "Void"];

lockQueueFunc := lockQueueFunc = loadFunc["LockQueue_LibraryLink", {}, "Void"];

unlockQueueFunc := unlockQueueFunc = loadFunc["UnlockQueue_LibraryLink", {}, "Void"];

getQueueSizeFunc := getQueueSizeFunc = loadFunc["GetQueueSize_LibraryLink", {}, Integer];

getFrontMessageSizeFunc := getFrontMessageSizeFunc = loadFunc["GetFrontMessageSize_LibraryLink", {}, Integer];

popQueueFunc := popQueueFunc = loadFunc["PopQueue_LibraryLink", { {LibraryDataType[ByteArray], "Shared"} }, "Void"];

getStdInFEOF := getStdInFEOF = loadFunc["GetStdInFEOF_LibraryLink", {}, Integer];

getStdInFError := getStdInFError = loadFunc["GetStdInFError_LibraryLink", {}, Integer];

getStdOutFEOF := getStdOutFEOF = loadFunc["GetStdOutFEOF_LibraryLink", {}, Integer];

getStdOutFError := getStdOutFError = loadFunc["GetStdOutFError_LibraryLink", {}, Integer];

readLineFromStdInFunc := readLineFromStdInFunc = loadFunc["ReadLineFromStdIn_LibraryLink", {}, "UTF8String"];

writeLineToStdOutFunc := writeLineToStdOutFunc = loadFunc["WriteLineToStdOut_LibraryLink", {"UTF8String"}, Integer];

writeBytesToStdOutFunc := writeBytesToStdOutFunc = loadFunc["WriteBytesToStdOut_LibraryLink", { {LibraryDataType[ByteArray], "Shared"} }, Integer];
)




Attributes[newestLinkObject] = {HoldFirst}

(*
Return the LinkObject that is created when evaluating expr along with the result of evaluating expr

this is all just to find the LinkObject associated with this LibraryFunction

TODO: If there is ever a nicer way to find the LinkObject, then use that
*)
newestLinkObject[expr_] :=
Catch[
Module[{before, after, res, set, first},
  before = Links[];
  (*evaluate*)
  res = expr;
  If[FailureQ[res],
    Throw[res]
  ];
  after = Links[];
  If[before == after,
    Throw[Failure["LinksDidNotChange", <||>]]
  ];
  set = Complement[after, before];
  If[Length[set] != 1,
    Throw[Failure["InternalLinksError", <| "Before" -> before, "After" -> after |>]]
  ];
  first = set[[1]];
  {res, first}
]]


(*
Handle the errors that may occur when calling LibraryLink functions
*)
libraryFunctionWrapper[libFunc_, args___] :=
Catch[
Module[{res},

  If[FailureQ[libFunc],
    Throw[libFunc]
  ];

  (*
  in the event of an abort, force reload of functions
  This will fix the transient error that can happen when an abort occurs
  and the next use throws LIBRARY_FUNCTION_ERROR
  *)
  CheckAbort[
  res = libFunc[args];
  ,
  loadAllFuncs[];
  Abort[]
  ];

  (*
  There may still be a hiccup when there is a LIBRARY_FUNCTION_ERROR and the next
  use of the function returns unevaluated
  *)
  If[MatchQ[res, _LibraryFunctionError | Verbatim[LibraryFunction][___][___]],
    (*
    Need to specify PageWidth, or else ToString does not do anything with Short
    Related bugs: ?
    *)
    Throw[Failure["LibraryFunctionError",
      <|
        "ShortResult" -> ToString[Short[res], OutputForm, PageWidth -> 100],
        (*
        "ShortArguments" and "Arguments" is really just taking up space to force "FullResult" to be hidden by default
        *)
        "ShortArguments" -> ToString[Short[{args}], OutputForm, PageWidth -> 100],
        "Arguments" -> {args},
        "FullResult" -> res
      |>]]
  ];

  res
]]


End[]

EndPackage[]
