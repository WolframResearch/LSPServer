BeginPackage["LSPServer`Library`"]

loadAllFuncs

libraryFunctionWrapper


(*
library functions calling INTO lib
*)
readLineFromStdInFunc
readBytesFromStdInFunc

writeLineToStdOutFunc
writeBytesToStdOutFunc



(*
library functions coming FROM lib
*)




Begin["`Private`"]

Needs["CodeParser`"]
Needs["PacletManager`"] (* for PacletInformation *)


$LSPServerLib := $LSPServerLib =
Catch[
Module[{res},

  res = FindLibrary["LSPServer"];
  If[FailureQ[res],
    Throw[Failure["LSPServerLibraryNotFound", <||>]]
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
      Throw[Failure["LibraryFunctionLoad", <|"Result"->loaded|>]]
    ];

    Throw[loaded]
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
    Throw[Failure["LibraryFunctionLoad", <|"Result"->loaded|>]]
  ];

  (*
  send fully-qualified symbol names over the wire
  library->kernel traffic has fully-qualified symbols.
  This allows LibraryLink traffic to work when CodeParser` is not on $ContextPath.
  And we want kernel->library traffic to match this behavior, to minimize surprises.
  Note: this still does not enable sending fully-qualified System` symbols
  bug 283291
  bug 284492
  *)
  MathLink`LinkSetPrintFullSymbols[linkObject, True];

  loaded
]]

loadAllFuncs[] := (

readLineFromStdInFunc := readLineFromStdInFunc = loadFunc["ReadLineFromStdIn_LibraryLink", {}, "UTF8String"];

readBytesFromStdInFunc := readBytesFromStdInFunc = loadFunc["ReadBytesFromStdIn_LibraryLink", { {LibraryDataType[ByteArray], "Shared"} }, "Void"];

writeLineToStdOutFunc := writeLineToStdOutFunc = loadFunc["WriteLineToStdOut_LibraryLink", {"UTF8String"}, "Void"];

writeBytesToStdOutFunc := writeBytesToStdOutFunc = loadFunc["WriteBytesToStdOut_LibraryLink", { {LibraryDataType[ByteArray], "Shared"} }, "Void"];
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
    Throw[Failure["InternalLinksError", <|"Before"->before, "After"->after|>]]
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
