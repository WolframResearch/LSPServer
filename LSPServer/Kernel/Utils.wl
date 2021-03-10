BeginPackage["LSPServer`Utils`"]

normalizeURI

merge

stringLineTake

log

logFull

lintToDiagnostics

isStale

Begin["`Private`"]

Needs["LSPServer`"]
Needs["CodeInspector`"]
Needs["CodeInspector`Utils`"]
Needs["CodeParser`"]


$DiagnosticSeverity = <|
  "Error" -> 1,
  "Warning" -> 2,
  "Information" -> 3,
  "Hint" -> 4
|>


(*
input:  "file:///Users/brenton/development/stash/COD/ast/build/test.m"
return:  "/Users/brenton/development/stash/COD/ast/build/test.m"
*)
normalizeURI[uri_String] := FileNameJoin[FileNameSplit[URL[uri]]]


merge[line1_Association, line2_Association] :=
  Module[{},
    If[line1["line"] =!= line2["line"],
      Throw[{line1, line2}, "Unhandled"]
    ];
    If[Length[line1["characters"]] =!= Length[line2["characters"]],
      Throw[{line1, line2}, "Unhandled"]
    ];
    <|"line" -> line1["line"], "characters" -> ((# /. {
        {a_, "&nbsp;"} :> a,
        {"&nbsp;", b_} :> b,
        (*
        FIXME: arbitrarily choose the first arrow
        if these are the same action, then there is no problem
        but if one is an Insert and one is a Delete? Is that possible?
        maybe should choose based on probability
        *)
        {a_, b_} :> a
      })& /@ Transpose[{line1["characters"], line2["characters"]}]) |>
  ]


stringLineTake[s_String, spec_] :=
  Catch[
  Module[{newlines, split},

    newlines = StringCases[s, "\r\n" | "\n" | "\r"];

    If[empty[newlines],
      Throw[s]
    ];

    newlines = Take[newlines, spec];

    split = StringSplit[s, "\r\n" | "\n" | "\r"];

    split = Take[split, spec];

    StringJoin[Riffle[split, newlines]]
  ]]


timeString[] := DateString[{"Hour24", ":", "Minute", ":", "SecondExact", " "}]


log[args___] := Write[$Messages, timeString[] //OutputForm, Sequence @@ (OutputForm /@ {args})]

logFull[args___] := Write[$Messages, timeString[] //OutputForm, Sequence @@ {args}]



lintToDiagnostics[InspectionObject[tag_, message_, severity_, data_]] :=
Catch[
Module[{srcs},

  If[!KeyExistsQ[data, Source],
    (*
    It is possible that abstracted problems may not have Source

    An example would be  a < b > c  being abstracted as an Inequality expression

    Inequality is an undocumented symbol, but it does not actually show up in the source code

    So it would be wrong to report "Inequality is an undocumented symbol" for  a < b > c
    *)
    Throw[{}]
  ];

  srcs = { data[Source] } ~Join~ Lookup[data, "AdditionalSources", {}];

  Function[{src}, (<|
    "code" -> tag,
    "message" -> plainify[message],
    "severity" -> lintSeverityToLSPSeverity[severity],
    "range" -> <|
      "start" -> <| "line" -> #[[1, 1]], "character" -> #[[1, 2]] |>,
      "end" -> <| "line" -> #[[2, 1]], "character" -> #[[2, 2]] |>
    |>,
    "source" -> "wolfram lint" |>)&[Map[Max[#, 0]&, src-1, {2}]]
  ] /@ srcs
]]


(*
convert from CodeTools Lint severities to LSP severities
*)
lintSeverityToLSPSeverity[severity_String] :=
Switch[severity,
  "Formatting" | "ImplicitTimes" | "Scoping", $DiagnosticSeverity["Hint"],
  "Remark", $DiagnosticSeverity["Information"],
  "Warning", $DiagnosticSeverity["Warning"],
  "Error" | "Fatal", $DiagnosticSeverity["Error"]
]



(*

Are there any didChangeFenceposts or didCloseFenceposts in the queue with the same uri?

Then we know that this current message is stale

*)

isStale[contents_, uri_] :=
  AnyTrue[contents,
      MatchQ[#, KeyValuePattern[{
        "method" -> "textDocument/didChangeFencepost" | "textDocument/didCloseFencepost",
        "params" -> KeyValuePattern["textDocument" -> KeyValuePattern["uri" -> uri]]}]]&]



End[]

EndPackage[]
