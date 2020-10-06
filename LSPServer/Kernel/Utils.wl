BeginPackage["LSPServer`Utils`"]

normalizeURI

merge

stringLineTake

log

Begin["`Private`"]

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



End[]

EndPackage[]
