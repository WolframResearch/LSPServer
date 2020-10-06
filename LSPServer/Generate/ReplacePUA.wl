BeginPackage["LSPServer`Generate`ReplacePUA`"]


Begin["`Private`"]

Needs["LSPServer`Generate`GenerateSources`"]



Print["Generating ReplacePUA..."]


puaChars = Select[importedLongNames, 16^^e000 <= #[[2]] <= 16^^f8ff &]


replacements = Select[puaChars, MatchQ[#[[3]], KeyValuePattern["ASCIIReplacements" -> _]]&]


nonReplacements = Keys[Select[puaChars, !MatchQ[#[[3]], KeyValuePattern["ASCIIReplacements" -> _]]&]]

nonReplacements = Complement[nonReplacements, importedUnsupportedLongNames]


replacePUARules =
	KeyValueMap[
		Function[{k, v},
			ToExpression["\"\\["<> k <> "]\""] -> v[[3, Key["ASCIIReplacements"], -1]]
		]
		,
		replacements
	] ~Join~

	Map[
		Function[{c},
			ToExpression["\"\\["<> c <> "]\""] -> " " <> c <> " "
		]
		,
		nonReplacements
	]


replacePUAWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

BeginPackage[\"LSPServer`ReplacePUA`\"]

replacePUA

Begin[\"`Private`\"]

replacePUA[s_String] :=
  StringReplace[s, "} ~Join~

{ToString[replacePUARules, InputForm, CharacterEncoding -> "ASCII", PageWidth -> 120]} ~Join~ {"

  ]

End[]

EndPackage[]
"
}

Print["exporting ReplacePUA.wl"]
res = Export[FileNameJoin[{generatedWLDir, "ReplacePUA.wl"}], Column[replacePUAWL], "String"]

If[FailureQ[res],
  Print[res];
  Quit[1]
]


End[]

EndPackage[]
