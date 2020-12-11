
If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["LSPServer`Generate`ReplacePUA`"]


Begin["`Private`"]

Needs["CodeTools`Generate`GenerateSources`"]


dataDir = FileNameJoin[{srcDir, "CodeParser", "Data"}]

importedLongNames = Get[FileNameJoin[{dataDir, "LongNames.wl"}]]


puaChars = Select[importedLongNames, (16^^e000 <= #[[2]] <= 16^^f8ff)&]

replacements = Select[puaChars, MatchQ[#[[3]], KeyValuePattern["ASCIIReplacements" -> _]]&]

nonReplacements = Select[puaChars, (!MatchQ[#[[3]], KeyValuePattern["ASCIIReplacements" -> _]] && !MemberQ[{"RawCharacter", "UnsupportedCharacter"}, SymbolName[#[[1]]]])&]


(*
Basically just converts "Alpha" => "\[Alpha]"

But handles newly-added characters by converting to \: notation
*)
toChar[k_, v_] :=
	Module[{},
		If[MatchQ[v[[3]], KeyValuePattern["Added" -> _]],
			ToExpression["\"\\:" <> IntegerString[v[[2]], 16, 4] <> "\""]
			,
			ToExpression["\"\\["<> k <> "]\""]
		]
	]



replacePUARules =
	KeyValueMap[
		Function[{k, v},
			toChar[k, v] -> v[[3, Key["ASCIIReplacements"], -1]]
		]
		,
		replacements
	] ~Join~

	KeyValueMap[
		Function[{k, v},
			toChar[k, v] -> (" " <> k <> " ")
		]
		,
		nonReplacements
	]


generate[] := (

Print["Generating ReplacePUA..."];

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
};

Print["exporting ReplacePUA.wl"];
res = Export[FileNameJoin[{generatedWLDir, "ReplacePUA.wl"}], Column[replacePUAWL], "String"];

If[FailureQ[res],
  Print[res];
  Quit[1]
];

Print["Done ReplacePUA"];
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
