
If[!MemberQ[$Path, #], PrependTo[$Path, #]]&[DirectoryName[$InputFileName, 3]]

BeginPackage["LSPServer`Generate`ReplaceLongNamePUA`"]


Begin["`Private`"]

(*
Do not allow PacletManager to participate in finding `Generate` files

PacletManager will find e.g. CodeParser/Kernel/TokenEnum.wl when asked to find CodeParser`Generate`TokenEnum`

related issues: PACMAN-54
*)
Block[{Internal`PacletFindFile = Null&},
Needs["CodeTools`Generate`GenerateSources`"];
]


dataDir = FileNameJoin[{srcDir, "CodeParser", "Data"}]

importedLongNames = Get[FileNameJoin[{dataDir, "LongNames.wl"}]]


longNamePUAChars = Select[importedLongNames, (16^^e000 <= #[[2]] <= 16^^f8ff)&]

replacements = Select[longNamePUAChars, MatchQ[#[[3]], KeyValuePattern["ASCIIReplacements" -> _]]&]

nonReplacements = Select[longNamePUAChars, (!MatchQ[#[[3]], KeyValuePattern["ASCIIReplacements" -> _]] && !MemberQ[{"RawCharacter", "UnsupportedCharacter"}, SymbolName[#[[1]]]])&]





Attributes[HoldFormBlindToInputForm] = {HoldFirst}

Format[HoldFormBlindToInputForm[s_Symbol], InputForm] := s

Format[HoldFormBlindToInputForm[s_String], InputForm] := s

Format[HoldFormBlindToInputForm[h_[arg1_]], InputForm] :=
	HoldFormBlindToInputForm[h][HoldFormBlindToInputForm[arg1]]


(*
Basically just converts "Alpha" => "\[Alpha]"

But handles newly-added characters by converting to \: notation

TODO: I would just write out \:xxxx if it were easy to do...
*)
toChar[k_, v_] :=
	Module[{},
		If[MatchQ[v[[3]], KeyValuePattern["Added" -> _]],
			With[{str = "\"\\:" <> IntegerString[v[[2]], 16, 4] <> "\""},
				HoldFormBlindToInputForm[ToExpression[str]]
			]
			,
			ToExpression["\"\\["<> k <> "]\""]
		]
	]



replaceLongNamePUARules =
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

Print["Generating ReplaceLongNamePUA..."];

replaceLongNamePUAWL = {
"
(*
AUTO GENERATED FILE
DO NOT MODIFY
*)

BeginPackage[\"LSPServer`ReplaceLongNamePUA`\"]

replaceLongNamePUA

Begin[\"`Private`\"]

replaceLongNamePUA[s_String] :=
  StringReplace[s, "} ~Join~

{ToString[replaceLongNamePUARules, InputForm, CharacterEncoding -> "ASCII", PageWidth -> 120]} ~Join~
{"  ]

End[]

EndPackage[]
"
};

Print["exporting ReplaceLongNamePUA.wl"];
res = Export[FileNameJoin[{generatedWLDir, "Kernel", "ReplaceLongNamePUA.wl"}], Column[replaceLongNamePUAWL], "String"];

If[FailureQ[res],
  Print[res];
  Quit[1]
];

Print["Done ReplaceLongNamePUA"];
)

If[!StringQ[script],
  Quit[1]
]
If[AbsoluteFileName[script] === AbsoluteFileName[$InputFileName],
generate[]
]

End[]

EndPackage[]
