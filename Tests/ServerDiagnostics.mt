
Needs["LSPServer`"]
Needs["LSPServer`ServerDiagnostics`"]


Switch[$OperatingSystem,
	"MacOSX",
		wolframKernel = FileNameJoin[{$InstallationDirectory, "MacOS", "WolframKernel"}];
	,
	"Unix",
		wolframKernel = FileNameJoin[{$InstallationDirectory, "Executables", "WolframKernel"}];
	,
	"Windows",
		wolframKernel = FileNameJoin[{$InstallationDirectory, "WolframKernel.exe"}];
]

Test[
	RunServerDiagnostic[{
		wolframKernel,
		"-noinit",
		"-noprompt",
		"-nopaclet",
		"-noicon",
		"-nostartuppaclets",
		"-run", "Needs[\"LSPServer`\"];LSPServer`StartServer[]"
	}]
	,
	Null
	,
	TestID->"ServerDiagnostics-20220901-D3O0N3"
]

codeParserVerStr = Information[PacletObject["CodeParser"]]["Version"];
lspServerVerStr = Information[PacletObject["LSPServer"]]["Version"];
codeFormatterVerStr = Information[PacletObject["CodeFormatter"]]["Version"];
codeInspectorVerStr = Information[PacletObject["CodeInspector"]]["Version"];

(* 
	With recent CodeParser, CodeFormatter, CodeInspector and LSPServer V 1.12 we do not expect
	to get any version mismatch warning message.
*)
VerificationTest[prints = {};
	If[	codeParserVerStr === "1.10" && codeFormatterVerStr === "1.10" &&
		codeInspectorVerStr === "1.10" && lspServerVerStr === "1.12",

		Block[{Print = AppendTo[prints, #] &}, 
			RunServerDiagnostic[{
				wolframKernel, 
				"-noinit", 
				"-noprompt", 
				"-nopaclet", 
				"-noicon", 
				"-nostartuppaclets", 
				"-run", 
				"Needs[\"LSPServer`\"]; LSPServer`StartServer[]"
			}]
		];
		Select[Select[prints, StringQ], StringMatchQ["WARNING*"]]
		,
		{}
	]
	, 
	(* expected output *)
	{}
	,
	(* Expecting a list of messages *)
	{___}
	,
	TestID -> "ServerDiagnostics-Version-mismatch-warning"
	
]
