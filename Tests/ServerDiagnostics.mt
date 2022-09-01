
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
