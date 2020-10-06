BeginPackage["LSPServer`Generate`GenerateSources`"]

buildDirFlagPosition

buildDir

srcDirFlagPosition

srcDir

generatedWLDir

dataDir


importedLongNames

importedUnsupportedLongNames


longNameToCharacterCode

longNameToHexDigits

codePointToHexDigits


Begin["`Private`"]

longNameToCharacterCode[name_] := importedLongNames[name][[2]]


Print["Generating additional required source files..."]


buildDirFlagPosition = FirstPosition[$CommandLine, "-buildDir"]

If[MissingQ[buildDirFlagPosition],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

buildDir = $CommandLine[[buildDirFlagPosition[[1]] + 1]]

If[!DirectoryQ[buildDir],
  Print["Cannot proceed; Unsupported build directory"];
  Quit[1]
]

srcDirFlagPosition = FirstPosition[$CommandLine, "-srcDir"]

If[MissingQ[srcDirFlagPosition],
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]

srcDir = $CommandLine[[srcDirFlagPosition[[1]] + 1]]

If[!DirectoryQ[srcDir],
  Print["Cannot proceed; Unsupported src directory"];
  Quit[1]
]



generatedWLDir = FileNameJoin[{buildDir, "generated", "wl"}]


dataDir = FileNameJoin[{srcDir, "LSPServer", "Data"}]

PrependTo[$Path, srcDir]

If[FailureQ[FindFile["LSPServer`Generate`GenerateSources`"]],
  Print["LSPServer`Generate`GenerateSources` could not be found."];
  Quit[1]
]

Print["Clean..."]

Quiet[DeleteDirectory[generatedWLDir, DeleteContents -> True], DeleteDirectory::nodir]

Quiet[CreateDirectory[generatedWLDir], CreateDirectory::filex]

Print["Done Clean"]



importedLongNames = Get[FileNameJoin[{dataDir, "LongNames.wl"}]]

importedUnsupportedLongNames = Keys[Select[importedLongNames, #[[1]] === UnsupportedCharacter &]]


Get["LSPServer`Generate`ReplacePUA`"]


Print["Done generating additional required source files"]

End[]

EndPackage[]
