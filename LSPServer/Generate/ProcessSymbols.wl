BeginPackage["LSPInfra`Generate`ProcessSymbols`"]

Begin["`Private`"]


Print["Processing Symbols..."]

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



LSPInfra`Generate`$constants =
{

"\\[FormalA]", "\\[FormalB]", "\\[FormalC]", "\\[FormalD]", "\\[FormalE]",
"\\[FormalF]", "\\[FormalG]", "\\[FormalH]", "\\[FormalI]", "\\[FormalJ]",
"\\[FormalK]", "\\[FormalL]", "\\[FormalM]", "\\[FormalN]", "\\[FormalO]",
"\\[FormalP]", "\\[FormalQ]", "\\[FormalR]", "\\[FormalS]", "\\[FormalT]",
"\\[FormalU]", "\\[FormalV]", "\\[FormalW]", "\\[FormalX]", "\\[FormalY]",
"\\[FormalZ]", "\\[FormalCapitalA]", "\\[FormalCapitalB]",
"\\[FormalCapitalC]", "\\[FormalCapitalD]", "\\[FormalCapitalE]",
"\\[FormalCapitalF]", "\\[FormalCapitalG]", "\\[FormalCapitalH]",
"\\[FormalCapitalI]", "\\[FormalCapitalJ]", "\\[FormalCapitalK]",
"\\[FormalCapitalL]", "\\[FormalCapitalM]", "\\[FormalCapitalN]",
"\\[FormalCapitalO]", "\\[FormalCapitalP]", "\\[FormalCapitalQ]",
"\\[FormalCapitalR]", "\\[FormalCapitalS]", "\\[FormalCapitalT]",
"\\[FormalCapitalU]", "\\[FormalCapitalV]", "\\[FormalCapitalW]",
"\\[FormalCapitalX]", "\\[FormalCapitalY]", "\\[FormalCapitalZ]",
"\\[FormalCapitalAlpha]", "\\[FormalCapitalBeta]",
"\\[FormalCapitalGamma]", "\\[FormalCapitalDelta]",
"\\[FormalCapitalEpsilon]", "\\[FormalCapitalZeta]",
"\\[FormalCapitalEta]", "\\[FormalCapitalTheta]",
"\\[FormalCapitalIota]", "\\[FormalCapitalKappa]",
"\\[FormalCapitalLambda]", "\\[FormalCapitalMu]", "\\[FormalCapitalNu]",
"\\[FormalCapitalXi]", "\\[FormalCapitalOmicron]",
"\\[FormalCapitalPi]", "\\[FormalCapitalRho]", "\\[FormalCapitalSigma]",
"\\[FormalCapitalTau]", "\\[FormalCapitalUpsilon]",
"\\[FormalCapitalPhi]", "\\[FormalCapitalChi]", "\\[FormalCapitalPsi]",
"\\[FormalCapitalOmega]", "\\[FormalAlpha]", "\\[FormalBeta]",
"\\[FormalGamma]", "\\[FormalDelta]", "\\[FormalCurlyEpsilon]",
"\\[FormalZeta]", "\\[FormalEta]", "\\[FormalTheta]", "\\[FormalIota]",
"\\[FormalKappa]", "\\[FormalLambda]", "\\[FormalMu]", "\\[FormalNu]",
"\\[FormalXi]", "\\[FormalOmicron]", "\\[FormalPi]", "\\[FormalRho]",
"\\[FormalFinalSigma]", "\\[FormalSigma]", "\\[FormalTau]",
"\\[FormalUpsilon]", "\\[FormalCurlyPhi]", "\\[FormalChi]",
"\\[FormalPsi]", "\\[FormalOmega]", "\\[FormalCurlyTheta]",
"\\[FormalCurlyCapitalUpsilon]", "\\[FormalPhi]", "\\[FormalCurlyPi]",
"\\[FormalCapitalStigma]", "\\[FormalStigma]",
"\\[FormalCapitalDigamma]", "\\[FormalDigamma]",
"\\[FormalCapitalKoppa]", "\\[FormalKoppa]", "\\[FormalCapitalSampi]",
"\\[FormalSampi]", "\\[FormalCurlyKappa]", "\\[FormalCurlyRho]",
"\\[FormalEpsilon]",

"Above", "After", "All", "Anonymous", "Automatic", "Axis", "Back", \
"Backward", "Baseline", "Before", "Below", "Black", "Blue", "Bold", \
"Bottom", "Brown", "Byte", "Catalan", "CellStyle", "Center", \
"Character", "ComplexInfinity", "Constant", "Cyan", "Dashed", \
"Decimal", "DefaultAxesStyle", "DefaultBaseStyle", "DefaultBoxStyle", \
"DefaultFaceGridsStyle", "DefaultFieldHintStyle", \
"DefaultFrameStyle", "DefaultFrameTicksStyle", \
"DefaultGridLinesStyle", "DefaultLabelStyle", "DefaultMenuStyle", \
"DefaultTicksStyle", "DefaultTooltipStyle", "Degree", "Delimiter", \
"DigitCharacter", "DirectedInfinity", "DotDashed", "Dotted", \
"DragAndDrop", "E", "EndOfBuffer", "EndOfFile", "EndOfLine", \
"EndOfString", "EulerGamma", "Expression", "False", "Flat", \
"FontProperties", "Forward", "ForwardBackward", "Friday", "Front", \
"FrontEndDynamicExpression", "Full", "General", "Generic", \
"Glaisher", "GoldenAngle", "GoldenRatio", "Gray", "Green", "Here", \
"HexadecimalCharacter", "HoldAll", "HoldAllComplete", "HoldFirst", \
"HoldRest", "I", "Indeterminate", "Infinity", "Inherited", "Integer", \
"Italic", "K", "Khinchin", "Large", "Larger", "Launch", "Left", \
"LetterCharacter", "LightBlue", "LightBrown", "LightCyan", \
"LightGray", "LightGreen", "LightMagenta", "LightOrange", \
"LightPink", "LightPurple", "LightRed", "LightYellow", "Listable", \
"Listen", "Locked", "Loopback", "MachinePrecision", "Magenta", \
"Manual", "Medium", "MeshCellCentroid", "MeshCellMeasure", \
"MeshCellQuality", "Modular", "Monday", "NHoldAll", "NHoldFirst", \
"NHoldRest", "NonAssociative", "None", "Now", "NoWhitespace", "Null", \
"Number", "NumberString", "Orange", "ParentForm", "Pi", "Pink", \
"Plain", "Protected", "PunctuationCharacter", "Purple", \
"ReadProtected", "Real", "Record", "Red", "Right", "Saturday", \
"SequenceHold", "Small", "Smaller", "SpanFromAbove", "SpanFromBoth", \
"SpanFromLeft", "StartOfLine", "StartOfString", "String", "Stub", \
"Sunday", "Temporary", "Thick", "Thin", "ThisLink", "Thursday", \
"Tiny", "Today", "Tomorrow", "Top", "Transparent", "True", "Tuesday", \
"Undefined", "Underlined", "Wednesday", "White", "Whitespace", \
"WhitespaceCharacter", "Word", "WordBoundary", "WordCharacter", \
"Yellow", "Yesterday", "$Aborted", "$AllowExternalChannelFunctions", \
"$AssertFunction", "$Assumptions", "$AsynchronousTask", \
"$AudioInputDevices", "$AudioOutputDevices", "$BaseDirectory", \
"$BatchInput", "$BatchOutput", "$BlockchainBase", "$BoxForms", \
"$ByteOrdering", "$CacheBaseDirectory", "$Canceled", "$ChannelBase", \
"$CharacterEncoding", "$CharacterEncodings", "$CloudBase", \
"$CloudConnected", "$CloudCreditsAvailable", "$CloudEvaluation", \
"$CloudExpressionBase", "$CloudRootDirectory", "$CloudSymbolBase", \
"$CloudUserID", "$CloudUserUUID", "$CloudVersion", "$CommandLine", \
"$CompilationTarget", "$ConfiguredKernels", "$Context", \
"$ContextPath", "$ControlActiveSetting", "$Cookies", "$CookieStore", \
"$CreationDate", "$CurrentLink", "$DateStringFormat", \
"$DefaultAudioInputDevice", "$DefaultAudioOutputDevice", \
"$DefaultFont", "$DefaultImagingDevice", "$DefaultLocalBase", \
"$DefaultNetworkInterface", "$DefaultPath", "$Display", \
"$DisplayFunction", "$DistributedContexts", "$DynamicEvaluation", \
"$Echo", "$EmbedCodeEnvironments", "$EmbeddableServices", \
"$EntityStores", "$Epilog", "$EvaluationCloudBase", \
"$EvaluationCloudObject", "$EvaluationEnvironment", "$ExportFormats", \
"$Failed", "$FontFamilies", "$FormatType", "$FrontEnd", \
"$FrontEndSession", "$GeoLocation", "$GeoLocationCity", \
"$GeoLocationCountry", "$GeoLocationSource", "$HistoryLength", \
"$HomeDirectory", "$HTTPCookies", "$IgnoreEOF", \
"$ImageFormattingWidth", "$ImagingDevice", "$ImagingDevices", \
"$ImportFormats", "$IncomingMailSettings", "$InitialDirectory", \
"$InitializationContexts", "$Input", "$InputFileName", \
"$InputStreamMethods", "$Inspector", "$InstallationDate", \
"$InstallationDirectory", "$InterpreterTypes", "$IterationLimit", \
"$KernelCount", "$KernelID", "$Language", "$LibraryPath", \
"$LicenseExpirationDate", "$LicenseID", "$LicenseProcesses", \
"$LicenseServer", "$LicenseSubprocesses", "$Line", "$Linked", \
"$LinkSupported", "$LocalBase", "$LocalSymbolBase", \
"$MachineAddresses", "$MachineDomain", "$MachineDomains", \
"$MachineEpsilon", "$MachineID", "$MachineName", "$MachinePrecision", \
"$MachineType", "$MaxExtraPrecision", "$MaxLicenseProcesses", \
"$MaxLicenseSubprocesses", "$MaxMachineNumber", "$MaxNumber", \
"$MaxPiecewiseCases", "$MaxPrecision", "$MaxRootDegree", \
"$MessageGroups", "$MessagePrePrint", "$MinMachineNumber", \
"$MinNumber", "$MinPrecision", "$MobilePhone", "$ModuleNumber", \
"$NetworkConnected", "$NetworkInterfaces", "$NetworkLicense", \
"$NewMessage", "$NewSymbol", "$Notebooks", "$NumberMarks", \
"$OperatingSystem", "$Output", "$OutputForms", "$OutputSizeLimit", \
"$OutputStreamMethods", "$Packages", "$ParentLink", \
"$ParentProcessID", "$PasswordFile", "$Path", "$PathnameSeparator", \
"$PerformanceGoal", "$Permissions", "$PersistenceBase", \
"$PersistencePath", "$PipeSupported", "$PlotTheme", "$Post", "$Pre", \
"$PrePrint", "$PreRead", "$PrintForms", "$Printout3DPreviewer", \
"$ProcessID", "$ProcessorCount", "$ProcessorType", \
"$ProductInformation", "$ProgramName", "$PublisherID", \
"$RandomState", "$RecursionLimit", "$ReleaseNumber", \
"$RequesterAddress", "$RequesterWolframID", "$RequesterWolframUUID", \
"$RootDirectory", "$ScheduledTask", "$ScriptCommandLine", \
"$ScriptInputString", "$ServiceCreditsAvailable", "$Services", \
"$SessionID", "$SharedFunctions", "$SharedVariables", \
"$SoundDisplayFunction", "$SourceLink", "$SummaryBoxDataSizeLimit", \
"$SuppressInputFormHeads", "$SynchronousEvaluation", \
"$SyntaxHandler", "$System", "$SystemCharacterEncoding", "$SystemID", \
"$SystemShell", "$SystemTimeZone", "$SystemWordLength", \
"$TemplatePath", "$TemporaryDirectory", "$TemporaryPrefix", \
"$TextStyle", "$TimedOut", "$TimeUnit", "$TimeZone", \
"$TimeZoneEntity", "$TracePattern", "$TracePostAction", \
"$TracePreAction", "$UnitSystem", "$Urgent", "$UserAgentString", \
"$UserBaseDirectory", "$UserDocumentsDirectory", "$UserName", \
"$UserURLBase", "$Version", "$VersionNumber", "$VoiceStyles", \
"$WolframID", "$WolframUUID",

"\\[SystemsModelDelay]"

}



setupSystemSymbols[] :=
Catch[
Module[{names, documentedSymbols, allSymbols, allASCIISymbols, obsoleteNames,
  experimentalNames, obsoleteString, experimentalString, res},

  allSymbols = Names["System`*"];

  allASCIISymbols = Flatten[StringCases[allSymbols, RegularExpression["[a-zA-Z0-9$]+"]]];

  Print["There are ", Length[allASCIISymbols], " System` symbols."];

  res = SetDirectory[FileNameJoin[{$InstallationDirectory, "Documentation/English/System/ReferencePages/Symbols"}]];

  If[FailureQ[res],
    Throw[res]
  ];

  Print["scanning Documented symbols..."];

  names = FileNames["*.nb", "", Infinity];

  documentedSymbols = StringDrop[#, -3]& /@ names;

  Print["There are ", Length[documentedSymbols], " documented symbols in System`. (there are also ", Length[Complement[documentedSymbols, allASCIISymbols]], " documented symbols not in System`)"];

  LSPInfra`Generate`$undocumentedSymbols = Complement[allASCIISymbols, documentedSymbols];

  Print["There are ", Length[LSPInfra`Generate`$undocumentedSymbols], " undocumented symbols in System`."];

  Print["scanning Obsolete symbols... \[WatchIcon]"];

  (*
  "OBSOLETE SYMBOL" is found in the first ~50 lines, so use 100 as a heuristic for how many lines to read
  *)

  obsoleteString =
    Which[
      $VersionNumber >= 11.1,
        "\"OBSOLETE SYMBOL\"",
      True,
        "\"OBSOLETE WOLFRAM LANGUAGE SYMBOL\""
    ];

  obsoleteNames = Select[names, FindList[#, obsoleteString, 100] != {}&];

  LSPInfra`Generate`$obsoleteSymbols = StringDrop[#, -3]& /@ obsoleteNames;

  Print["There are ", Length[LSPInfra`Generate`$obsoleteSymbols], " obsolete symbols in System`."];

  If[LSPInfra`Generate`$obsoleteSymbols == {},
    LSPInfra`Generate`$obsoleteSymbols = {"ObsoletePlaceholderXXX"}
  ];

  Print["scanning Experimental symbols... \[WatchIcon]"];

  experimentalString =
    Which[
      $VersionNumber >= 11.1,
        "\"EXPERIMENTAL\"",
      True,
        "\"EXPERIMENTAL\""
    ];

  (*
  "EXPERIMENTAL" is found in the first ~500 lines, so use 1000 as a heuristic for how many lines to read
  *)
  experimentalNames = Select[names, FindList[#, experimentalString, 1000] != {}&];

  LSPInfra`Generate`$experimentalSymbols = StringDrop[#, -3]& /@ experimentalNames;

  Print["There are ", Length[LSPInfra`Generate`$experimentalSymbols], " experimental symbols in System`."];

  If[LSPInfra`Generate`$experimentalSymbols == {},
    LSPInfra`Generate`$experimentalSymbols = {"ExperimentalPlaceholderXXX"}
  ];

  LSPInfra`Generate`$builtInFunctions =
    Complement[documentedSymbols,
      LSPInfra`Generate`$constants,
      LSPInfra`Generate`$obsoleteSymbols,
      LSPInfra`Generate`$experimentalSymbols];

  ResetDirectory[];
]]

dumpSystemSymbols[] :=
Module[{dumpFile},

  dumpFile = FileNameJoin[{buildDir, "processedSymbols.mx"}];

  DumpSave[dumpFile, {
    LSPInfra`Generate`$builtInFunctions,
    LSPInfra`Generate`$constants,
    LSPInfra`Generate`$undocumentedSymbols,
    LSPInfra`Generate`$experimentalSymbols,
    LSPInfra`Generate`$obsoleteSymbols}]
]

setupSystemSymbols[]

dumpSystemSymbols[]

Print["Done Processing Symbols"]

End[]

EndPackage[]
