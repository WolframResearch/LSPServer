BeginPackage["LSPServer`"]

StartServer::usage = "StartServer[] puts the kernel into a state ready for traffic from the wolfram_lsp_proxy script.\
 StartServer[logFile] logs traffic to logFile."

Begin["`Private`"]

Needs["AST`"]
Needs["AST`Utils`"]
Needs["Lint`"]
Needs["Lint`Report`"]





LSPEvaluate

handleContent



$ErrorCodes = <|
	(*
	Defined by JSON RPC
	*)
	"ParseError" -> -32700,
	"InvalidRequest" -> -32600,
	"MethodNotFound" -> -32601,
	"InvalidParams" -> -32602,
	"InternalError" -> -32603,
	"serverErrorStart" -> -32099,
	"serverErrorEnd" -> -32000,
	"ServerNotInitialized" -> -32002,
	"UnknownErrorCode" -> -32001,

	(*
	Defined by the protocol.
	*)
	"RequestCancelled" -> -32800,
	"ContentModified" -> -32801
|>

$DiagnosticSeverity = <|
	"Error" -> 1,
	"Warning" -> 2,
	"Information" -> 3,
	"Hint" -> 4
|>


$TextDocumentSyncKind = <|
	"None" -> 0,
	"Full" -> 1,
	"Incremental" -> 2
|>






$logFileStream

$Debug


(*
setup the REPL to handle traffic from wolfram_lsp_proxy script
*)
StartServer[logFile_String:""] :=
Module[{},

	$Debug = (logFile != "");

	If[$Debug,
		
		$logFileStream = OpenWrite[logFile, CharacterEncoding -> "UTF8"];

		WriteString[$logFileStream, "$CommandLine: ", $CommandLine, "\n"];
	];

	(*
	Convert the line of text from stdin into a String
	*)
	$PreRead = ToString[#, InputForm]&;
	
	$Pre = LSPEvaluate;
	
	(*
	Some mode is turned on where everything is printed as InputForm.
	Control by wrapping in OutputForm, and make sure to return Null.
	*)
	$Post = Print[OutputForm[#]]&;
	
	(*
	Ensure that no messages are printed to stdout
	*)
	If[$Debug,
		$Messages = { $logFileStream }
		,
		$Messages = {}
	];
]


(*
input string: RPC-JSON string on a single line

returns: RPC-JSON string
*)
LSPEvaluate[string_String] :=
Catch[
Module[{content, json},

	If[$Debug,
		WriteString[$logFileStream, "P-->K  ", string, "\n"];
	];

	content = ImportString[string, "RawJSON"];

	content = handleContent[content];

	If[content === Null,
		Throw[""]
	];
	If[!AssociationQ[content],

		If[$Debug,
			WriteString[$logFileStream, "ERROR\n"];
			WriteString[$logFileStream, content, "\n"];
		];

		Exit[1]
	];

	json = ExportString[content, "JSON"];

	If[!StringQ[json],

		If[$Debug,
			WriteString[$logFileStream, "ERROR\n"];
			WriteString[$logFileStream, content, "\n"];
		];

		Exit[2]
	];

	json = StringReplace[json, "\n" -> ""];

	If[$Debug,
		WriteString[$logFileStream, "P<--K  ", json, "\n"];
	];

	json
]]



(*
content: JSON-RPC Association

returns: JSON-RPC Association
*)
handleContent[content:KeyValuePattern["method" -> "initialize"]] :=
Module[{id, params, capabilities, textDocument, codeAction},

	id = content["id"];
	params = content["params"];
	capabilities = params["capabilities"];
	textDocument = capabilities["textDocument"];
	codeAction = textDocument["codeAction"];

	<| "jsonrpc" -> "2.0", "id" -> id,
	   "result" -> <| "capabilities"-> <| "referencesProvider" -> True,
	                                      "textDocumentSync" -> <| "openClose" -> True,
	                                                               "save" -> <| "includeText" -> False |>,
	                                                               "change" -> $TextDocumentSyncKind["None"]
	                                                            |>
	                                   |>
	               |>
	|>
]


(*
Do not send a response back
*)
handleContent[content:KeyValuePattern["method" -> "initialized"]] := (
	Null
)



handleContent[content:KeyValuePattern["method" -> "shutdown"]] :=
Module[{id},
	id = content["id"];
	<| "jsonrpc" -> "2.0", "id" -> id, "result" -> Null |>
]


handleContent[content:KeyValuePattern["method" -> "exit"]] := (
	Exit[0]
)


handleContent[content:KeyValuePattern["method" -> "textDocument/references"]] :=
Catch[
Module[{id, params, doc, uri, file, cst, pos, line, char, cases, sym, name},

	id = content["id"];
	params = content["params"];
	doc = params["textDocument"];
	uri = doc["uri"];
	pos = params["position"];
	line = pos["line"];
	char = pos["character"];

	(*
	convert from 0-based to 1-based
	*)
	line+=1;
	char+=1;

	file = FileNameJoin[FileNameSplit[URL[uri]]];

	cst = ConcreteParseFile[file];

	(*
	Find the name of the symbol at the position
	*)
	cases = Cases[cst, LeafNode[Symbol, _, KeyValuePattern[Source -> src_ /; SourceMemberQ[src, {line, char}]]], Infinity];

	If[cases == {},
		Throw[<|"jsonrpc" -> "2.0", "id" -> id, "result" -> {} |>]
	];

	sym = cases[[1]];

	name = sym["String"];

	cases = Cases[cst, LeafNode[Symbol, name, _], Infinity];

	locations = (<| "uri" -> uri,
		             "range" -> <| "start" -> <| "line" -> #[[1,1]], "character" -> #[[1,2]] |>,
		                                                                                  (* end is exclusive *)
		             	            "end" -> <| "line" -> #[[2,1]], "character" -> #[[2,2]]+1 |>
		             	         |> |>&[#[[3]][Source] - 1])& /@ cases;

	<|"jsonrpc" -> "2.0", "id" -> id, "result" -> locations |>
]]







(*
textDocument/didOpen is a notification (so no response), but take this chance to do linting and send textDocument/publishDiagnostics
*)
handleContent[content:KeyValuePattern["method" -> "textDocument/didOpen"]] :=
Module[{params, doc, uri},

	params = content["params"];
	doc = params["textDocument"];
	uri = doc["uri"];
	
	publishDiagnosticsNotification[uri]
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didClose"]] :=
Module[{params, doc, uri},

	params = content["params"];
	doc = params["textDocument"];
	uri = doc["uri"];

	(*
	clear lints on file close

	NOTE: may want to be able to control this behavior
	*)
	publishDiagnosticsNotification[uri, {}]
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didSave"]] :=
Module[{params, doc, uri},
	
	params = content["params"];
	doc = params["textDocument"];
	uri = doc["uri"];
	
	publishDiagnosticsNotification[uri]
]

handleContent[content:KeyValuePattern["method" -> "textDocument/didChange"]] := (
	Null
)



(*
convert from CodeTools Lint severities to LSP severities
*)
lintSeverityToLSPSeverity[severity_String] :=
Switch[severity,
	"Formatting" | "ImplicitTimes", $DiagnosticSeverity["Hint"],
	"Remark", $DiagnosticSeverity["Information"],
	"Warning", $DiagnosticSeverity["Warning"],
	"Error" | "Fatal", $DiagnosticSeverity["Error"]
]


publishDiagnosticsNotification[uri_String] :=
Module[{file, lints},
	file = FileNameJoin[FileNameSplit[URL[uri]]];

	lints = LintFile[file];

	(*
	Might get something like FileTooLarge
	Still want to update
	*)
	If[FailureQ[lints],
		lints = {}
	];

	publishDiagnosticsNotification[uri, lints]
]


publishDiagnosticsNotification[uri_String, lints_List] :=
Module[{diagnostics},

	(*
	
	if no lints, must still publish in order to update client

	If[lints == {},
		Throw[Null]
	];
	*)

	diagnostics = Function[{tag, message, severity, data},
						Module[{srcs},
							srcs = { data[Source] } ~Join~ Lookup[data, "AdditionalSources", {}];
							Function[{src},
								<|"code" -> tag,
				              "message" -> plainify[message],
				              "severity" -> lintSeverityToLSPSeverity[severity],
				              "range" -> <|"start" -> <|"line" -> (src-1)[[1, 1]], "character" -> (src-1)[[1, 2]]|>,
				                                                                                     (* end is exclusive *)
				                           "end" -> <|"line" -> (src-1)[[2, 1]], "character" -> (src-1)[[2, 2]]+1|>|>,
				              "source" -> "CodeTools Lint"
				            |>] /@ srcs]] @@@ lints;

	diagnostics = Flatten[diagnostics];

	<| "jsonrpc" -> "2.0",
		"method" -> "textDocument/publishDiagnostics",
		"params" -> <| "uri" -> uri,
							"diagnostics" -> diagnostics |> |>
]




(*

do not send `` markup
do not send ** markup
do not send ?? markup
FIXME: what to do about ?? contents?

\n newlines are ok to send

*)
plainify[s_String] := StringReplace[s, {
  RegularExpression["``(.*?)``"] :> "$1",
  RegularExpression["\\*\\*(.*?)\\*\\*"] :> "$1",
  RegularExpression["\\?\\?(.*?)\\?\\?"] :> "$1" }]








End[]

EndPackage[]
