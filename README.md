# LSPServer

LSPServer is a package that implements the Language Server Protocol for Wolfram Language.

[Language Server Protocol](https://microsoft.github.io/language-server-protocol/)


## Setup

LSPServer depends on the CodeParser paclet and the CodeInspector paclet. Make sure that the paclets can be found on your system:
```
Needs["CodeParser`"]
Needs["CodeInspector`"]
Needs["CodeFormatter`"]
Needs["LSPServer`"]
```

[CodeParser on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)
[CodeInspector on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)
[CodeFormatter on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)
[LSPServer on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)

Install LSPServer and dependencies from the CodeTools paclet server:
```
PacletUpdate["CodeParser", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
PacletUpdate["CodeInspector", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
PacletUpdate["CodeFormatter", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
PacletUpdate["LSPServer", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
```


## Troubleshooting

If there is any output written to the kernel's stdout, then this will break the protocol with LSP clients.

For example, any messages that are produced when running the startup code will break the protocol and must be fixed first.

```
/Applications/Mathematica.app/Contents/MacOS/WolframKernel -noprompt -run Needs["LSPServer`"];LSPServer`StartServer[]

Get::noopen: Cannot open CodeParser`.

Needs::nocont: Context CodeParser` was not created when Needs was evaluated.

Get::noopen: Cannot open CodeParser`.

Needs::nocont: Context CodeParser` was not created when Needs was evaluated.

Get::noopen: Cannot open CodeParser`.

General::stop: Further output of Get::noopen will be suppressed during this calculation.

Needs::nocont: Context CodeParser` was not created when Needs was evaluated.

General::stop: Further output of Needs::nocont will be suppressed during this calculation.
```



Make sure that the required paclets are up-to-date:
CodeParser
CodeInspector
CodeFormatter
LSPServer


remove older paclets




### Debugging

### Server settings

Turn on debug logging from the kernel.

Give a string argument to StartServer[]. This is a directory that kernel logs will be written to.

```
Needs["LSPServer`"];LSPServer`StartServer["/path/to/log/directory/"]
```








