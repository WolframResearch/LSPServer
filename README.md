# LSPServer

LSPServer is a package that implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) for Wolfram Language and allows a Wolfram Language kernel to run as an LSP server.

LSPServer implements several LSP features:
* Code diagnostics
* Suggestions for fixes
* Formatting files and selections
* Semantic highlighting
* Expand / shrink selection
* Outline
* Color swatches
* Symbol references
* Documentation on hover

This repo is for users who are interested in adding LSP support for Wolfram Language to LSP clients.

There are official Wolfram LSP clients for [Sublime Text](https://github.com/WolframResearch/Sublime-WolframLanguage) and [Visual Studio Code](https://github.com/WolframResearch/vscode-wolfram).


## Setup

LSPServer depends on [CodeParser paclet](https://github.com/WolframResearch/codeparser), [CodeInspector paclet](https://github.com/WolframResearch/codeinspector), and [CodeFormatter paclet](https://github.com/WolframResearch/codeformatter).

LSPServer and its dependencies are included in Mathematica 13.0 and above.

Install LSPServer paclet and dependencies from the public paclet server:
```
PacletInstall["CodeParser"]
PacletInstall["CodeInspector"]
PacletInstall["CodeFormatter"]
PacletInstall["LSPServer"]
```

[Build and install the LSPServer paclet locally](HowToBuild.md)


## Using LSPServer

99% of users will not need to worry about using LSPServer directly. LSPServer is used internally when an LSP client launches a Wolfram kernel as an LSP server. This all happens in the background.

But it can be useful to run LSPServer when developing a new LSP client.

Create a file named server.wl:
```
Needs["LSPServer`"]

StartServer[]
```

And run from the command-line:
```
brenton@brenton2maclap % WolframKernel -noprompt -run Get\[\"server.wl\"\]
14:03:48.605 $CommandLine: {WolframKernel, -noprompt, -run, Get["server.wl"]}
14:03:48.607 


14:03:48.608 $commProcess: StdIO
14:03:48.608 


14:03:48.608 $ProcessID: 54603
14:03:48.609 


14:03:48.609 $ParentProcessID: 54582
14:03:48.609 


14:03:48.609 Starting server... (If this is the last line you see, then StartServer[] may have been called in an unexpected way and the server is hanging.)
14:03:48.610 
```

Notice the proper character escapes on the command-line.

The kernel process is blocked waiting on input to its stdin.

Properly formed LSP JSON-RPC can be sent to the kernel, and the kernel would send its response to stdout.


## Troubleshooting

Make sure that the paclets can be found on your system:
```
Needs["LSPServer`"]
```


### Server settings

Turn on debug logging from the kernel.

Give a string argument to StartServer[]. This is a directory that kernel logs will be written to.

```
Needs["LSPServer`"];LSPServer`StartServer["/path/to/log/directory/"]
```
