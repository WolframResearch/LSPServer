
## 1.7.1 - 5 September, 2022

### Fixes

Fix $MaxLicenseProcesses with Infinity crashes LSPServer

https://github.com/WolframResearch/vscode-wolfram/issues/14

Fix 426896: Expand Selection and Shrink Selection fails in VSCode


## 1.7 - 4 July, 2022

Fix handling non-BMP PUA characters

Address issues here: https://github.com/WolframResearch/vscode-wolfram/issues/10

Only compare major.minor when doing version checks and only do build
date check if versions are identical

Add foldingRange

13.1 syntax updates


## 1.6 - 12 May, 2022

Add function usage and function definition patterns to hover.


### Fixes

Fix handling stale content for textDocument/documentSymbol and textDocument/references


## 1.5 - 7 Mar, 2022

Add ProcessDirectory option to RunServerDiagnostic

Add Hierarchical Document Symbol support (outlines)

Add a mini server to diagnostics to test for various bugs before doing actual diagnostics

Work-around serious bug 419428 for now

Internal\`WithLocalSettings is broken, so manually insert UnlockQueue[] calls

Use 0.4 seconds, same as default value of spelling squiggly in FE

Handle ScheduledTask in earlier versions before it held its expr

FromDateString was introduced in 12.3, so use a version check

https://github.com/WolframResearch/vscode-wolfram/issues/8

Create a special message for the error and put in queue as regular traffic

This guarantees the proper order of things being read

initializationOptions may be Null

Handle workspace/didChangeConfiguration notification

https://github.com/WolframResearch/LSPServer/issues/1

13.0.1 syntax updates

Only try reporting stdout / stderr for up to 1 second


### Fixes

Fix race condition with stdio error being checked before all previous traffic has been processed

Fix issues found by running with Jupyter Lab LSP


## 1.4 - 25 Oct, 2021

Add Startup Message handling

There may be internal errors in LSPServer that emit messages during ``Needs["LSPServer`"] ``

These messages are exceptionally hard to handle because any code for handling has not yet been loaded

The messages may cause unexplained hangs in clients

So manually set $Messages to a tmp file and then handle the messages later


Do not allow PacletManager to participate in finding \`Generate\` files


Add more features to RunServerDiagnostic:

Print given kernel path and kernel path to-be-started, and check they are the same.

Add a 30 second timeout for the while diagnostic.

Keep track of how long initialize takes, and error if greater than 10 seconds


Use Internal\`WithLocalSettings to protect against aborts when doing LockQueue / UnlockQueue


RunServerDiagnostic: reduce "must be run with same kernel" to warning


If there were messages when loading LSPServer\`, then report in the diagnostic


use FromDateString with `"Language" -> "en"` for more robust date parsing


### Fixes

Various fixes for RunServerDiagnostic:

BinaryWrite may fail, so check return value and quiet `BinaryWrite::errfile`

If `arr == {}` returns unevaluated, then whole Which returns unevaluated


## 1.3 - 30 Aug, 2021

Notes on compatibility have been added to docs/compatibility.md

work around bug 410895, all quotes are stripped from StartProcess on Windows

Experimental support for sockets

Experimental support for multiple clients


## 1.2 - 25 Mar, 2021

Allow `textDocument/definition` to lookup symbols with or without contexts

e.g., allow foo\`bar to look up definition for bar and vice versa

Allow `textDocument/hover` to work with symbols and display their usage messages.

The usage messages are parsed directly from the linear syntax.

Add a build step to generate a file ReplacePUA.wl that provides a map for converting PUA characters to ASCII approximations.

Use LongNames.wl to provide a text replacement for PUA characters that cannot render properly in other editors

Add a background thread for reading from stdin. This thread will write to a queue that the server will read on its main thread.

The server will look at the queue and determine if any of the messages can be discarded.

For example, a long sequence of `textDocument/didChange` requests do not need to be processed. Only the final one needs to be processed.

Similarly, other requests that may be in the queue before a `textDocument/didChange` may also be discarded.

Do a little work on only reparsing if needed.

Handle `textDocument/documentSymbol`

Handle more color literals and also handle `textDocument/colorPresentation`

LSP clients will have a 10 second timeout for starting the kernel. After that, a dialog is presented explaining that there is a problem and diagnostic code is presented to run in a notebook.

Introduce delays for running various methods.

Implementation of `textDocument/selectionRange`

Initial implementation of semantic tokens


## 1.1 - 16 Sep, 2020

Initial work with formatting

Wire up tabSize and insertSpaces options

Add range formatting

Introduce new BracketMatcher UI

Handle workspace/didChangeWachedFiles that comes from IntelliJ plugin

Add implicit tokens for ExpectedOperands


## 1.0 - 2 Apr, 2020

Add preliminary implementation of hover

Add a native library for handling stdio, and remove the Python proxy script

Keep all content as ByteArrays and introduce publishing of implicit Times

Add versions notification

Add a definitions provider

Display other implicit tokens


Enable sending implicit 1, implicit All, and implicit Null to a client.
A little language has been invented for representing implicit tokens,
and combinations thereof, with a single character.


## 0.15 - 15 Jan, 2020

Add Creator field

Quit kernel if any messages on startup

Add color provider

Require using `File[]` wrapper


## 0.14 - 28 Oct, 2019

Add support for CodeActions

Add ConfidenceLevel setting

Handle other CodeAction commands

Remove Lints that are shadowed

Only convert bytes to string if debug logging


### Fixes

Handle `$/` messages gracefully

Fix handling of non-ASCII characters


## 0.13 - 16 Sep, 2019

Use `"AdditionalSources"` for Lints


## 0.12 - 5 Aug, 2019

Add some error handling for missing files and directories.

Add --extra argument for extra arguments to WolframKernel

Various bug fixes.


## 0.11 - 10 Jun, 2019

Added LSPServer paclet to CodeTools suite.

Various bug fixes.
