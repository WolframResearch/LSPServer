
# Diagnostics

## Server

Handle when LSPServer is not installed

Right now, the command-line is:
```
Needs["LSPServer`"];LSPServer`StartServer[]
```

But what if LSPServer is not installed?


Right now, kernel hangs after returning ``LSPServer`StartServer[]`` unevaluated, waiting for input


Cannot simply do:
```
Needs["LSPServer`"];LSPServer`StartServer[];Exit[1]
```
because then I have no way of displaying error dialog to user.


Could do:
```
Needs["LSPServer`"];LSPServer`StartServer[];Pause[20];Exit[1]
```
which will trigger timeout after 10 seconds and show dialog, but this is getting really ugly and esoteric.


Could try to do:
```
Needs["LSPServer`"];LSPServer`StartServer[];Print["Content-Length: 64\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":0,\"error\":{\"code\":-32603,\"message\":\"XXX\"}}"];Exit[1]"
```
but this getting REALLY ugly now.


## Client

I at least want to be able to kill the kernel process that is hanging when LSPServer is not installed.

Clients should be able to do this.


## Sublime

Resolution of sublime issue is that I should implement the can_start function of the API.

This is not ideal for Wolfram, because an implementation of can_start basically needs to try to start a kernel itself.

But implementing can_start is also complicated:

Requires starting a separate kernel process, and now managing that process manually.

Exiting this first kernel and starting the actual server kernel may expose race condition in license handling, may need to wait 1 second, etc.

After paclets are installed, it would be silly to start a temp kernel for can_start every time, so need some kind of caching?

Just so complicated.

There is no direct way of killing kernel, and Sublime LSP developers do not really understand the issue that I was asking about.

Related Sublime issue:
https://github.com/sublimelsp/LSP/issues/1872



## VSCode

A bug has been fixed in VSCode to allow clients to kill the server process.

Related VSCode issue:
https://github.com/microsoft/vscode-languageserver-node/issues/834











