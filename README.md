# LSPServer

LSPServer is a paclet that implements the Language Server Protocol for Wolfram Language.

[Language Server Protocol](https://microsoft.github.io/language-server-protocol/)


## Installing

Install LSPServer and dependencies from the public paclet server:
```
In[1]:= PacletUpdate["AST", "Site" -> "http://pacletserver.wolfram.com", "UpdateSites" -> True]
			PacletUpdate["Lint", "Site" -> "http://pacletserver.wolfram.com", "UpdateSites" -> True]
			PacletUpdate["LSPServer", "Site" -> "http://pacletserver.wolfram.com", "UpdateSites" -> True]

Out[1]= Paclet[AST,0.2,<>]
Out[2]= Paclet[Lint,0.2,<>]
Out[3]= Paclet[LSPServer,0.2,<>]
```


## Setup

LSPServer depends on the AST paclet and the Lint paclet. Make sure that the paclets can be found on your system:
```
In[1]:= Needs["AST`"]
			Needs["Lint`"]
			Needs["LSPServer`"]
```

[AST on stash.wolfram.com](https://stash.wolfram.com/projects/COD/repos/ast/browse)

[Lint on stash.wolfram.com](https://stash.wolfram.com/projects/COD/repos/lint/browse)


## Building

LSPServer uses a Wolfram Language kernel to build a `.paclet` file.

LSPServer uses CMake to generate build scripts.

Here is an example transcript using the default make generator to build LSPServer:
```
cd lspserver
mkdir build
cd build
cmake ..
cmake --build . --target paclet
```

The result is a directory named `paclet` that contains the WL package source code and a built LSPServer `.paclet` file for installing.

You may see an error because the default path to `WolframKernel` may not be correct.

Here is the cmake command using supplied values for `WOLFRAMKERNEL`:
```
cmake -DWOLFRAMKERNEL=/path/to/WolframKernel ..
```

Here are typical values for the variables:
* `WOLFRAMKERNEL` `/Applications/Mathematica.app/Contents/MacOS/WolframKernel`

Here is the build directory layout after building LSPServer:

```
paclet/
  LSPServer/
    Kernel/
      LSPServer.wl
    PacletInfo.m
    ...
```

### Windows

It is recommended to specify `wolfram.exe` instead of `WolframKernel.exe`.

`WolframKernel.exe` opens a new window while it is running. But `wolfram.exe` runs inside the window that started it.


## Testing

You can simulate the client on the command-line:

```
brenton2maclap:build brenton$ echo "{\"method\":\"textDocument/didOpen\", \"params\":{ \"textDocument\": { \"uri\":\"file:///Applications/Mathematica120.app/Contents/AddOns/Applications/ClusterIntegration/CCSWin.m\" } }}" | /Applications/Mathematica120.app/Contents/MacOS/WolframKernel -noprompt -rawterm -run \(Needs[\"LSPServer\`\"]\;LSPServer\`StartServer[\"test.txt\"]\)
```
