# LSPServer

LSPServer is a package that implements the Language Server Protocol for Wolfram Language.

[Language Server Protocol](https://microsoft.github.io/language-server-protocol/)


## Setup

LSPServer depends on the CodeParser paclet and the CodeInspector paclet. Make sure that the paclets can be found on your system:
```
In[1]:= Needs["CodeParser`"]
			Needs["CodeInspector`"]
      Needs["CodeFormatter`"]
			Needs["LSPServer`"]
```

[CodeParser on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)
[CodeInspector on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)
[CodeFormatter on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)
[CodeParser on github.com](https://github.com/<<TODO_placeholder_for_actual_link>>)

Install LSPServer and dependencies from the CodeTools paclet server:
```
In[1]:= PacletUpdate["CodeParser", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
      PacletUpdate["CodeInspector", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
      PacletUpdate["CodeFormatter", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]
      PacletUpdate["LSPServer", "Site" -> "<<TODO_placeholder_for_actual_link>>", "UpdateSites" -> True]

Out[1]= PacletObject[CodeParser, 1.0, <>]
Out[2]= PacletObject[CodeInspector, 1.0, <>]
Out[3]= PacletObject[CodeFormatter, 1.0, <>]
Out[4]= PacletObject[LSPServer, 1.0, <>]
```


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

Specify `MATHEMATICA_INSTALL_DIR` if you have Mathematica installed in a non-default location:
```
cmake -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica111.app/Contents/ ..
cmake --build . --target paclet
```
