# Building

LSPServer uses a Wolfram Language kernel to generate code at build time and a C++ compiler to compile an executable.

LSPServer uses C++11 features and requires a compiler that can support at least C++11.

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
cmake -DMATHEMATICA_INSTALL_DIR=/Applications/Mathematica120.app/Contents/ ..
cmake --build . --target paclet
```

On Windows:

```
cmake -DMATHEMATICA_INSTALL_DIR="C:/Program Files/Wolfram Research/Mathematica/12.0" ..
cmake --build . --target paclet
```
