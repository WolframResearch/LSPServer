

# Setup

`LSPServer` depends on the `AST` paclet and the `Lint` paclet. Make sure that both `Needs["AST``"]` and `Needs["Lint``"]` succeed on your system.






# Building

```
cd lspserver
mkdir build
cd build
cmake -DWOLFRAMKERNEL=/path/to/WolframKernel ..
cmake --build . --target paclet
```

After building, there will be a .paclet file in the `lspserver/build/paclet` directory





# Installing

This will install the LSPServer paclet on your system:

```
PacletInstall["/path/to/build/paclet/file"]
```



Or if you want to run directly from sources, you could put this in your `init.m` file:

```
PacletDirectoryAdd["/path/to/lspserver/LSPServer"]
```





# Testing

Make sure `python` is on your path.

Make sure that `Needs["LSPServer``"]` succeeds.



You can simulate the client on the command-line:

```
brenton2maclap:build brenton$ echo "{\"method\":\"textDocument/didOpen\", \"params\":{ \"textDocument\": { \"uri\": \"file:///Applications/Mathematica120.app/Contents/AddOns/Applications/ClusterIntegration/CCSWin.m\" } }}" | /Applications/Mathematica120.app/Contents/MacOS/WolframKernel -noprompt -rawterm -run \(Needs[\"LSPServer\`\"]\;LSPServer\`StartServer[\"test.txt\"]\)


{	"jsonrpc":"2.0",	"method":"textDocument\/publishDiagnostics",	"params":{		"uri":"file:\/\/\/Applications\/Mathematica120.app\/Contents\/AddOns\/Applications\/ClusterIntegration\/CCSWin.m",		"diagnostics":[			{				"code":"ImplicitTimesBlanks",				"message":"Implicit Times between ___ and _. Did you mean ___ ?",				"severity":1,				"range":{					"start":{						"line":618,			"character":8					},					"end":{						"line":618,						"character":12					}				},				"source":"CodeTools Lint"			},			{				"code":"UnusedBlockVariables",				"message":"Unused variables in Block: runAlways.",				"severity":2,				"range":{			"start":{						"line":202,						"character":23					},					"end":{						"line":202,						"character":32					}				},			"source":"CodeTools Lint"			},			{				"code":"UnusedBlockVariables",				"message":"Unused variables in Block: runAlways.",				"severity":2,				"range":{					"start":{						"line":235,			"character":59					},					"end":{						"line":235,						"character":68					}				},				"source":"CodeTools Lint"			},			{				"code":"UnusedBlockVariables",				"message":"Unused variables in Block: nopts.",				"severity":2,				"range":{				"start":{						"line":478,						"character":22					},					"end":{						"line":478,						"character":27					}				},			"source":"CodeTools Lint"			}		]	}}
brenton2maclap:build brenton$ 
```







