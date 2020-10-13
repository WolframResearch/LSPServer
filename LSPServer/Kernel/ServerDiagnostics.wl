BeginPackage["LSPServer`ServerDiagnostics`"]

Begin["`Private`"]

Needs["LSPServer`"]
Needs["LSPServer`Utils`"]


RunServerDiagnostic[command:{_String...}] :=
  Module[{},
    (*
    launch a child kernel with same command args that a client has
    this kernel will be the client
    scheduled task for 3 seconds to kill child kernel if no response
    send traffic to kernel, read response
    read stderr also
    then shutdown child kernel
    *)
    True
  ]

End[]

EndPackage[]
