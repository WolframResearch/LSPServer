BeginPackage["LSPServer`Workspace`"]

Begin["`Private`"]


Needs["LSPServer`"]
Needs["LSPServer`Utils`"]
Needs["CodeParser`"]
Needs["CodeParser`Utils`"]


handleContent[content:KeyValuePattern["method" -> "workspace/didChangeWatchedFiles"]] :=
      {}


End[]

EndPackage[]
