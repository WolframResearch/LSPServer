BeginPackage["LSPServer`Utils`"]

normalizeURI

Begin["`Private`"]

(*
input:  "file:///Users/brenton/development/stash/COD/ast/build/test.m"
return:  "/Users/brenton/development/stash/COD/ast/build/test.m"
*)
normalizeURI[uri_String] := FileNameJoin[FileNameSplit[URL[uri]]]

End[]

EndPackage[]
