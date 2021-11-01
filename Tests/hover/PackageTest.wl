(* ::Package:: *)

BeginPackage["MyPackages`Test`"]

testFunction::usage = "Usage message of testFunction." ;
multiUsageFunction::usage = "First usage message of multiUsageFunction." ;
multiUsageFunction::usage := "Second usage message of multiUsageFunction." ;

Begin["`Private`"]

testFunction [a_] [b_] := (a+b)/2;
p = testFunction[3][4];

    (* We are not going to search one level down *)
    Begin["`Context1`"]

        testFunction [a_] := (a)/2;

    End[]

(* Function with UpSetDelayed *)
f[g[x_]] ^:= fg[x]

f[g[2]]

Plot[Sin[x],{x, 0, Pi}]
(* Function with TagSetDelayed *)
g /: f[g[x_]] := fg[x];

f[g[2]]

    foo[] := xxx

foo[]

multiUsageFunction[x_] := x+1;


End[]
EndPackage[]