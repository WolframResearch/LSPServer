

Module[{a},
(*      ^ scoped *)
  a
(* ^ scoped *)
]


Module[{a},
(*      ^ unused *)
   Module[{a},
(*         ^ scoped, shadowed *)
     a
(*    ^ scoped, shadowed *)
   ]
]


g[x_] :=
(*^ scoped *)
   Module[{a},
(*         ^ scoped *)
      a + x
(*    ^ scoped *)
(*        ^ scoped *)
   ]


  he:ThreeJSymbol[{j1_, m1_}]:= {j1, m1}
(*^ unused *)
(*                 ^ scoped *)
(*                      ^ scoped *)
(*                               ^ scoped *)
(*                                   ^ scoped *)


Table[i, {i, nblks}]
(*    ^ scoped *)
(*        ^ scoped *)
(*           ^ NOT scoped*)

Table[i, {i, nblks}, {blksize}]
(*    ^ scoped *)
(*        ^ scoped *)
(*           ^ NOT scoped*)
(*                    ^ NOT scoped*)


Block[{pred},
(*     ^ unused *)
  Function[{pred},
(*           ^ shadowed *)
      pred
(*    ^ shadowed *)
   ]
]

Block[{pred},
  pred_ :> pred
(* ^ shadowed *)
(*         ^ shadowed *)
]


Compile[{{rows, _Integer, 0}, {cols, _Integer, 0}},
(*        ^ scoped *)
(*                             ^ scoped *)
  Table[cols*(i - 1) + j, {i, rows}, {j, cols}]
(*      ^ scoped *)
(*                            ^ scoped *)
(*                                       ^ scoped *)
]


Module[{func},
(*      ^ scoped *)
  func /: Map[func, _] := lst;
(*^ scoped *)
(*            ^ scoped *)
]


Compile[{{staterules,_Integer,2},{colorrules,_Integer,2},{positionrules,_Integer,3},{steps,_Integer}},
(*        ^ unused *)
(*                                ^ unused *)
(*                                                        ^ unused *)
(*                                                                                   ^ unused *)
   body
   ,
   {{Extract[tapelist[[step]],position],_Integer}}
]


Module[{a},
(*      ^ unused*)
   aa
(* ^ NOT scoped*)
]


Function[{a}, aa]
(*        ^ unused*)
(*            ^ NOT scoped*)


Do[5 + 6, {i, 8, 9}]
(*         ^ unused*)


f[a_] := 4 + 3
(*^ unused*)


f[args___] := {args}
(*^ scoped*)
(*             ^ scoped*)


Module[{a},
(*      ^ scoped*)
   With[{a = a},
(*       ^ unused, shadowed*)
(*           ^ scoped*)
      j
   ]
]


Function[Null, Function[Null, Null, {HoldAllComplete}], {HoldAllComplete}]
(*       ^ NOT semantic highlighted*)
(*                      ^ NOT semantic highlighted*)
(*                            ^ NOT semantic highlighted*)


With[{baseTy = baseTy1},
(*    ^ scoped*)
(*             ^ NOT scoped*)
   Function[{Typed[arg,"ArrayList"[baseTy]], Typed[elem, baseTy]},
(*                 ^ scoped*)
(*                                 ^ scoped*)
(*                                                 ^ scoped*)
(*                                                       ^ scoped*)
      Append[arg, elem]
(*           ^ scoped*)
(*                ^ scoped*)
   ]
]




Do[
  {i, j}
(* ^ scoped*)
(*    ^ scoped*)
  ,
  {i, 1, j}
(* ^ scoped*)
(*       ^ NOT scoped*)
  ,
  {j, 1, i}
(* ^ scoped*)
(*       ^ scoped*)
]






(*
Condition
*)

g[x_] /; MatchQ[x, g[a_]] := x
(*^ scoped*)
(*              ^ scoped*)
(*                   ^ NOT semantic highlighted*)
(*                           ^ scoped*)


g[x_] /; MatchQ[y, g[a_]] := z
(*^ unused*)
(*              ^ NOT scoped*)
(*                   ^ NOT scoped*)
(*                           ^ NOT scoped*)


g[x_] /; MatchQ[x, g[a_]] :=
(*^ scoped*)
(*              ^ scoped *)
(*                   ^ NOT semantic highlighted *)
   Module[{a},
(*         ^ scoped *)
      a
(*    ^ scoped *)
   ]


f[args___ /; goodQ[args]] := {args}
(*^ scoped*)
(*                 ^ scoped*)
(*                            ^ scoped*)


f[args___] := {args} /; goodQ[args]
(*^ scoped*)
(*             ^ scoped*)
(*                            ^ scoped*)


Module[{x},
   g[x_] /; MatchQ[x, g[a_]] := x
(*   ^ shadowed*)
(*                 ^ shadowed*)
(*                      ^ NOT scoped*)
(*                              ^ shadowed*)
]








(*
NOT WORKING YET:
*)

Switch[x,
   a_ /; f[a],
(* ^ scoped*)
(*         ^ scoped*)
      6
]








g[x_] := k[x_]
(* ^ scoped*)
(*         ^ error*)







(*
NOT WORKING YET:
*)

g[x_] /; MatchQ[x, g[a_] /; h[a]] := x + a
(* ^ scoped*)
(*              ^ scoped*)
(*                   ^ scoped*)
(*                            ^ scoped*)
(*                                   ^ scoped*)
(*                                       ^ NOT scoped*)





g[x_] /; MatchQ[x, g[a_] /; h[a]] := x + k[a_]
(* ^ scoped*)
(*              ^ scoped*)
(*                   ^ scoped*)
(*                            ^ scoped*)
(*                                   ^ scoped*)
(*                                         ^ NOT scoped and no error*)






(*
Non-linear patterns
*)

sqlEqual[a_, a_] := True
(*       ^ scoped*)
(*           ^ scoped*)






(*
Sum
*)

Sum[k + j, {k,0,Infinity}, {j, 1, 2}]
(*  ^ scoped*)
(*      ^ scoped*)
(*          ^ scoped*)
(*                          ^ scoped*)


Sum[Pochhammer[a,k] * Pochhammer[b,k] * x^k/(Pochhammer[c,k] * k!) + j,
(*               ^ scoped*)
(*                                 ^ scoped*)
(*                                        ^ scoped*)
(*                                                        ^ scoped*)
(*                                                             ^ scoped*)
                  {k,0,Infinity}, {j, 1, 2}, VerifyConvergence->False + k + j]
(*                 ^ scoped*)
(*                                                                      ^ NOT scoped*)
(*                                                                          ^ NOT scoped*)







(*
NOT WORKING YET:
*)

  Experimental`OptimizedExpression /:
(*^ scoped *)
  HoldPattern[ SetDelayed[ lhs_, Experimental`OptimizedExpression[body_] ] ] :=
(*                         ^ scoped*)
(*                               ^ scoped *)
(*                                                                ^ scoped*)
    lhs := body;
(*  ^ scoped*)
(*         ^ scoped*)












xxx /: Part[xxx, yyy] := foo
(*     ^ it is convenient to mark Part as having a definition here *)
bar[fileInfo_] := Block[{path = fileInfo[[67]]}, jjj]
(*                                      ^ and then asking for the Source of the Part syntax here was causing a crash*)








  interpreter_Interpreter[input_:Missing["NoInput"]] := s
(*^ unused*)
(*            ^ defined*)
(*                        ^ unused*)







 (#)&
(*^ scoped*)

((#)&)&
(*^ shadowed*)



((##234)&)&
(*^ shadowed*)


Function[# + 1]


  #
(*^ error*)






CurrentValue /: (CurrentValue[target_, option_] = value_) := With[{t=target, o=option},
  MathLink`CallFrontEndHeld[FrontEnd`SetValue[FEPrivate`Set[CurrentValue[t, o], value]]]; CurrentValue[t, o]]



With[{a = 2, p}, foo[]]





foo[a_] :=
Module[{a},
(*      ^ error*)
   a + 1
]




