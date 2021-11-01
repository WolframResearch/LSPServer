f::usage = "f[x] has multiple definition.";


f[x_] := ppp[x] /; x > 0;
f[x_String]:=x <> "Test";
f[x_String]:=x <> "Test";
f[x_String]/;x :=x <> "Test-String";
f[x_Number] := x;

(* UpSetDelayed *)
p[q[x_]] ^:= pq[x]

p[q[2]]
(* TagSetDelayed *)
g/:f2[g[x_]]:=f2g[x]

g[x]

f2[]

    g/:f2[g[x_]]:=f2g[x]

(* Function with constrained args *)
addTwo[a_Integer,b_] := (a+b);

addTwo;