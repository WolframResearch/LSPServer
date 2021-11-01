subFnUsage::usage = "SubValue function with usage."

subFn[a_][b_] := a+b;

subFn[3][4]

subFnUsage[a_][b_] := a+b+2;

subFnUsage[3][4]

g::usage = "g is a function with UpValue."

g /: f[g[x_]] := h[x]
f[g] ^:= h[1]
g[5]

downValFn::usage = "downValFn is a function with DownValue."
downValFn[x_ /; x > -2] := g1[x]
downValFn[x_ /; x < 2] := g2[x]
downValFn[0]