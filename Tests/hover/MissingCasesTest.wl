onlyUsage::usage = "Test function with only usage."

(* Function is defined with no usage message *)
noFunctionUsage[x_Integer] := x^2;

noFunctionUsage[x];

(* Function is not defined, but only has usage message *)
onlyUsage[];

(* Function is not defined and has usage message *)
noFunction[x];