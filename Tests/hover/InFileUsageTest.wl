mySin::usage = "mySin is just a sin function."
myTan::usage = "myTan is Tan function."
myCos::usage = "\!\(\*RowBox[{\"Cos\", \"[\", StyleBox[\"z\", Bold], \"]\"}]\) gives the cosine of \!\(\*StyleBox[\"z\", \"TI\"]\). "


mySin[x_]:= Sin[x];

myCos[x_]:= Cos[x];

    myTan[x_]:= Tan[x];



a::usage = "Test for single usage."
a[]:=2;
a[]


aa::usage := "Usage with SetDelay";
aa[x_] := 2x;
aa[2]


aaa::foo = "test"
aaa[]

aaaa::foo := "test"
aaaa[]


bb::usage::English = "test"
bb[x_] := 1+2
bb[]


bbb::usage::English := "test"
bbb[x_] := 1+2
bbb[]


f::usage = "Usage 1.";
f::usage := "Usage 2.";
f::usage::English = "Usage 3"
f::usage::English := "Usage 4."

f[x_] := ppp[x] /; x > 0;
f[x_String]:=x <> "Test";
f[x_String]:=x <> "Test";
f[x_String]/;x :=x <> "Test-String";
f[x_Number] := x;
f[] = 42;

linearSyntaxTest::usage = "\!\(\*RowBox[{\"Sin\", \"[\", StyleBox[\"z\", Bold], \"]\"}]\) gives the sine of \!\(\*StyleBox[\"z\", \"TI\"]\). "
linearSyntaxTest[x_]:= 2x;