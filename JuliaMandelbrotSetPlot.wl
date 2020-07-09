cf = Compile[{{z, _Complex}, {c, _Complex}}, 
   Module[{zn = z, x = Re[z], y = Im[z], k, abs}, 
    Do[zn = zn^2 + c, 9]; 
    If[(abs = Abs[zn]) > 2, 
     k = 10 - 
       IntegerPart[(Log[(2 Log[abs])/Log[2]]/Log[2.])]; {0.5*(1 + 
         Sin[k]), 0.5*(1 + Cos[k + \[Pi]/6]), 
      0.5*(1 - Cos[k - \[Pi]/6]), x, y}, {0, 0, 0, x, y}]], 
   CompilationTarget -> "C", RuntimeAttributes -> Listable, 
   Parallelization -> True, RuntimeOptions -> "Speed"];
juliasetplot[c_Complex]:=Graphics[Map[{RGBColor[#[[;; 3]]], Point@#[[4 ;;]]} &, Flatten[cf[Table[x + y*I, {x, -1.5, 1.5, 0.01}, {y, -1.2, 1.2, 0.01}],c], 1]]];
cf1 = Compile[{{z, _Complex}, {c, _Complex}}, 
   Module[{zn = z, x = Re[c], y = Im[c], k, abs}, 
    Do[zn = zn^2 + c, 10]; 
    If[(abs = Abs[zn]) > 2, 
     k = 11 - 
       IntegerPart[(Log[(2 Log[abs])/Log[2]]/Log[2.])]; {0.5*(1 + 
         Sin[k]), 0.5*(1 + Cos[k + \[Pi]/6]), 
      0.5*(1 - Cos[k - \[Pi]/6]), x, y}, {0, 0, 0, x, y}]], 
   CompilationTarget -> "C", RuntimeAttributes -> Listable, 
   Parallelization -> True, RuntimeOptions -> "Speed"];
mandelbrotsetplot[z_Complex]:=Graphics[Map[{RGBColor[#[[;; 3]]], Point@#[[4 ;;]], 
    PointSize[Tiny]} &, 
  Flatten[cf1[z, 
    Table[x + y*I, {x, -2, 1, 0.01}, {y, -1.5, 1.5, 0.01}]], 1]]];