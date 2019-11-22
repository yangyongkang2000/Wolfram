Module[{list}, 
 list[x_] = 
  Flatten[Join[{#}, {Join[{#1[[1]]}, Differences[#1]]}] & /@ 
    Module[{f}, 
     f[x_] = Evaluate[
       Normal@NonlinearModelFit[{{80, 32.49}, {100, 33.28}, {200, 
             34.5}, {300, 35.01}, {400, 35.33}}, #, {a, b}, 
           x] & /@ {Log[Abs[a*x + b]], a - b/(x), a - b/Sqrt[x]}]; 
     Flatten[(t /. 
          Table[Solve[
            2.5*(t/(10^3*(Total[Range[1.5, 5, 0.5][[1 ;; n]]]/
                    101))) == 
             f[1213/Total[Range[1.5, 5, 0.5][[1 ;; n]]]][[#]] + 
              RandomReal[{x - 0.01, x}], t, Reals], {n, 1, 8}])] & /@ 
      Range[3]], 1]; 
 Manipulate[
  Dataset@Table[<|"逻辑回归T累计时间/s" -> list[n][[1]][[k]], 
     "逻辑回归T时间差/s" -> list[n][[2]][[k]], 
     "双曲线回归T累计时间/s" -> list[n][[3]][[k]], 
     "双曲线回归T时间差/s" -> list[n][[4]][[k]], 
     "根号双曲线回归T累计时间/s" -> list[n][[5]][[k]], 
     "根号双曲线回归T时间差/s" -> list[n][[6]][[k]]|>, {k, 1, 8}], {n, -5, 5, 
   0.01}]]