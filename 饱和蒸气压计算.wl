FormFunction[{"\[CapitalDelta]p/KPa" -> "String", "温度" -> "String"}, 
 Module[{temp = ToExpression[(#"温度")], 
    press = ToExpression[(#"\[CapitalDelta]p/KPa")], plist, arrary, f,
     lm}, plist = 102.25 - press; 
   arrary = {temp[[#]] + 273.15, Log[plist[[#]]*10^3]} & /@ Range[10];
    lm = LinearModelFit[arrary, 1/T, T]; 
   f[T_] = Normal[LinearModelFit[arrary, 1/T, T]];
   TabView[{"表格" -> 
      Dataset@Table[<|"温度" -> temp[[k]], 
         "\[CapitalDelta]p/KPa" -> press[[k]], 
         "1/T*\!\(\*SuperscriptBox[\(10\), \(3\)]\)" -> 
          1/(press[[k]] + 273.15)*10^3, "饱和蒸汽压P/KPa" -> plist[[k]], 
         "ln(P)" -> Log[plist[[k]]]|>, {k, 1, 10}], 
     "散点图" -> 
      ListPlot[
       Labeled[#, 
          StringJoin["(", ToString[#[[1]]], ",", ToString[#[[2]]], 
           ")"]] & /@ arrary, PlotStyle -> PointSize[0.03], 
       ColorFunction -> Hue, PlotTheme -> "Detailed"], 
     "拟合图" -> (Show[
        ListPlot[
         Labeled[#, 
            StringJoin["(", ToString[#[[1]]], ",", ToString[#[[2]]], 
             ")"]] & /@ arrary, PlotStyle -> PointSize[0.03], 
         ColorFunction -> Hue, PlotTheme -> "Detailed"], 
        Plot[f[T], {T, 340, 350}]]), 
     "数据分析" -> 
      Dataset[{<|
         "拟合乙醇摩尔蒸发焓\[CapitalDelta]H/KJ" -> 
          D[f[T], T]*T^2*8.3144621/10^3, 
         "决定系数 \!\(\*FormBox[SuperscriptBox[\(R\), \(2\)],
TraditionalForm]\)" -> lm["RSquared"]|>}]}]] &]
