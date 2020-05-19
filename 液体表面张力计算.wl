Function[{Data}, 
 Module[{a, b, c, k, data = Data, SurfaceTensionData, Gibbs, g = 9.8, 
   NLM, LM, gamma, R = 8.31445598, T = 293.15}, 
  k = 7.280*(10^-2*1*g)/Mean@data[[1]];
  SurfaceTensionData = {10^3*First@#, k*Mean@#[[2 ;;]]/g} & /@ 
    Data[[2 ;;]]; 
  NLM = NonlinearModelFit[SurfaceTensionData, 
    a - b*Log[x + c], {a, b, c}, x]; 
  gamma = (-#/(R*T)*D[NLM[x], x] /. x -> #) &; 
  Gibbs = {#1, #1/gamma[#1]} & @@@ SurfaceTensionData; 
  LM = LinearModelFit[Gibbs, x, x]; 
  TabView[{"实验数据表格" -> 
     Dataset@Association[(ToString[10^3*#1] <> 
            "mol/\!\(\*SuperscriptBox[\(m\), \(3\)]\)" -> 
           Association@{"第一次最大压力" <> ToString[Subscript[P, max]] <> 
               "/Pa" -> #2, 
             "\!\(\*SubscriptBox[\(第二次最大压力P\), \(Max\)]\)/Pa" -> #3, 
             "\!\(\*SubscriptBox[\(第三次最大压力P\), \(Max\)]\)/Pa" -> #4, 
             "平均值/Pa" -> Mean@List[##2]}) & @@@ data[[2 ;;]]], 
    "\[Sigma]-C Graph" -> 
     Show[ListPlot[SurfaceTensionData, PlotTheme -> "Scientific", 
       FrameLabel -> {"C/mol/\!\(\*SuperscriptBox[\(m\), \(3\)]\)", 
         ToString[\[Sigma]] <> 
          "/N/\!\(\*SuperscriptBox[\(m\), \(-2\)]\)"}, 
       PlotLabel -> ToString[\[Sigma]] <> "-C Graph"], 
      Plot[NLM[x], {x, 0.02*10^3, 0.35*10^3}]], 
    "\!\(\*FormBox[FractionBox[
RowBox[{\"\[PartialD]\", \"\[Sigma]\"}], 
RowBox[{\"\[PartialD]\", \"c\"}],\nMultilineFunction->None],
TraditionalForm]\) Table" -> 
     Dataset[Association[
       ToString[10^3*#] <> 
           "mol/\!\(\*SuperscriptBox[\(m\), \(3\)]\)" -> 
          "\!\(\*FractionBox[\(\[PartialD]\[Sigma]\), \
\(\[PartialD]c\),\nMultilineFunction->None]\)=" <> 
           ToString[D[NLM[x], x] /. x -> 10^3*#] <> 
           "N*\!\(\*SuperscriptBox[\(m\), \(2\)]\)/mol" & /@ 
        Range[0.05, 0.3, 0.05]]], 
    "\!\(\*FractionBox[\(c\), \(\[CapitalGamma]\)]\)~c Graph" -> 
     Show[ListPlot[Gibbs, PlotTheme -> "Scientific", 
       FrameLabel -> {"C/mol/m^3", 
         "\!\(\*FractionBox[\(c\), \
\(\[CapitalGamma]\)]\)/\!\(\*SuperscriptBox[\(m\), \(-1\)]\)"}, 
       PlotLabel -> 
        "\!\(\*FractionBox[\(c\), \(\[CapitalGamma]\)]\)~c Graph"], 
      Plot[LM[x], {x, 1000*0.02, 1000*0.35}]], 
    "离散数据函数拟合表以及其他值" -> 
     Dataset[<|"\[Sigma]-C拟合函数" -> NLM[x], 
       "\!\(\*FractionBox[\(c\), \(\[CapitalGamma]\)]\)-c拟合函数" -> 
        LM[x], "\!\(\*FractionBox[\(c\), \
\(\[CapitalGamma]\)]\)-c拟合函数决定系数 \!\(\*FormBox[SuperscriptBox[\(R\), \
\(2\)],
TraditionalForm]\)" -> LM["RSquared"], 
       "K/N/\!\(\*SuperscriptBox[\(m\), \(2\)]\)" -> k, 
       "\!\(\*FractionBox[\(1\), SubscriptBox[\(\[CapitalGamma]\), \(\
\[Infinity]\)]]\)/\!\(\*SuperscriptBox[\(m\), \(2\)]\)/mol" -> 
        D[LM[x], x], 
       "分子横截面积/\!\(\*SuperscriptBox[\(m\), \(2\)]\)" -> 
        ScientificForm[D[LM[x], x]/(6.02*10^23)]|>]}]]]