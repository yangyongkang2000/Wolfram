FormFunction[{"TimeSeries/s" -> 
   "String" -> 
    "{11.69,16.39,21.39,27.09,32.64,38.81,45.91,53.81,63.77}", 
  "V" -> "String" -> "{2,3,4,5,6,7,8,9,10}", 
  "V_\[Infinity]/ml" -> "Number" -> 17.0467}, 
 Module[{timeseries = ToExpression[#"TimeSeries/s"], 
    V = ToExpression[#"V"], VInfinity = #"V_\[Infinity]/ml", Delta, 
    list, data, data1, m, k, lm, nlm}, Delta = VInfinity - V; 
   list = N@Log@Delta; 
   data = MapThread[{#1, #2} &, {timeseries, list}];
   data1 = MapThread[{#1/60, #2} &, {timeseries, V}];
   nlm = NonlinearModelFit[data1, a + b*E^(-c*x), {a, b, c}, x];
   lm = LinearModelFit[data, x, x]; k = -D[lm[x], x]; 
   TabView[{"实验数据表格" -> 
      Grid[List[Prepend[timeseries, "t/s"], Prepend[V, "V/ml"], 
        Prepend[Delta, 
         "\!\(\*SubscriptBox[\(V\), \
\(\[Infinity]\)]\)-\!\(\*SubscriptBox[\(V\), \(t\)]\)/ml"], 
        Prepend[list, 
         "\!\(TraditionalForm\`log(\*SubscriptBox[\(V\), \
\(\[Infinity]\)] - \*SubscriptBox[\(V\), \(t\)])\)"]], 
       Rule[Frame, All]], 
     "\!\(\*SubscriptBox[\(V\), \(t\)]\)-T散点图" -> 
      Show[ListPlot[data1, PlotTheme -> "Scientific", 
        FrameLabel -> {"t/min", 
          "\!\(\*SubscriptBox[\(V\), \(t\)]\)/ml"}], 
       Plot[nlm[x], {x, timeseries[[1]]/60, timeseries[[-1]]/60}]], 
     "图形" -> Show[
       ListPlot[data, PlotTheme -> "Scientific", 
        FrameLabel -> {"t/s", 
          "\!\(TraditionalForm\`log(\*SubscriptBox[\(V\), \
\(\[Infinity]\)] - \*SubscriptBox[\(V\), \(t\)])\)"}], 
       Plot[lm[x], {x, timeseries[[1]], timeseries[[-1]]}]], 
     "计算数据表格" -> 
      Grid[{{"K", k}, {"半衰期/s", 
         Log[2]/k}, {"\!\(\*SubscriptBox[\(线性拟合估计V\), \
\(\[Infinity]\)]\)/ml", E^(lm[0])}, {"线性拟合方程", lm[x]}, {"非线性拟合方程", 
         nlm[x]}, {"\!\(\*SubscriptBox[\(非线性拟合估计V\), \(\[Infinity]\)]\
\)/ml", nlm[x] /. a_ + b_*E^(c_*x) :> (Abs[a] + Abs[b])/2}}, 
       Frame -> All]}]] &]