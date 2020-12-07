plot[data_List, xd_, xw_, xF_, q_, r_] := 
 Block[{k1 = r/(r + 1), b1 = xd/(r + 1), 
   r1 = (r + 1)*(xF - xw)/(xd - xF) + (q - 1)*(xd - xw)/(xd - xF), k2,
    b2, xf = ((r + 1)*xF + (q - 1)*xd)/(r + q), 
   yf = (r*xF + q*xd)/(r + q), t, f = Interpolation[data], 
   g = Interpolation[Reverse /@ data]}, k2 = (r1 + 1)/r1; b2 = -xw/r1;
   With[{data1 = 
     NestWhileList[({#, 
           If[# > xf, k1*# + b1, k2*# + b2]} &@(g[#[[2]]])) &, {xd, 
       xd}, #[[1]] >= xw &]}, 
   Show[Plot[{f[x], x, k2*x + b2, k1*x + b1}, {x, 0, 1}, 
     PlotRange -> {0, 1}, PlotTheme -> "Scientific", 
     FrameLabel -> (Style[#, 15] & /@ {"x", "y"}), 
     Epilog -> ({PointSize[Medium], Point[data]}~
        Join~{Blue, PointSize[Medium], 
         Point[{{xw, xw}, {xf, yf}, {xd, xd}}]}), 
     PlotLegends -> 
      Placed[LineLegend[{"气液平衡图", "对角线", "提馏段", "精馏段"}, 
        LegendMarkerSize -> 20, 
        LegendFunction -> Frame], {{0.8, 0.3}}]], 
    ListLinePlot[Riffle[data1, {g[#2], #2} & @@@ data1[[;; -2]]], 
     PlotStyle -> Red]]]]
