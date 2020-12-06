plot[\[Alpha]_, xd_, xw_, xF_, q_, r_] := 
 Block[{k1 = r/(r + 1), b1 = xd/(r + 1), 
   r1 = (r + 1)*(xF - xw)/(xd - xF) + (q - 1)*(xd - xw)/(xd - xF), k2,
    b2, xf = ((r + 1)*xF + (q - 1)*xd)/(r + q), 
   yf = (r*xF + q*xd)/(r + q), t}, k2 = (r1 + 1)/r1; b2 = -xw/r1; 
  With[{data = 
     NestWhileList[({#, 
           If[# > xf, k1*# + b1, 
            k2*# + b2]} &@(#[[2]]/(\[Alpha] - (\[Alpha] - 
                1)*#[[2]]))) &, {xd, xd}, #[[1]] >= xw &]}, 
   Show[Plot[{x, (x \[Alpha])/(1 - x + x \[Alpha]), k1*x + b1, 
      k2*x + b2}, {x, 0, 1}, PlotRange -> {0, 1}, 
     PlotTheme -> "Scientific", PlotStyle -> Black, 
     Epilog -> {Blue, PointSize[Medium], 
       Point[{{xw, xw}, {xf, yf}, {xd, xd}}]}], 
    ListLinePlot[
     Riffle[data, {#2/(\[Alpha] - (\[Alpha] - 1)*#2), #2} & @@@ 
       data[[;; -2]]], PlotStyle -> Red]]]]
