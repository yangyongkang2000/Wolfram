FormFunction[{"error" -> "Number" -> 0}, 
 Block[{error = #"error", function, function1, x1 = -0.03, 
    xarray = {RandomReal[{20, 21}], RandomReal[{19, 20}]}, 
    yarray = {RandomReal[{1.78, 1.80}], RandomReal[{1.80/4, 1.90/4}]},
     xlist, list, list1, list2}, 
   xlist = Join[{-0.03}, Range[0, {20, 19}[[#]], 0.5], 
       xarray[[#]] + Range[0, {2, 3}[[#]]]] & /@ Range[2];
   function[x_, x2_, y_] = Sqrt[((-x + x1) y^2)/(x1 - x2)];
   function1[x_, x2_, y_] = Log[(x - E^y x + E^y x1 - x2)/(x1 - x2)];
   list = 
    Table[(Join[
         MapThread[{function, 
             function1}[[n]], {xlist[[#]][[1 ;; {43, 41}[[#]]]], 
            Table[xarray[[#]], {43, 41}[[#]]], 
            Table[yarray[[#]], {43, 41}[[#]]]}]*(Join[
            RandomReal[{1 - error, 1 + error}] & /@ 
             Range[{42, 40}[[#]]], {1}]), {yarray[[1]] & /@ Range[2], 
           yarray[[2]] & /@ Range[3]}[[#]]]) & /@ Range[2], {n, 1, 
      2}]; list1 = Range[0.5, 3, 0.1]^2; 
   list2 = Join[Table[yarray[[1]]/x^2/(2 - x), {x, 0.5, 1, 0.1}], 
     Table[
      yarray[[1]]/x^2*RandomReal[{1 - error, 1 + error}], {x, 1, 3, 
       0.1}]];
   TabView[
    Join[({"r=1cm", "r=2cm"}[[#]] -> 
         Grid[Join[{{"测量次数", "电压/V", "理论电流/\[Mu]A", "逻辑电流/\[Mu]A"}}, 
           Table[{n, xlist[[#]][[n]], list[[1]][[#]][[n]], 
             list[[2]][[#]][[n]]}, {n, 1, {45, 44}[[#]]}]], 
          Frame -> All]) & /@ Range[2], 
     Table[{"理论伏安特性曲线", "逻辑伏安特性曲线"}[[k]] -> 
       Show@MapThread[
         ListLinePlot, {Table[{xlist[[#]][[n]], 
              list[[k]][[#]][[n]]}, {n, 1, {45, 44}[[#]]}] & /@ 
           Range[2], Mesh -> All & /@ Range[2], 
          PlotLabels -> {{"r=1cm", "r=2cm"}[[#]]} & /@ Range[2]}], {k,
        1, 2}], {"光电效应特性数据" -> 
       Grid[Join[{{"r/cm", 
           "饱和电流/\[Mu]A"}}, {Range[0.5, 3, 0.1][[#]], list2[[#]]} & /@
           Range[26]], Frame -> All], 
      "光电效应特性曲线" -> 
       ListLinePlot[{list1[[#]], list2[[#]]} & /@ Range[26], 
        Mesh -> All]}]]] &]