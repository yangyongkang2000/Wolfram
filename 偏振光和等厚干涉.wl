FormFunction[{"CurrentMaximum" -> "Number", "multiple" -> "Number", 
  "error" -> "Number" -> 0.01, "theta/(degree)" -> "Number" -> 30, 
  "start/mm" -> "Number" -> 10, "error1" -> "Number" -> 0.01}, 
 Module[{CurrentMaximum = #"CurrentMaximum", multipe = #"multiple", 
    error = #"error", CurrentMinimum, AngleList = Range[0, 375, 15], 
    function, CurrentList, theta = #"theta/(degree)", list1, list2, 
    RealCurrentList, MeanCurrentMaximum, MeanCurrentMinimum, list3, 
    start = #"start/mm", lambda = 6.33*10^(-4), error1 = #"error1", 
    endlist, startlist, lambdalist, around, meanlamdalist}, 
   CurrentMinimum = 
    CurrentMaximum/
     RandomReal[Cot[theta/180*Pi]*{1 - error, 1 + error}]; 
   function[x_] := 
    multipe*Sqrt[
      CurrentMaximum^2*
       CurrentMinimum^2/(CurrentMaximum^2*Sin[x]^2 + 
          CurrentMinimum^2*Cos[x]^2)]*
     RandomReal[{1 - error, 1 + error}]; 
   CurrentList = function /@ (Pi/180*AngleList); 
   list1 = Riffle[
     Flatten@{"P2角度值/\!\(\*SuperscriptBox[\((\), \(o\)]\))", 
         Partition[AngleList, {5}][[#]]} & /@ Range[5], 
     Flatten@{"光电流值", Partition[CurrentList, {5}][[#]]} & /@ 
      Range[5]]; 
   MeanCurrentMaximum = Mean[CurrentList[[#]] & /@ {1, 23}]; 
   MeanCurrentMinimum = Mean[CurrentList[[#]] & /@ {7, 13}]; 
   RealCurrentList = (Sqrt[
        MeanCurrentMaximum^2*
         MeanCurrentMinimum^2/((MeanCurrentMaximum^2*Sin[#]^2 + 
             MeanCurrentMinimum^2*Cos[#]^2))] & /@ (Pi/180*
        AngleList)); 
   list2 = Riffle[
     Flatten@{"P2角度值/\!\(\*SuperscriptBox[\((\), \(o\)]\))", 
         Partition[AngleList, {5}][[#]]} & /@ Range[5], 
     Flatten@{"E(\[Phi])", Partition[RealCurrentList, {5}][[#]]} & /@ 
      Range[5]]; 
   list3 = RealCurrentList[[#]]*({Cos[#], Sin[#]} & /@ (Pi/180*
            AngleList))[[#]] & /@ Range[25]; 
   startlist = 
    Join[{start}, start + N@RandomInteger[{-5, 5}]/100 & /@ Range[5]];
    endlist = 
    startlist[[#]] + 500*lambda*RandomReal[{1 - error, 1 + error}] & /@
      Range[6]; lambdalist = (endlist - startlist)/500; 
   meanlamdalist = Mean@lambdalist; 
   around = Sqrt[Total[(lambdalist - meanlamdalist)^2]/5]; 
   TabView[{"偏振光实验数据" -> Grid[list1, Frame -> All], 
     "数据处理" -> Grid[list2, Frame -> All], 
     "数据可视化绘制" -> ListLinePlot[list3, Mesh -> All], 
     "等厚干涉实验数据" -> 
      Grid[{Flatten@{"次数", Range[6]}, Flatten@{"起点读数/mm", startlist}, 
        Flatten@{"终点读数/mm", endlist}}, Frame -> All], 
     "数据处理" -> 
      Grid[{Flatten@{"\!\(\*SubscriptBox[\(\[Lambda]\), \(i\)]\)/mm", 
          Range[6], "平均值\[Lambda]/mm", "不确定度"}, 
        Flatten@{"波长/mm", lambdalist, meanlamdalist, around}}, 
       Frame -> All]}]] &]