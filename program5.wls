FormFunction[{"betalist" -> "String"}, 
 Block[{betalist = ToExpression[#"betalist"], transbetalist, 
    meantransbetalist, meanthetalist, aroundbetalist, d, 
    lambda = 5.4607*10^-7, aroundd, aroundlambdalist, lambdalist, 
    Trans, function, meandegrees, deltabetalist}, 
   meandegrees[list1_, list2_, list3_, list4_] := 
    1/24*Total[Abs[list1 - list2] + Abs[list3 - list4]]; 
   Trans[x_] := IntegerPart[x] + FractionalPart[x]/60*100;
   function[list_] := 
    Sqrt[Total[(Mean[list] - list)^2]/(Length[list] - 1)];
   transbetalist = 
    Partition[Partition[Trans /@ Flatten@betalist, 6], 3];
   meantransbetalist = Partition[Mean /@ Flatten[transbetalist, 1], 3];
   meanthetalist = MapThread[meandegrees, transbetalist];
   deltabetalist = Partition[function /@ Flatten[transbetalist, 1], 3];
   aroundbetalist = 1/4*(Total[deltabetalist^2] + 1/675)/180*Pi;
   d = lambda/Sin[meanthetalist[[1]]/180*Pi];
   aroundd = 
    lambda*Cos[meanthetalist[[1]]/180*Pi]/
      Sin[meanthetalist[[1]]/180*Pi]^2*aroundbetalist[[1]];
   lambdalist = d*Sin[meanthetalist[[2 ;; 3]]*Pi/180];
   aroundlambdalist = 
    d*Cos[meanthetalist[[2 ;; 3]]*Pi/180]*aroundbetalist[[2 ;; 3]];
   TabView@
    Table[{"绿光", "黄光1", "黄光2"}[[k]] -> 
      Grid[Join[{{"d/m", 
          "\[Theta]平均值\!\(\*SuperscriptBox[\(/\), \(o\)]\)", 
          "\[Beta]1平均值\!\(\*SuperscriptBox[\(/\), \(0\)]\)", 
          "\[Beta]2平均值\!\(\*SuperscriptBox[\(/\), \(0\)]\)", 
          "\[Beta]1'平均值\!\(\*SuperscriptBox[\(/\), \(0\)]\)", 
          "\[Beta]2'平均值\!\(\*SuperscriptBox[\(/\), \(0\)]\)", 
          "\!\(\*SubscriptBox[\(\[CapitalDelta]\), \
\(\[Theta]\)]\)/Rad", 
          "\!\(\*SubscriptBox[\(\[CapitalDelta]\), \(d\)]\)/m", 
          "\[Lambda]/m"}}, {Join[{d}, {meanthetalist[[k]]}, 
          meantransbetalist[[#]][[k]] & /@ 
           Range[4], {aroundbetalist[[k]]}, {Join[{aroundd}, 
             aroundlambdalist][[k]]}, {Flatten[{lambda, 
              lambdalist}][[k]]}]}], Frame -> All], {k, 1, 3}]] &]