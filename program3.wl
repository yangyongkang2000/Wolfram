FormFunction[{"理论光栅常数/nm" -> "Number", "error" -> "Number" -> 1}, 
 Manipulate[
   Module[{d = #"理论光栅常数/nm", degreeslist, integerdegreeslist, 
     lamdalist = {546.07, 565, 590}, integerleftbenchleft, 
     integerrightbenchleft, list1, list2, list3, list4, list, 
     error = #"error"}, degreeslist = ArcSin[lamdalist/d]*180/Pi;
    integerdegreeslist = Floor[degreeslist];
    integerleftbenchleft = 
     RandomInteger[{90, 180, 270, 360}[[n ;; n + 1]]];
    integerrightbenchleft = 
     RandomInteger[{{0, 90}, {90, 180}, {90, 270}}[[n]]];
    list1 = 
     Table[Table[
       RandomInteger[{integerleftbenchleft - error, 
          integerleftbenchleft + error}] + RandomReal[{0, 0.59}], 6], 
      3];
    list2 = 
     Table[Table[
       RandomInteger[{integerrightbenchleft - error, 
          integerrightbenchleft + error}] + RandomReal[{0, 0.59}], 6],
       3];
    list3 = 
     Table[Table[
       Floor[list1[[m]][[k]]] + 2*integerdegreeslist[[m]] + 
        RandomReal[{0, 0.59}] + RandomInteger[{0, 1}], {k, 1, 6}], {m,
        1, 3}];
    list4 = 
     Table[Table[
       Floor[list2[[m]][[k]]] + 2*integerdegreeslist[[m]] + 
        RandomInteger[{0, 1}] + RandomReal[{0, 0.59}], {k, 1, 6}], {m,
        1, 3}]; list = {list1, list2, list3, list4};
    Dataset@
     Table[<|Join[{"实验次数" -> r}, 
        Flatten@Table[
          Table[StringJoin[{"绿光", "黄光1", "黄光2"}[[m]], {"\[Beta]1", 
               "\[Beta]1'", "\[Beta]2", "\[Beta]2'"}[[k]]] -> 
            list[[k]][[m]][[r]], {k, 1, 4}], {m, 1, 3}]]|>, {r, 1, 
       6}]], {n, 1, 3, 1}] &]