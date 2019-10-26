FormFunction[{"沸点" -> "String", "气体折射率" -> "String", 
  "液体折射率" -> "String", "室温" -> "String"}, 
 Module[{BoilingPointList = ToExpression[(#"沸点")], 
    GasRefractiveIndexList = ToExpression[(#"气体折射率")], 
    LiquidRefractiveIndexList = ToExpression[(#"液体折射率")], 
    RoomTemperatureList = ToExpression[(#"室温")], TransFunction, 
    NormalGasRefractiveIndexList, NormalLiquidRefractiveIndexList, 
    GasDensityList, LiquidDensityList, length}, 
   TransFunction[x_] := 97.20*x^2 - 255.96*x + 168.3; 
   length = Length@BoilingPointList; 
   NormalGasRefractiveIndexList = 
    Table[GasRefractiveIndexList[[index]] + \
(RoomTemperatureList[[index]] - 20)*0.0005, {index, 1, 7}]; 
   NormalLiquidRefractiveIndexList = 
    Table[LiquidRefractiveIndexList[[index]] + \
(RoomTemperatureList[[index]] - 20)*0.0005, {index, 1, 7}]; 
   GasDensityList = TransFunction /@ NormalGasRefractiveIndexList; 
   LiquidDensityList = 
    TransFunction /@ NormalLiquidRefractiveIndexList; 
   If[Length@
      Union[Length /@ {BoilingPointList, GasRefractiveIndexList, 
         LiquidRefractiveIndexList, RoomTemperatureList}] == 1, 
    TabView[{"实验数据表格" -> 
       Dataset@Table[<|"沸点" -> BoilingPointList[[index]], 
          "气体折射率" -> GasRefractiveIndexList[[index]], 
          "液体折射率" -> LiquidRefractiveIndexList[[index]], 
          "室温" -> RoomTemperatureList[[index]], 
          "室温气体折射率" -> NormalGasRefractiveIndexList[[index]], 
          "室温液体折射率" -> NormalLiquidRefractiveIndexList[[index]], 
          "气体组成成分" -> GasDensityList[[index]], 
          "液体组成成分" -> LiquidDensityList[[index]]|>, {index, 1, 7}], 
      "实验图形" -> 
       Show[Flatten[{ListPlot[#, PlotStyle -> PointSize[0.03], 
             PlotTheme -> "Detailed"], 
            ListLinePlot[#, PlotStyle -> {Blue, Red}, 
             PlotLegends -> {"a", "b"}]} & /@ {Partition[
            Labeled[#, 
               StringJoin["(", ToString[#[[1]]], ",", 
                ToString[#[[2]]], ")"]] & /@ 
             Flatten[
              Table[{{GasDensityList, 
                    LiquidDensityList}[[index]][[#]], 
                  BoilingPointList[[#]]} & /@ Range[length], {index, 
                1, 2}], 1], length]}, 1]]}], Print["不要瞎几把输入"]]] &]
