(* ::Package:: *)

(* ::Input:: *)
CourseTable[coursedata_,l_,h_]:=DynamicModule[{Tbl},
Tbl[date_]:=Module[{week,DateRule,ColorRule,CourseMatrix,CourseTable,rgbcolor},
rgbcolor[x_]:=RGBColor@@Append[List@@RandomColor[],x];
week[y_,z_]:=Floor[1+DateDifference[z,y,"Week"][[1]]];
DateRule[x_,w_]:=If[#1[[1]]<=w<=#1[[2]],#2->#3,Nothing]&@@@x;
ColorRule[x_]:={#1+1,#2}->rgbcolor[0.5]&@@@x[[All,1]];
CourseMatrix[x_,w_,z_]:=Prepend[ReplacePart[ConstantArray["",{7,5}],x],
With[{first=DatePlus[z,Quantity[w-1,"Weeks"]]},
DateString[#,
{"DayName", "\n","Month","/","Day"}]&/@DateRange[first,
DatePlus[first, Quantity[6,"Days"]]]]];
CourseTable[x_,y_,z_]:=TabView[Table[w->With[{d=DateRule[x,w]},
Grid[CourseMatrix[d,w,z],Background->{DateValue[date,"ISOWeekDay"]->rgbcolor[0.5],
None,ColorRule[d]},ItemSize->{l,h},Frame->All]],{w,#,#+3}]]&@week[y,z];
CourseTable[coursedata,date,{2021,8,30}]];
Dynamic@With[{time=Now[[1]]},Manipulate[Tbl[{Year,Month,Day}],
{Year,If[time[[1]]==2021,{2021,2022},{2022}]},
{Month,Range[time[[2]],12]~Join~Range[1,time[[2]]-1]},
{Day,Range[time[[3]],31]~Join~Range[1,time[[3]]-1]}]]]
