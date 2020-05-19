(* ::Package:: *)

f[n_Integer]:=Expand@FullSimplify[\!\(
\*UnderoverscriptBox[\(\[Product]\), \(i = 0\), \(\(-1\) + #1\)]\(
\*FractionBox[
SubscriptBox[\(K\), 
SubscriptBox[\(a\), \(1 + i\)]], \(H\)]/\((1 + 
\*UnderoverscriptBox[\(\[Sum]\), \(k = 0\), \(\(-1\) + n\)]
\*UnderoverscriptBox[\(\[Product]\), \(i = 0\), \(k\)]
\*FractionBox[
SubscriptBox[\(K\), 
SubscriptBox[\(a\), \(1 + i\)]], \(H\)])\)\)\)&/@Range[0,n]];
g[n_Integer,list_List]:=f[n]/.AssociationThread[Subscript[K, Subscript[a, #]]&/@Range[n],list];
h[n_Integer,i_Integer]:=Join[Product[H/Subscript[K, Subscript[a, q]],{q,#,i}]&/@Range[1,i+1],Product[Subscript[K, Subscript[a, q+1]]/H,{q,i,#}]&/@Range[i,n-1]];
m[n_Integer,i_Integer]:=FullSimplify[h[n,i][[#]]/Total[h[n,i]]&/@Range[n+1]];
Eqn1[n_Integer,list_List]:=Select[H/.List@ToRules[NRoots[Numerator[Together[H+c1*a/(1+a)-c2/(1+a)*Total[Range[0,n]*f[n]]-Subscript[K, w]/H]]==0/.AssociationThread[Join[{a,c1,c2,Subscript[K, w]},Subscript[K, Subscript[a, #]]&/@Range[n]],list],H]],Positive];
