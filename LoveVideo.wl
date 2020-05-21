Export["[Wolfram语言520的表白]数学,计算机,音乐和爱情的一次伟大结合\[LongDash]\[LongDash]\
杰伦歌迷选择的告白气球.mp4", {"Animation" -> 
   With[{G = 
      Plot[(Surd[x, 3]^2 + 0.9 (3.3 - x^2)^(1/2) Sin[50*Pi x])/
        1.5, {x, -Pi, Pi}, ColorFunction -> "Rainbow"]}, 
    Animate[Show[
      Graphics[
       Rotate[Text[Style["520", Hue[RandomReal[]], FontSize -> 100]], 
        20*\[Theta] Degree], Background -> Black], G, 
      Graphics[
       Flatten@Table[{Disk[{r*Cos[-2 (3 - r) \[Theta]], 
            r*Sin[-2 (3 - r) \[Theta]]}, r/60], 
          RGBColor @@ (RandomReal[{0, 1}] & /@ Range[3])}, {r, 0.01, 
          3, 0.01}], Background -> Black], 
      PlotRange -> {{-3, 3}, {-3, 3}}, 
      ImageSize -> {1080, 720}], {\[Theta], -Pi, 100 \[Pi], \[Pi]/40},
      ControlType -> None]], 
  "Audio" -> 
   Audio[Import[
     "/Users/yangyongkang/Downloads/告白气球-周杰伦-7149583.flac"]]}, 
 "AnimationDuration" -> 215]