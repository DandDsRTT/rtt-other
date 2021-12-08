B = {4, -4, 1};

axisLabelOffset = 0.4;
vectorLabelOffset = 1;

projX = 5;
projY = 1;
projZ = 2;

oppX = -2;
oppY = -2;
oppZ = -2;

zeroOut = 0;

replace[v_, d_, n_] := {
  If[d == "x", n, v[[1]]],
  If[d == "y", n, v[[2]]],
  If[d == "z", n, v[[3]]]
}

origin = {0, 0, 0};

centerProjectedXfinal = {5, -2, 0};
centerProjectedYfinal = {0, 2, 0};
centerProjectedZfinal = {3, -3, 3};

Graphics3D[{
  Gray,
  Thickness[Medium],
  Arrowheads[0.015],
  Arrow[{origin, {projX + 2, 0, 0}}],
  Arrow[{origin, {0, projY + 3, 0}}],
  Arrow[{origin, {0, 0, projZ + 1.5}}],
  Arrow[{origin, {oppX - 3, 0, 0}}],
  Arrow[{origin, {0, oppY - 2, 0}}],
  Arrow[{origin, {0, 0, oppZ - 2}}],
  Text[Style[Subscript["+\[Pi]", 2], FontSize -> 24], {projX + 2 + axisLabelOffset, 0, 0}],
  Text[Style[Subscript["+\[Pi]", 3], FontSize -> 24], {0, projY + 3 + axisLabelOffset, 0}],
  Text[Style[Subscript["+\[Pi]", 5], FontSize -> 24], {0, 0, projZ + 1.5 + axisLabelOffset}],
  Text[Style[Subscript["-\[Pi]", 2], FontSize -> 24], {oppX - 3 - 1.5 * axisLabelOffset, 0, 0}],
  Text[Style[Subscript["-\[Pi]", 3], FontSize -> 24], {0, oppY - 2 - axisLabelOffset, 0}],
  Text[Style[Subscript["-\[Pi]", 5], FontSize -> 24], {0, 0, oppZ - 2 - axisLabelOffset}],
  Dashed,
  Thickness[Small],
  Red,
  Line[{origin, replace[origin, "x", projX]}],
  Line[{B, replace[B, "x", projX]}],
  Arrow[Tube[{replace[origin, "x", projX], replace[B, "x", projX]}, 0.02]],
  Text[Style[\[Sqrt]17, FontSize -> 24], centerProjectedXfinal],
  Text[Style["                      != 1", FontSize -> 24], centerProjectedXfinal],
  Green,
  Line[{origin, replace[origin, "y", projY]}],
  Line[{B, replace[B, "y", projY]}],
  Arrow[Tube[{replace[origin, "y", projY], replace[B, "y", projY]}, 0.02]],
  Text[Style[\[Sqrt]17, FontSize -> 24], centerProjectedYfinal],
  Text[Style["                      != 4", FontSize -> 24], centerProjectedYfinal],
  Blue,
  Line[{origin, replace[origin, "z", projZ]}],
  Line[{B, replace[B, "z", projZ]}],
  FaceForm[Opacity[1]],
  Arrow[Tube[{replace[origin, "z", projZ], replace[B, "z", projZ]}, 0.02]],
  Text[Style[4\[Sqrt]2, FontSize -> 24], centerProjectedZfinal],
  Text[Style["                      != 4", FontSize -> 24], centerProjectedZfinal],
  Black,
  Dashing[{}],
  Thickness[Large],
  Arrow[Tube[{origin, B}, 0.03]],
  Text[Style[Rotate["C₂ = [4 -4 1⟩", 30 Degree], FontSize -> 24, FontFamily -> "Cambria Math"], {0, -1, -0.2}],
},
  BoxStyle -> Opacity[0],
  ViewPoint -> {4.7, -2.4, 2.4},
  ImageSize -> 2000
]
