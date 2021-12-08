B = {4, -4, 1};

axisLabelOffset = 0.4;
vectorLabelOffset = 1;

projX = 5;
projY = 1;
projZ = 2;

oppX = -2;
oppY = -3;
oppZ = -2;

zeroOut = 0;

replace[v_, d_, n_] := {
  If[d == "x", n, v[[1]]],
  If[d == "y", n, v[[2]]],
  If[d == "z", n, v[[3]]]
}

origin = {0, 0, 0};

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
  Line[{B, {4, 0, 0}}],
  Arrow[Tube[{origin, {4, 0, 0}}, 0.02]],
  Text[Style[4, FontSize -> 24], {2, 0, 0.3}],
  Green,
  Line[{B, {0, -4, 0}}],
  Arrow[Tube[{origin, {0, -4, 0}}, 0.02]],
  Text[Style[4, FontSize -> 24], {0, -2, 0.3}],
  Blue,
  Line[{B, {0, 0, 1}}],
  FaceForm[Opacity[1]],
  Arrow[Tube[{origin, {0, 0, 1}}, 0.02]],
  Text[Style[1, FontSize -> 24], {0, 0.3, 0.3}],
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
