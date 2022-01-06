B = {4, -4, 1};
A = {7, 0, -3};
AB = A + B;

axisLabelOffset = 0.4;
vectorLabelOffset = 1;

projX = 12;
projY = 1;
projZ = 2;

oppX = -1;
oppY = -2;
oppZ = -4;

zeroOut = 0;

replace[v_, d_, n_] := {
  If[d == "x", n, v[[1]]],
  If[d == "y", n, v[[2]]],
  If[d == "z", n, v[[3]]]
}
replace[A, "y", 0];

origin = {0, 0, 0};

centerProjectedXfinal = {12.35, -2, -1.6}
centerProjectedYfinal = {6.0, 1.35, -1.8};
centerProjectedZfinal = {5.0, -2.3, 2.7};

text3D[str_, location_ : {0, 0, 0},
  scaling : {_?NumericQ, _?NumericQ} : {1, 1},
  longitude : _?NumericQ : 0,
  elevation : _?NumericQ : 0,
  tilt : _?NumericQ : 0,
  opt : OptionsPattern[]] := Module[{
  mesh = DiscretizeGraphics[
    Text[Style[str, opt, FontFamily -> "Cambria Math",
      FontSize -> 12]], _Text,
    MaxCellMeasure -> 0.04],
  rotmatrx = RotationMatrix[longitude, {0, 0, 1}].
      RotationMatrix[-elevation, {0, 1, 0}].RotationMatrix[tilt, {1, 0, 0}]},
  Join[{EdgeForm[]},
    MeshPrimitives[mesh, 2] /. {x_?NumberQ, y_?NumberQ} :> (
      rotmatrx.(scaling ~ Join ~ {1} {x, y, 0}) + location)
    (*
       ,{Black},
       MeshPrimitives[BoundaryMesh[mesh],1]/.{x_?NumberQ,y_?NumberQ}:>
       (rotmatrx.(scaling~Join~{1} {x,y,0})+location)
    REMOVE REM IF NEEDING OUTLINES*)
  ]]




Graphics3D[{
  (*axes *)
  Gray,
  Thickness[Medium],
  Arrowheads[0.015],
  Arrow[{origin, {projX + 2, 0, 0}}],
  Arrow[{origin, {0, projY + 3, 0}}],
  Arrow[{origin, {0, 0, projZ + 1.5}}],
  Arrow[{origin, {oppX - 3, 0, 0}}],
  Arrow[{origin, {0, oppY - 2, 0}}],
  Arrow[{origin, {0, 0, oppZ - 3}}],
  Text[Style[Subscript["+\[Pi]", 2], FontSize -> 24], {projX + 2 + axisLabelOffset, 0, 0}],
  Text[Style[Subscript["+\[Pi]", 3], FontSize -> 24], {0, projY + 3 + axisLabelOffset, 0}],
  Text[Style[Subscript["+\[Pi]", 5], FontSize -> 24], {0, 0, projZ + 1.5 + axisLabelOffset}],
  Text[Style[Subscript["-\[Pi]", 2], FontSize -> 24], {oppX - 3 - 1.5 * axisLabelOffset, 0, 0}],
  Text[Style[Subscript["-\[Pi]", 3], FontSize -> 24], {0, oppY - 2 - axisLabelOffset, 0}],
  Text[Style[Subscript["-\[Pi]", 5], FontSize -> 24], {0, 0, oppZ - 3 - axisLabelOffset}],


  (*mulitvector area *)
  FaceForm[Opacity[0.82]],
  EdgeForm[Directive[Yellow, Thin, Dashed]],
  White,
  Parallelepiped[origin, {A, B}],

  (*projections *)
  FaceForm[Opacity[0.18]],
  EdgeForm[Directive[Red, Thin, Dashed]],
  Red,
  Parallelepiped[{projX, 0, 0}, {replace[A, "x", 0], replace[B, "x", 0]}],
  EdgeForm[Directive[Green, Thin, Dashed]],
  Green,
  Parallelepiped[{0, projY, 0}, {replace[A, "y", 0], replace[B, "y", 0]}],
  EdgeForm[Directive[Blue, Thin, Dashed]],
  Blue,
  Parallelepiped[{0, 0, projZ}, {replace[A, "z", 0], replace[B, "z", 0]}],

  (*projection connecter lines *)
  Dashed,
  Thickness[Small],
  Red,
  Line[{origin, replace[origin, "x", projX]}],
  Line[{A, replace[A, "x", projX]}],
  Line[{B, replace[B, "x", projX]}],
  Line[{AB, replace[AB, "x", projX]}],
  Green,
  Line[{origin, replace[origin, "y", projY]}],
  Line[{A, replace[A, "y", projY]}],
  Line[{B, replace[B, "y", projY]}],
  Line[{AB, replace[AB, "y", projY]}],
  Blue,
  Line[{origin, replace[origin, "z", projZ]}],
  Line[{A, replace[A, "z", projZ]}],
  Line[{B, replace[B, "z", projZ]}],
  Line[{AB, replace[AB, "z", projZ]}],

  (*projection area labels*)
  FaceForm[Opacity[1]],
  Red,
  text3D["12", centerProjectedXfinal, {0.035, 0.035}, \[Pi] / 2, 0, \[Pi] / 2], (*area(3,5) = *)
  Green,
  text3D["19", centerProjectedYfinal, {0.045, 0.045}, 0, 0, \[Pi] / 2], (*area(2,5) = *)
  Blue,
  text3D["28", centerProjectedZfinal, {0.045, 0.045}, \[Pi] / 2], (*area(2,3) = *)

  (*vectors *)
  Black,
  Dashing[{}], (*solid again *)
  Thickness[Large],
  Arrow[Tube[{origin, A}, 0.03]],
  Arrow[Tube[{origin, B}, 0.03]],

  (*vector labels *)
  Black,
  Text[Style[Rotate["C₂ = [4 -4 1⟩", 30 Degree], FontSize -> 24, FontFamily -> "Cambria Math"], {0, -1, -0.2}],
  Text[Style[Rotate["C₁ = [7 0 -3⟩", -59 Degree], FontSize -> 24, FontFamily -> "Cambria Math"], {3.5, 0, -1.15 }],

  (*main label*)
  Text[Style["C₁\[And]C₂ =\n[[28 -19 12⟩⟩", FontSize -> 24, TextAlignment -> Center, FontFamily -> "Cambria Math"], {4, -4, -0.7}],

  (* pitch points inside blocks *)
  PointSize[Medium],
  Red,
  Point[{projX, -1, 0}],
  Point[{projX, -2, 0}],
  Point[{projX, -3, 0}],
  Point[{projX, -4, 0}],
  Point[{projX, -1, - 1}],
  Point[{projX, -2, - 1}],
  Point[{projX, -3, - 1}],
  Point[{projX, -4, - 1}],
  Point[{projX, -1, - 2}],
  Point[{projX, -2, - 2}],
  Point[{projX, -3, - 2}],
  Point[{projX, -4, - 2}],
  Green,
  Point[{1, projY, 0}],
  Point[{2, projY, 0}],
  Point[{3, projY, 0}],
  Point[{4, projY, 0}],
  Point[{5, projY, 0}],
  Point[{6, projY, 0}],
  Point[{3, projY, -1}],
  Point[{4, projY, -1}],
  Point[{5, projY, -1}],
  Point[{6, projY, -1}],
  Point[{7, projY, -1}],
  Point[{8, projY, -1}],
  Point[{5, projY, -2}],
  Point[{6, projY, -2}],
  Point[{7, projY, -2}],
  Point[{8, projY, -2}],
  Point[{9, projY, -2}],
  Point[{10, projY, -2}],
  Point[{11, projY, -2}],
  Blue,
  Point[{2, -1, projZ}],
  Point[{3, -1, projZ}],
  Point[{4, -1, projZ}],
  Point[{5, -1, projZ}],
  Point[{6, -1, projZ}],
  Point[{7, -1, projZ}],
  Point[{8, -1, projZ}],
  Point[{3, -2, projZ}],
  Point[{4, -2, projZ}],
  Point[{5, -2, projZ}],
  Point[{6, -2, projZ}],
  Point[{7, -2, projZ}],
  Point[{8, -2, projZ}],
  Point[{9, -2, projZ}],
  Point[{4, -3, projZ}],
  Point[{5, -3, projZ}],
  Point[{6, -3, projZ}],
  Point[{7, -3, projZ}],
  Point[{8, -3, projZ}],
  Point[{9, -3, projZ}],
  Point[{10, -3, projZ}],
  Point[{5, -4, projZ}],
  Point[{6, -4, projZ}],
  Point[{7, -4, projZ}],
  Point[{8, -4, projZ}],
  Point[{9, -4, projZ}],
  Point[{10, -4, projZ}],
  Point[{11, -4, projZ}],

  (*projections of the main vectors*)
  Red,
  Arrow[Tube[{replace[origin, "x", projX], replace[A, "x", projX]}, 0.02]],
  Arrow[Tube[{replace[origin, "x", projX], replace[B, "x", projX]}, 0.02]],
  Green,
  Arrow[Tube[{replace[origin, "y", projY], replace[A, "y", projY]}, 0.02]],
  Arrow[Tube[{replace[origin, "y", projY], replace[B, "y", projY]}, 0.02]],
  Blue,
  Arrow[Tube[{replace[origin, "z", projZ], replace[A, "z", projZ]}, 0.02]],
  Arrow[Tube[{replace[origin, "z", projZ], replace[B, "z", projZ]}, 0.02]],

  (*hints of tilings of periodicity blocks*)
  (*FaceForm[Opacity[0.0]],
  EdgeForm[Directive[Red, Thin,Dashed]],
  EdgeForm[Opacity[0.1]],
  Red,
  Parallelepiped[{projX,0,0} + replace[A,"x",0], {replace[A,"x",0],replace[B,"x",0]}],
  Parallelepiped[{projX,0,0} - replace[A,"x",0], {replace[A,"x",0],replace[B,"x",0]}],
  Parallelepiped[{projX,0,0} + replace[B,"x",0], {replace[A,"x",0],replace[B,"x",0]}],
  Parallelepiped[{projX,0,0} - replace[B,"x",0], {replace[A,"x",0],replace[B,"x",0]}],
  Parallelepiped[{projX,0,0} + replace[A,"x",0] + replace[B,"x",0], {replace[A,"x",0],replace[B,"x",0]}],
  Parallelepiped[{projX,0,0} + replace[A,"x",0] - replace[B,"x",0], {replace[A,"x",0],replace[B,"x",0]}],
  Parallelepiped[{projX,0,0} - replace[A,"x",0] + replace[B,"x",0], {replace[A,"x",0],replace[B,"x",0]}],
  Parallelepiped[{projX,0,0} - replace[A,"x",0] - replace[B,"x",0], {replace[A,"x",0],replace[B,"x",0]}],*)
},
  BoxStyle -> Opacity[0],
  ViewPoint -> {4.7, -2.4, 2.4},
  ImageSize -> 2000
]

(* label it 12-ET, label the parallelogram area [4 -4 1⟩\[And][7 0 -3⟩ = [[12 -19 28⟩⟩ and then magnitude || of that is \[Sqrt]1289 but with sqrt of squares *)
