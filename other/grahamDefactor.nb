hnf[a_] := Last[HermiteDecomposition[a]];

grahamDefactorWithComment[m_] := Module[{hermite, transformation},
  hermite = hnf[m];
  transformation = Inverse[Transpose[Take[hnf[Transpose[hermite]], MatrixRank[m]]]];
  Print[transformation // MatrixForm, ".", hermite // MatrixForm];

  transformation.hermite
];

grahamDefactorWithComment[{{5, 8, 12}, {24, 38, 56}}]
hnf[%]
grahamDefactorWithComment[{{3, 0, -1}, {0, 3, 5}}]
hnf[%]
grahamDefactorWithComment[{{6, 5, -4}, {4, -4, 1}}]
hnf[%]
grahamDefactorWithComment[{{17, 16, -4}, {4, -4, 1}}]
hnf[%]

(*check equivalence *)

grahamDefactor[m_] := Module[{hermite, huh, taken, transformation},
  hermite = hnf[m];
  transformation = Inverse[Transpose[Take[hnf[Transpose[hermite]], MatrixRank[m]]]];

  transformation.hermite
];

rightReducingMatrix[a_] := Last[SmithDecomposition[a]];
smithDefactor[a_] := Take[Inverse[rightReducingMatrix[a]], MatrixRank[a]];

hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];
colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];

ms = {};
Do[
  d = RandomInteger[{2, 6}];
  r = RandomInteger[{1, d}];
  m = RandomInteger[{-99, 99}, {r, d}];
  ms = Join[ms, {m}],
  1000
];

Do[
  g = hnf[grahamDefactor[m]];
  s = hnf[smithDefactor[m]];
  c = hnf[colHermiteDefactor[m]];

  If[g != s || g != c, Print["#######bad!", g, s, c], Print[m, g]],
  {m, ms}
]

(* compare speed *)

ms = {};
Do[
  d = RandomInteger[{2, 6}];
  r = RandomInteger[{1, d}];
  m = RandomInteger[{-99, 99}, {r, d}];
  ms = Join[ms, {m}],
  1000
];

AbsoluteTiming[Do[smithDefactor[m], {m, ms}]]
AbsoluteTiming[Do[colHermiteDefactor[m], {m, ms}]]
AbsoluteTiming[Do[grahamDefactor[m], {m, ms}]]
