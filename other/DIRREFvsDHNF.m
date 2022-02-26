multByLcd[l_] := Apply[LCM, Denominator[l]] * l;

hnf[a_] := Last[HermiteDecomposition[a]];

rref[a_] := RowReduce[a];

irref[a_] := Map[multByLcd, rref[a]];

hermiteRightUnimodular[a_] := Transpose[First[HermiteDecomposition[Transpose[a]]]];
colHermiteDefactor[a_] := Take[Inverse[hermiteRightUnimodular[a]], MatrixRank[a]];

ms = {};
Do[
  d = RandomInteger[{3, 6}];
  r = RandomInteger[{2, d - 1}];
  m = RandomInteger[{-9, 9}, {r, d}];
  ms = Join[ms, {m}],
  100
];

Do[
  t = irref[colHermiteDefactor[m]];
  c = hnf[colHermiteDefactor[m]];
  
  Print[m // MatrixForm, " \[RightArrow] DIRREF: ", t // MatrixForm, " DHNF: ", c // MatrixForm, "\n"],
  {m, ms}
]
