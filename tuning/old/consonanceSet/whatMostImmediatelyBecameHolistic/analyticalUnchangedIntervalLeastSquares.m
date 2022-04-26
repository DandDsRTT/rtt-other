(*getLeastSquaresTuning[m_, tim_ : Null] := Module[{d, ptm, ma, tima, unchangedIntervals, p, result},
  d = getD[m];
  ptm = getPrimesTuningMap[d];
  ma = getA[m];
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  
  unchangedIntervals = ma.Transpose[tima].tima;
  
  p = getPFromMAndUnchangedIntervals[m, unchangedIntervals];
  
  Print["P: ", p];
  (*Print["min sum-of-squares damage: ", getSumOfSquaresDamage[p, tima, ptm]];*)
  Print[getSumOfSquaresDamage[p, tima, ptm]];
  
  
  result = ptm.Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]] // N;
  
  Print[1200 * result[[1]]];
  Print[1200 * result[[2]]];
  
  result
];*)


getLeastSquaresTuningWeighted[m_, weighting_ : "unweighted", complexityWeighting_ : Null, complexityP_ : Null, tim_ : Null] := Module[{d, ptm, ma, tima, w, unchangedIntervals, p, result },
  d = getD[m];
  ptm = getPrimesTuningMap[d];
  ma = getA[m];
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  w = getW[tima, weighting, complexityWeighting, complexityP];
  (*Print["what w: ", w, weighting];*)
  tima = tima * w;
  (*W = DiagonalMatrix[1 / ptm](*.Transpose[DiagonalMatrix[1 / ptm]]*);*)
  
  unchangedIntervals = ma.Transpose[tima].tima;
  
  p = getPFromMAndUnchangedIntervals[m, unchangedIntervals];
  
  Print[ weighting, complexityWeighting, complexityP];
  (*Print["P: ", p // N];*)
  (*Print["min sum-of-squares damage: ", getSumOfSquaresDamage[p, tima, ptm]];*)
  
  
  result = ptm.Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]] // N;
  
  Print[1200 * result[[1]]];
  Print[1200 * result[[2]]];
  Print[getSumOfSquaresDamage[p, tima, ptm] // N];
  result
];

m = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)
m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)
(*m = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}; (*miracle *)
m = {{{1, 0, 7, 9}, {0, 1, -3, -4}}, "co"}; (*pelogic *)
m = {{{1, 2, 3}, {0, -3, -5}}, "co"}; (*porcupine*)
m = {{{5, 8, 12}, {0, 0, -1}}, "co"}; (* blackwood *)*)

1200 * getLeastSquaresTuningWeighted[m]
1200 * getLeastSquaresTuningWeighted[m, "simplicityWeighted", "noop", 1]
1200 * getLeastSquaresTuningWeighted[m, "simplicityWeighted", "noop", 2]
1200 * getLeastSquaresTuningWeighted[m, "simplicityWeighted", "logProduct", 1]
1200 * getLeastSquaresTuningWeighted[m, "simplicityWeighted", "logProduct", 2]
1200 * getLeastSquaresTuningWeighted[m, "complexityWeighted", "noop", 1]
1200 * getLeastSquaresTuningWeighted[m, "complexityWeighted", "noop", 2]
1200 * getLeastSquaresTuningWeighted[m, "complexityWeighted", "logProduct", 1]
1200 * getLeastSquaresTuningWeighted[m, "complexityWeighted", "logProduct", 2]

(*1200 * getLeastSquaresTuning[{{{1, 1, 0}, {0, 1, 4}}, "co"}]
1200 * getLeastSquaresTuning[{{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}]
1200 * getLeastSquaresTuning[{{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}]
1200 * getLeastSquaresTuning[{{{1, 0, 7, 9}, {0, 1, -3, -4}}, "co"}]
1200 * getLeastSquaresTuning[{{{1, 2, 3}, {0, -3, -5}}, "co"}]
1200 * getLeastSquaresTuning[{{{5, 8, 12}, {0, 0, -1}}, "co"}]


1200 * getLeastSquaresTuningWeighted[{{{1, 1, 0}, {0, 1, 4}}, "co"}]
1200 * getLeastSquaresTuningWeighted[{{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}]
1200 * getLeastSquaresTuningWeighted[{{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}]
1200 * getLeastSquaresTuningWeighted[{{{1, 0, 7, 9}, {0, 1, -3, -4}}, "co"}]
1200 * getLeastSquaresTuningWeighted[{{{1, 2, 3}, {0, -3, -5}}, "co"}]
1200 * getLeastSquaresTuningWeighted[{{{5, 8, 12}, {0, 0, -1}}, "co"}]*)
