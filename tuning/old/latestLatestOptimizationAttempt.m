dualNorm[norm_] := If[norm == 1, Infinity, 1 / (1 - 1 / norm)];

jip[d_] := Map[Log2, Map[Prime, Range[d]]];

getWeightingMatrix[d_, weight_] := If[
  weight == "Partch",
  DiagonalMatrix[jip[d]],
  If[
    weight == "Tenney",
    DiagonalMatrix[1 / jip[d]],
    If[
      weight == "unweighted",
      DiagonalMatrix[Table[1, d]],
      Error
    ]
  ]
];

precision = 7;

optimizeGtm[m_, norm_, weight_ : "unweighted"] := If[
  norm == 2,
  optimizeGtmWithPseudoinverse[m, weight],
  optimizeGtmWithMinimizer[m, norm, weight]
];

optimizeGtmWithPseudoinverse[m_, weight_] := Module[{d, ma, w, g, ptm, gtm, tm, e},
  d = getD[m];
  ma = getA[getM[m]];
  w = getWeightingMatrix[d, weight];
  g = PseudoInverse[ma.w];
  ptm = w.Log[2, getPrimes[d]];
  gtm = ptm.g;
  
  (* this stuff is all only for debugging *)
  (*tm = gtm.ma.w;
  e = tm - ptm;
  Print["\n w: ", w, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ",precision *1200 // N,  "\n p: ", ptm," = ", ptm*1200 //N, "\n t: ", tm," = ", tm  *1200 //N, "\n e: ", e," = ", e *1200 // N, "\n G: ", g];*)
  
  gtm // N
];

optimizeGtmWithMinimizer[m_, norm_, weight_] := Module[{r, d, ma, gtm, ptm, tm, e, optimalGtmSolution, w},
  
  r = getR[m];
  d = getD[m];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  ptm = Log[2, getPrimes[d]];
  tm = gtm.ma;
  e = tm - ptm;
  w = getWeightingMatrix[d, weight];
  
  (*Print["about to minimize! here's e.w: ", e.w," and the dual norm is: ", dualNorm[norm]];*)
  optimalGtmSolution = NMinimize[Norm[e.w, dualNorm[norm]], gtm];
  
  (* debugging *)
  Print["\n w: ", w, "\n r: ", r, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ", 1200 * gtm /. Last[optimalGtmSolution] // N, "\n p: ", ptm, " = ", 1200 * ptm // N, "\n t: ", tm, " = ", 1200 * tm /. Last[optimalGtmSolution] // N, "\n e: ", e, " = ", 1200 * e /. Last[optimalGtmSolution] // N];
  
  gtm /. Last[optimalGtmSolution] // N

];


optimizeGtm[sensamagic, 1]
optimizeGtm[sensamagic, 1, "Tenney"]
optimizeGtm[sensamagic, 2]
optimizeGtm[sensamagic, 2, "Tenney"]
optimizeGtm[sensamagic, Infinity]
optimizeGtm[sensamagic, Infinity, "Tenney"]
