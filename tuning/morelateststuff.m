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
  tm = gtm.ma.w;
  e = tm - ptm;
  Print["\n w: ", w, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ", precision * 1200 // N, "\n p: ", ptm, " = ", ptm * 1200 // N, "\n t: ", tm, " = ", tm * 1200 // N, "\n e: ", e, " = ", e * 1200 // N, "\n G: ", g];
  
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
  optimalGtmSolution = NMinimize[Norm[e.w, dualNorm[norm]], gtm, Method -> "NelderMead"];
  
  (* debugging *)
  Print["\n w: ", w, "\n r: ", r, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ", 1200 * gtm /. Last[optimalGtmSolution] // N, "\n p: ", ptm, " = ", 1200 * ptm // N, "\n t: ", tm, " = ", 1200 * tm /. Last[optimalGtmSolution] // N, "\n e: ", e, " = ", 1200 * e /. Last[optimalGtmSolution] // N];
  
  gtm /. Last[optimalGtmSolution] // N

];

sensamagic = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* this is actually meantone *)
sensamagic = {{{1, 0, 0, 0}, {0, 1, 1, 2}, {0, 0, 2, -1}}, "co"};

optimizeGtm[sensamagic, 1]

optimizeGtm[sensamagic, 1, "Tenney"]

optimizeGtm[sensamagic, 2]

optimizeGtm[sensamagic, 2, "Tenney"]

optimizeGtm[sensamagic, Infinity]

optimizeGtm[sensamagic, Infinity, "Tenney"]

d = 4;
ma = {{1, 0, 0, 0}, {0, 1, 1, 2}, {0, 0, 2, -1}};
gtm = {0.9992544543489527`, 1.586144162620642`, 0.36702641443676626`};
tm = gtm.ma;
ptm = Log[2, getPrimes[d]];
e = tm - ptm

d = 4;
ma = {{1, 0, 0, 0}, {0, 1, 1, 2}, {0, 0, 2, -1}};
gtm = {1, 1.586144162620642`, 0.36702641443676626`};
tm = gtm.ma;
ptm = Log[2, getPrimes[d]];
e = tm - ptm

optimizeGtm[{{{12, 19, 28}}, "co"}, 1, "Tenney"]

optimizeGtm[{{{5, 8, 0}, {0, 0, 1}}, "co"}, 1, "Tenney"]

dual[{{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}]
dual[{{{1, 2, -3, 1}}, "contra"}]

{{1, 0, 0, -1}, {0, 1, 0, -2}, {0, 0, 1, 3}}.Transpose[{{1, 2, -3, 1}}]

mapping = {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "co"};
mapping = {{{1, 1, 0}, {0, 1, 4}}, "co"};
1200 * optimizeGtm[mapping, 1, "unweighted"]
1200 * optimizeGtm[mapping, 2, "unweighted"]
1200 * optimizeGtm[mapping, Infinity, "unweighted"]
1200 * optimizeGtm[mapping, 1, "Tenney"]
1200 * optimizeGtm[mapping, 2, "Tenney"]
1200 * optimizeGtm[mapping, Infinity, "Tenney"]
1200 * optimizeGtm[mapping, 1, "Partch"]
1200 * optimizeGtm[mapping, 2, "Partch"]
1200 * optimizeGtm[mapping, Infinity, "Partch"]



First[Part[{{1, 0, -4, -13}, {0, 1, 4, 10}}.Transpose[{{0, 0, -1, 1}}], 2]]

{{5, 8, 0}, {0, 0, 1}}.Transpose[{{-1, 1, 0}}]

(*dual[{{{0,-5,1,2},{0,3,-2,0}},"contra"}]*)
dual[{{{0, -5, 1, 2}, {0, 22, -15, 0}}, "contra"}]
(*getLm[getA[%]]*)

{{1, 0, 0, 0}, {0, 30, 44, 53}}.Transpose[{{0, -1, 1, 0}}]

{{2, 0, 11, 12}, {0, 1, -2, -2}}.Transpose[{{-1, 1, 0, 0}}]
{{2, 0, 11, 12}, {0, 1, -2, -2}}.Transpose[{{-2, 0, 1, 0}}]
{{2, 0, 11, 12}, {0, 1, -2, -2}}.Transpose[{{0, -1, 1, 0}}]
{{2, 0, 11, 12}, {0, 1, -2, -2}}.Transpose[{{-2, 0, 0, 1}}]
{{2, 0, 11, 12}, {0, 1, -2, -2}}.Transpose[{{-1, -1, 0, 1}}]
{{2, 0, 11, 12}, {0, 1, -2, -2}}.Transpose[{{0, 0, -1, 1}}]



optimizeGtm[{{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}, 2]

{0.498621149017398`, 1.5847743690701703`} * 1200

{{2, 3, 5, 6}, {0, 1, -2, -2}}.Transpose[{{-1, 1, 0, 0}}]
{{2, 3, 5, 6}, {0, 1, -2, -2}}.Transpose[{{-2, 0, 1, 0}}]
{{2, 3, 5, 6}, {0, 1, -2, -2}}.Transpose[{{0, -1, 1, 0}}]
{{2, 3, 5, 6}, {0, 1, -2, -2}}.Transpose[{{-2, 0, 0, 1}}]
{{2, 3, 5, 6}, {0, 1, -2, -2}}.Transpose[{{-1, -1, 0, 1}}]
{{2, 3, 5, 6}, {0, 1, -2, -2}}.Transpose[{{0, 0, -1, 1}}]
