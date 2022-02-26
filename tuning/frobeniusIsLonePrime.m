thing = Max[
  Abs[(1 * g1 + 0 * g2 - 1200 * Log[2, 2 / 1])],
  Abs[(1 * g1 + 1 * g2 - 1200 * Log[2, 3 / 1])],
  Abs[(0 * g1 + 4 * g2 - 1200 * Log[2, 5 / 1])]
];

Plot3D[
  thing,
  {g1, 1198, 1208},
  {g2, 692, 702},
  PlotTheme -> "Web"
]

solution = NMinimize[
  thing,
  {g1, g2},
  Method -> "NelderMead"
];

{g1, g2} /. Last[solution] // N






dualNorm[norm_] := If[norm == 1, Infinity, 1 / (1 - 1 / norm)];

jip[d_] := Map[Log2, Map[Prime, Range[d]]];

getWeightingMatrix[d_, weight_] := If[
  weight == "inverse-prime-map",
  DiagonalMatrix[jip[d]],
  If[
    weight == "prime-map",
    DiagonalMatrix[1 / jip[d]],
    If[
      weight == "flat",
      DiagonalMatrix[Table[1, d]],
      Error
    ]
  ]
];

precision = 7;

optimizeGtm[m_, norm_, weight_ : "flat"] := If[
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
  (*  tm = gtm.ma.w;
   e = tm - ptm;
   Print["\n w: ", w, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ", precision * 1200 // N, "\n p: ", ptm, " = ", ptm * 1200 // N, "\n t: ", tm, " = ", tm * 1200 // N, "\n e: ", e, " = ", e * 1200 // N, "\n G: ", g];*)
  
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
  (*  Print["\n w: ", w, "\n r: ", r, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ", 1200 * gtm /. Last[optimalGtmSolution] // N, "\n p: ", ptm, " = ", 1200 * ptm // N, "\n t: ", tm, " = ", 1200 * tm /. Last[optimalGtmSolution] // N, "\n e: ", e, " = ", 1200 * e /. Last[optimalGtmSolution] // N];*)
  
  gtm /. Last[optimalGtmSolution] // N
];

mapping = {{{1, 1, 0}, {0, 1, 4}}, "co"};

1200 * optimizeGtm[mapping, 2, "flat"]
1200 * optimizeGtm[mapping, 2, "prime-map"]
1200 * optimizeGtm[mapping, 1, "prime-map"]


(* meantone {{{1,1,0}, {0, 1,4}}, "co"}; *)
ListPlot[
  {
    {
      {1202.3895892420837`, 697.1758205000406`} (* minimax, green *)
    },
    {
      {1202.60682298142`, 696.7413549025475`} (* F2 (Frobenius), red *)
    },
    {
      {1201.396851362292`, 697.0491106350094`} (* P2 (Breed), blue *)
    },
    {
      {1201.6985184346495`, 697.5643823930193`} (* P1 (Tenney), black *)
    }
  },
  PlotStyle -> {Green, Red, Blue, Black}
]
