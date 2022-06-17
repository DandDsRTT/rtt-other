frob[t_, tenney_ : False] := Module[{r, d, ma, gtm, ptm, tm, e, result, result2},
  r = getR[t];
  d = getD[t];
  ma = getA[getM[t]];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  ptm = Log[2, getPrimes[d]];
  tm = gtm.ma;
  e = tm - ptm;
  
  result = Minimize[Norm[e, 2], gtm, Reals];
  
  Print[" r: ", r, " d: ", d, " m: ", ma, " g: ", gtm, " p: ", ptm, ptm // N, " t: ", tm, N[tm /. Last[result], 7] * 1200, " e: ", e, N[e /. Last[result], 7] * 1200];
  
  N[gtm /. Last[result], 7]
]

frob[{{{1, 0, -4}, {0, 1, 4}}, "co"}]



frobWithPseudoinverse[t_] := Module[{d, ma, g, ptm, gtm},
  d = getD[t];
  ma = getA[getM[t]];
  g = PseudoInverse[ma];
  ptm = Log[2, getPrimes[d]];
  gtm = ptm.g;
  Print[" d: ", d, " m: ", ma, " g: ", gtm, " p: ", ptm, " G: ", g];
  
  N[gtm, 7]
]

frobWithPseudoinverse[{{{1, 0, -4}, {0, 1, 4}}, "co"}]



te[t_, tenney_ : False] := Module[{r, d, ma, gtm, ptm, tm, e, result, result2},
  r = getR[t];
  d = getD[t];
  ma = getA[getM[t]];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  ptm = Log[2, getPrimes[d]].tenneyWeights[ma];
  tm = gtm.ma.tenneyWeights[ma];
  e = tm - ptm;
  
  result = Minimize[Norm[e, 2], gtm, Reals];
  
  Print[" r: ", r, " d: ", d, " m: ", ma, " g: ", gtm, " p: ", ptm, " t: ", tm, N[tm /. Last[result], 7] * 1200, N[gtm.ma /. Last[result], 7] * 1200, " e: ", e, N[e /. Last[result], 7] * 1200];
  
  N[gtm /. Last[result], 7]
]

te[{{{1, 0, -4}, {0, 1, 4}}, "co"}]


jip[d_] := Map[Log2, Map[Prime, Range[d]]];
tenneyWeights[ma_] := DiagonalMatrix[1 / jip[getD[ma]]]; (* or tenney buoys? *)

teWithPseudoinverse[t_] := Module[{d, ma, g, ptm, gtm},
  d = getD[t];
  ma = getA[getM[t]];
  g = PseudoInverse[ma.tenneyWeights[ma]];
  ptm = tenneyWeights[ma].Log[2, getPrimes[d]];
  gtm = ptm.g;
  Print[" d: ", d, " m: ", ma, " g: ", gtm, " p: ", ptm, " G: ", g];
  
  N[gtm, 7]
]

teWithPseudoinverse[{{{1, 0, -4}, {0, 1, 4}}, "co"}]



jip[d_] := Map[Log2, Map[Prime, Range[d]]];
tenneyWeights[ma_] := DiagonalMatrix[1 / jip[3]]; (* or tenney buoys? *)

Inverse[tenneyWeights[{}]] // N


l1[t_, tenney_ : False] := Module[{r, d, ma, gtm, ptm, tm, e, result, result2},
  r = getR[t];
  d = getD[t];
  ma = getA[getM[t]];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  ptm = Log[2, getPrimes[d]];
  tm = gtm.ma;
  e = tm - ptm;
  
  result = Minimize[Norm[e, Infinity], gtm, Reals];
  
  (*Print[" r: ", r, " d: ", d, " m: ", ma, " g: ", gtm, " p: ", ptm,ptm // N, " t: ", tm, N[tm /. Last[result],7] *1200, " e: ", e, N[e /. Last[result],7] *1200];*)
  
  N[gtm /. Last[result], 7]
]

l1[{{{1, 2, 4}, {0, -1, -4}}, "co"}] * 1200

