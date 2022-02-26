(* I think this only agrees in the case of damage mean power 1... wait but isn't that least absolutes, the one we've barely looked at yet? *)

getGpt[m_] := Module[{ma, decomp, left, snf, right, gpt},
  ma = getA[m];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  gpt = right.Transpose[snf].left;
  
  gpt
];

getMaxDamage[p_, tima_, ptm_, weighting_ : "unweighted", complexityWeighting_ : "F", complexityP_ : 1] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighting, complexityWeighting, complexityP];
  
  Max[Map[Abs, e * w]]
];

getSumOfSquaresDamage[p_, tima_, ptm_, weighting_ : "unweighted", complexityWeighting_ : "F", complexityP_ : 1] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighting, complexityWeighting, complexityP];
  
  Total[Map[#^2&, e * w]]
];

getSumOfAbsolutesDamage[p_, tima_, ptm_, weighting_ : "unweighted", complexityWeighting_ : "F", complexityP_ : 1] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighting, complexityWeighting, complexityP];
  
  Total[Map[Abs, e * w]]
];

getPFromMAndUnchangedIntervals[m_, unchangedIntervalEigenvectors_] := Module[{commaEigenvectors, eigenvectors, diagonalEigenvalueMatrix},
  commaEigenvectors = getA[getC[m]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueMatrix = getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueMatrix.Inverse[eigenvectors]]
];

getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];

tieBreak[tiedPs_, tima_, ptm_,weighting_, complexityWeighting_, complexityP_ ] := Module[{meanOfDamages},
  
  meanOfDamages = Map[getSumOfSquaresDamage[#, tima, ptm, weighting, complexityWeighting, complexityP]&, tiedPs];
  
  tiedPs[[First[First[Position[meanOfDamages, Min[meanOfDamages]]]]]]
];

getTuningCandidateStyle[m_, meanP_, weighting_ : "unweighted", complexityWeighting_ : Null, complexityP_ : Null, tim_ : Null] := Module[
  {
    d,
    r,
    tima,
    ptm,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialPs,
    meanOfDamages,
    minMeanIndices,
    minMeanIndex,
    tiedPs,
    minMeanP,
    gpt,
    projectedGenerators
  },
  
  d = getD[m];
  r = getR[m];
  
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  ptm = getPtm[d];
  
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromMAndUnchangedIntervals[m, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  meanOfDamages = If[
    meanP == \[Infinity],
    Map[getMaxDamage[#, tima, ptm, weighting, complexityWeighting, complexityP]&, potentialPs],
    If[
      meanP == 2,
      Map[getSumOfSquaresDamage[#, tima, ptm, weighting, complexityWeighting, complexityP]&, potentialPs],
      Map[getSumOfAbsolutesDamage[#, tima, ptm, weighting, complexityWeighting, complexityP]&, potentialPs]
    ]
  ];
  
  minMeanIndices =  Position[meanOfDamages, Min[meanOfDamages]];
  
  If[
    Length[minMeanIndices] == 1,
    minMeanIndex = First[First[Position[meanOfDamages, Min[meanOfDamages]]]];
    minMeanP = potentialPs[[minMeanIndex]],
    tiedPs = Part[potentialPs,Flatten[minMeanIndices]];
    (* Print["the f are my tiedPs", tiedPs];*)
    minMeanP  = tieBreak[tiedPs, tima, ptm, weighting, complexityWeighting, complexityP]
  ];
  
  (* Print["minimum damage mean: ", Min[meanOfDamages]];*)
  
  
  
  gpt = getGpt[m];
  projectedGenerators = minMeanP.gpt;
  solution = ptm.projectedGenerators // N;
  
  Print[meanP, weighting, complexityWeighting, complexityP];
  Print[1200 * First[solution]];
  Print[1200 * Last[solution]];
  Print[Min[meanOfDamages]];
  (*Print[meanOfDamages];*)
  
  solution
];


m = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)
m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)
m = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}; (*miracle *)
m = {{{1, 2, 1,1}, {0, -1, 3, 4}}, "co"}; (*pelogic *)
m = {{{1, 2, 3}, {0, -3, -5}}, "co"}; (*porcupine*)
m = {{{5, 8, 12}, {0, 0, -1}}, "co"}; (* blackwood *)


(* ONLY WORKS FOR LEAST ABSOLUTES *)

1200 * getTuningCandidateStyle[m, \[Infinity]]

1200 * getTuningCandidateStyle[m, \[Infinity], "regressive", "F", 1]
1200 * getTuningCandidateStyle[m, \[Infinity], "regressive", "F", 2]
1200 * getTuningCandidateStyle[m, \[Infinity], "regressive", "P", 1]
1200 * getTuningCandidateStyle[m, \[Infinity], "regressive", "P", 2]

1200 * getTuningCandidateStyle[m, \[Infinity], "progressive", "F", 1]
1200 * getTuningCandidateStyle[m, \[Infinity], "progressive", "F", 2]
1200 * getTuningCandidateStyle[m, \[Infinity], "progressive", "P", 1]
1200 * getTuningCandidateStyle[m, \[Infinity], "progressive", "P", 2]


1200 * getTuningCandidateStyle[m, 2]

1200 * getTuningCandidateStyle[m, 2, "regressive", "F", 1]
1200 * getTuningCandidateStyle[m, 2, "regressive", "F", 2]
1200 * getTuningCandidateStyle[m, 2, "regressive", "P", 1]
1200 * getTuningCandidateStyle[m, 2, "regressive", "P", 2]

1200 * getTuningCandidateStyle[m, 2, "progressive", "F", 1]
1200 * getTuningCandidateStyle[m, 2, "progressive", "F", 2]
1200 * getTuningCandidateStyle[m, 2, "progressive", "P", 1]
1200 * getTuningCandidateStyle[m, 2, "progressive", "P", 2]


1200 * getTuningCandidateStyle[m, 1]

1200 * getTuningCandidateStyle[m, 1, "regressive", "F", 1]
1200 * getTuningCandidateStyle[m, 1, "regressive", "F", 2]
1200 * getTuningCandidateStyle[m, 1, "regressive", "P", 1]
1200 * getTuningCandidateStyle[m, 1, "regressive", "P", 2]

1200 * getTuningCandidateStyle[m, 1, "progressive", "F", 1]
1200 * getTuningCandidateStyle[m, 1, "progressive", "F", 2]
1200 * getTuningCandidateStyle[m, 1, "progressive", "P", 1]
1200 * getTuningCandidateStyle[m, 1, "progressive", "P", 2]


