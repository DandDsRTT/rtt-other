getGeneratorsPreimageTransversal[m_] := Module[{ma, decomp, left, snf, right, generatorsPreimageTransversal},
  ma = getA[m];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorsPreimageTransversal = right.Transpose[snf].left;
  
  generatorsPreimageTransversal
];

getMaxDamage[p_, tima_, ptm_, weighting_ : "unweighted", complexityWeighting_ : "noop", complexityP_ : 1] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighting, complexityWeighting, complexityP];
  
  Max[Map[Abs, e * w]]
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


getMinimaxTuning[m_, weighting_ : "unweighted", complexityWeighting_ : Null, complexityP_ : Null, tim_ : Null] := Module[
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
    maxDamages,
    minimaxIndex,
    minimaxP,
    generatorsPreimageTransversal,
    projectedGenerators
  },
  
  d = getD[m];
  r = getR[m];
  
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  ptm = getPrimesTuningMap[d];
  
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromMAndUnchangedIntervals[m, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  maxDamages = Map[getMaxDamage[#, tima, ptm, weighting, complexityWeighting, complexityP]&, potentialPs];
  
  minimaxIndex = First[First[Position[maxDamages, Min[maxDamages]]]];
  minimaxP = potentialPs[[minimaxIndex]];
  
  Print["min of maxDamage", Min[maxDamages]];
  
  generatorsPreimageTransversal = getGeneratorsPreimageTransversal[m];
  projectedGenerators = minimaxP.generatorsPreimageTransversal;
  ptm.projectedGenerators // N
];


m = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)
(*m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)
m = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}; (*miracle *)
m = {{{1, 0, 7, 9}, {0, 1, -3, -4}}, "co"}; (*pelogic *)
m = {{{1, 2, 3}, {0, -3, -5}}, "co"}; (*porcupine*)
m = {{{5, 8, 12}, {0, 0, -1}}, "co"}; (* blackwood *)*)

1200 * getMinimaxTuning[m]

1200 * getMinimaxTuning[m, "simplicityWeighted", "noop", 1]
1200 * getMinimaxTuning[m, "simplicityWeighted", "noop", 2]
1200 * getMinimaxTuning[m, "simplicityWeighted", "logProduct", 1]
1200 * getMinimaxTuning[m, "simplicityWeighted", "logProduct", 2]

1200 * getMinimaxTuning[m, "complexityWeighted", "noop", 1]
1200 * getMinimaxTuning[m, "complexityWeighted", "noop", 2]
1200 * getMinimaxTuning[m, "complexityWeighted", "logProduct", 1]
1200 * getMinimaxTuning[m, "complexityWeighted", "logProduct", 2]
