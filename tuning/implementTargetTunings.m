leastSquaresSomethingEvenOlder[m_] := Module[{},
  diamondThing = {{19, -2, -6}, {-2, 4, -2}, {-6, -2, 4}};
  
  ma = getA[m];
  
  Print[{{0, 1, 4}}.diamondThing];
  
  Join[ma.diamondThing, {{-4, 4, -1}}]

];

leastSquaresSomethingEvenOlder[{{{1, 1, 0}, {0, 1, 4}}, "co"}]

RowReduce[Transpose[{{1, 0, 0}, {-26, -4, 14}, {-4, 4, -1}}]]

1200 * {1, Log[2, 3], Log[2, 5]}.Transpose[{{1, 0, 0}, {14 / 13, -1 / 13, 7 / 26}, {4 / 13, -4 / 13, 14 / 13}}].Transpose[{{1, -2, 1}}] // N
1200 * {1, Log[2, 3], Log[2, 5]}.Transpose[{{1, 0, 0}, {1, 0, 1 / 4}, {0, 0, 1}}].Transpose[{{1, -2, 1}}] // N
{{1, 0, 0}, {1, 0, 1 / 4}, {0, 0, 1}}.Transpose[{{1, -2, 1}}] // N

meantone = {{1, 0, -4}, {0, 1, 4}};
PseudoInverse[meantone].meantone
1200 * {1, Log[2, 3], Log[2, 5]}.PseudoInverse[meantone].meantone.Transpose[{{1, -2, 1}}] // N

Transpose[{{1, 1, 1}, {1, 2, 3}}].{19 / 3, 1 / 2} // N
LeastSquares[Transpose[{{1, 1, 1}, {1, 2, 3}}], {7, 7, 8}]

(*LeastSquares[{{1,1,1},{1,2,3}},Transpose[{7,7,8}]]*)
LeastSquares[Transpose[{{1, 1, 0}, {0, 1, 4}}], {0, 0, 0}]

getGpt[m_] := Module[{decomp, left, snf, right, gpt},
  ma = getA[m];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  gpt = right.Transpose[snf].left;
  
  gpt
];

jip[d_] := Map[Log2, Map[Prime, Range[d]]];

leastSquaresSomethingOld[m_] := Module[{T, diamondThing, unchangedIntervalEigenvectors, commaEigenvectors, eigenvectors, diagonalEigenvalueMatrix, P, gpt, projectedGens},
  ma = getA[m];
  
  T = Transpose[{{1, 1, -1}, {-2, 0, 1}, {2, -1, 0}, {-1, 1, 0}, {3, 0, -1}, {0, -1, 1}}];
  diamondThing = T.Transpose[T];
  
  unchangedIntervalEigenvectors = ma.diamondThing;
  commaEigenvectors = getA[getC[m]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  Print["eigenvectors: ", eigenvectors, " diamondThing: ", diamondThing];
  
  diagonalEigenvalueMatrix = {{1, 0, 0}, {0, 1, 0}, {0, 0, 0}};
  P = eigenvectors.diagonalEigenvalueMatrix.Inverse[eigenvectors];
  
  gpt = getGpt[m];
  projectedGens = P.gpt;
  1200 * jip[getD[m]].projectedGens // N
];

leastSquaresSomethingOld[{{{1, 1, 0}, {0, 1, 4}}, "co"}]




canonicalForm[{{{1, 0, -9}, {0, 1, 4}}, "co"}]
(*getLm[getA[%]]*)

{{1, 1, 0}, {0, 1, 4}}.Transpose[{{-6, -2, 4}}]

leastSquares[m_] := Module[{j, ma, v, unchangedIntervals},
  j = jip[getD[m]];
  ma = getA[m];
  v = Transpose[{{1, 1, -1}, {-2, 0, 1}, {2, -1, 0}, {-1, 1, 0}, {3, 0, -1}, {0, -1, 1}}];
  
  (*Print["unchanged intervals are ", ma.v.Transpose[v]];*)
  
  unchangedIntervals = ma.v.Transpose[v];
  (*Print[1200 * j.Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]] //N];*)
  
  j.Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]] // N
];

1200 * leastSquares[{{{1, 1, 0}, {0, 1, 4}}, "co"}]

(*3/2*) Transpose[{{-1, 1, 0}}];
(*5/4*) Transpose[{{-2, 0, 1}}];
(*5/3*) Transpose[{{0, -1, 1}}];

hnf[{{1, 0, 0}, {-1, 1, 0}}]
hnf[{{1, 0, 0}, {-2, 0, 1}}]
hnf[{{1, 0, 0}, {0, -1, 1}}]

getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[Table[1, Length[unchangedIntervalEigenvectors]], Table[0, Length[commaEigenvectors]]]];

getPm[m_, unchangedIntervalEigenvectors_] := Module[{commaEigenvectors, eigenvectors, diagonalEigenvalueMatrix},
  commaEigenvectors = getA[getC[m]];
  (*Print["hm!? commaEigenvectors: ", commaEigenvectors];*)
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueMatrix = getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors, commaEigenvectors];
  (*Print["diagonalEigenvalueMatrix: ", diagonalEigenvalueMatrix];*)
  
  eigenvectors.diagonalEigenvalueMatrix.Inverse[eigenvectors]
];

getPm[{{{1, 1, 0}, {0, 1, 4}}, "co"}, {{1, 0, 0}, {0, 1, 0}}]
getPm[{{{1, 1, 0}, {0, 1, 4}}, "co"}, {{1, 0, 0}, {0, 0, 1}}]
getPm[{{{1, 1, 0}, {0, 1, 4}}, "co"}, {{1, 0, 0}, {0, 1, -1}}]

DiagonalMatrix[{1, 1, 0, 0}]

maxError[pm_, tima_, justIntervals_, p_] := Module[{errors},
  errors = N[p.pm.Transpose[tima]] - justIntervals;
  
  Max[Map[Abs, errors]]
];

getGpt[m_] := Module[{decomp, left, snf, right, gpt},
  ma = getA[m];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  gpt = right.Transpose[snf].left;
  
  gpt
];

jip[d_] := Map[Log2, Map[Prime, Range[d]]];

gFromMAndPm[m_, pm_] := Module[{gpt, projectedGens},
  gpt = getGpt[m];
  projectedGens = pm.gpt;
  1200 * jip[getD[m]].projectedGens // N
];


minimaxSomethingOld[m_, tim_] := Module[{r, tima, indices, potentialUnchangedIntervalSets, normalizedPotentialUnchangedIntervalSets, filteredNormalizedPotentialUnchangedIntervalSets, p, potentialPms, maxErrors, minimaxIndex, minimaxPm, justIntervals},
  r = getR[m];
  tima = getA[tim];
  indices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, indices];
  (* forgot to filter for bad ones, by hnf'ing them, but I got that later*)
  
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  Print["normalizedPotentialUnchangedIntervalSets: ", normalizedPotentialUnchangedIntervalSets];
  Print[Map[MatrixRank, normalizedPotentialUnchangedIntervalSets]];
  
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  
  p = jip[getD[m]];
  potentialPms = Map[getPm[m, #]&, filteredNormalizedPotentialUnchangedIntervalSets];
  Print["potentialPms: ", potentialPms];
  justIntervals = p.Transpose[tima];
  Print["justIntervals: ", 1200 * justIntervals // N];
  maxErrors = Map[maxError[#, tim, justIntervals, p]&, potentialPms];
  Print["maxErrors: ", maxErrors];
  minimaxIndex = First[First[Position[maxErrors, Min[maxErrors]]]];
  Print["minimaxIndex: ", minimaxIndex];
  minimaxPm = potentialPms[[minimaxIndex]];
  
  Print["projection matrix is: ", minimaxPm];
  
  gFromMAndPm[m, minimaxPm]
];

minimaxSomethingOld[{{{1, 1, 0}, {0, 1, 4}}, "co"}]






