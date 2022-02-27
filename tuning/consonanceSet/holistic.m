Options[optimizeGtm] = {
  "meanPower" -> \[Infinity],
  "weighted" -> False,
  "weightingDirection" -> "regressive",
  "complexityWeighting" -> "P",
  "complexityPower" -> 1,
  "tim" -> Null
};

optimizeGtm[m_, OptionsPattern[]] := Module[{meanPower, weighted, weightingDirection, complexityWeighting, complexityPower, tima, d, ma, ptm},
  meanPower = OptionValue["meanPower"];
  weighted = OptionValue["weighted"];
  weightingDirection = OptionValue["weightingDirection"];
  complexityWeighting = OptionValue["complexityWeighting"];
  complexityPower = OptionValue["complexityPower"];
  tima = If[OptionValue["tim"] === Null, getDiamond[getD[m]], If[Length[OptionValue["tim"]] == 0, {}, getA[OptionValue["tim"]]]];
  
  d = getD[m];
  ma = getA[getM[m]];
  ptm = getPtm[d];
  
  1200 * If[
    meanPower == \[Infinity],
    optimizeGtmMinimax[m, tima, d, ma, ptm, weighted, weightingDirection, complexityWeighting, complexityPower],
    If[
      meanPower == 2,
      optimizeGtmLeastSquares[m, tima, d, ma, ptm, weighted, weightingDirection, complexityWeighting, complexityPower],
      optimizeGtmLeastAbsolutes[m, tima, d, ma, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]
    ]
  ]
];

optimizeGtmMinimax[m_, tima_, d_, ma_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := If[
  weighted == True && weightingDirection == "regressive" && Length[tima] == 0,
  optimizeGtmMinimaxPLimit[m, d, ma, ptm, complexityWeighting, complexityPower],
  If[
    weighted == False,
    optimizeGtmMinimaxConsonanceSetAnalytical[m, tima, d, ma, ptm, weighted, weightingDirection, complexityWeighting, complexityPower],
    optimizeGtmMinimaxConsonanceSetNumerical[m, tima, d, ma, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]
  ]
];

optimizeGtmMinimaxPLimit[m_, d_, ma_, ptm_, complexityWeighting_, complexityPower_] := If[
  complexityPower == 2,
  optimizeGtmMinimaxPLimitPseudoInverseAnalytical[m, d, ma, ptm, complexityWeighting],
  optimizeGtmMinimaxPLimitLinearProgrammingNumerical[m, d, ma, ptm, complexityWeighting, complexityPower]
];

optimizeGtmMinimaxPLimitPseudoInverseAnalytical[m_, d_, ma_, ptm_, complexityWeighting_] := Module[{weightingMatrix, g, gtm},
  weightingMatrix = getWeightingMatrix[d, complexityWeighting];
  g = weightingMatrix.PseudoInverse[ma.weightingMatrix];
  gtm = ptm.g;
  
  gtm // N
];

optimizeGtmMinimaxPLimitLinearProgrammingNumerical[m_, d_, ma_, ptm_, complexityWeighting_, complexityPower_] := Module[{gtm, tm, e, solution},
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[m]}];
  tm = gtm.ma;
  e = If[complexityWeighting == "P", tm / ptm - Table[1, d], tm - ptm];
  
  solution = NMinimize[Norm[e, dualPower[complexityPower]], gtm, Method -> "NelderMead", WorkingPrecision -> 15];
  gtm /. Last[solution] // N
];

getWeightingMatrix[d_, complexityWeighting_] := If[
  complexityWeighting == "P",
  DiagonalMatrix[1 / getPtm[d]],
  IdentityMatrix[d]
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];

optimizeGtmMinimaxConsonanceSetAnalytical[m_, tima_, d_, ma_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    optimizeGtmSimplex[m, tima, d, ma, ptm, weighted, weightingDirection, complexityWeighting, complexityPower, getMaxDamage];

optimizeGtmMinimaxConsonanceSetNumerical[m_, tima_, d_, ma_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[
  {
    gtm,
    mappedTima,
    pureTimaSizes,
    w,
    solution
  },
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[m]}];
  
  mappedTima = Transpose[ma.Transpose[tima]];
  pureTimaSizes = Map[ptm.#&, tima];
  w = getW[tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  solution = NMinimize[
    Max[
      MapIndexed[
        Function[
          {mappedTi, tiIndex},
          Abs[
            Total[
              MapThread[
                Function[
                  {mappedTiEntry, gtmEntry},
                  mappedTiEntry * gtmEntry
                ],
                {mappedTi, gtm}
              ]
            ] - pureTimaSizes[[tiIndex]]
          ] * w[[tiIndex]]
        ],
        mappedTima
      ]
    ],
    gtm,
    Method -> "NelderMead",
    WorkingPrecision -> 15
  ];
  
  gtm /. Last[solution] // N
];

optimizeGtmLeastSquares[m_, tima_, d_, ma_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{w, weightedTima, unchangedIntervals},
  w = getW[tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower];
  weightedTima = tima * w;
  unchangedIntervals = ma.Transpose[weightedTima].weightedTima;
  
  ptm.Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]] // N
];

optimizeGtmSimplex[m_, tima_, d_, ma_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_, damageMean_] := Module[
  {
    r,
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
  
  r = getR[m];
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromMAndUnchangedIntervals[m, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  meanOfDamages = Map[damageMean[#, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]&, potentialPs];
  
  minMeanIndices = Position[meanOfDamages, Min[meanOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    minMeanIndex = First[First[Position[meanOfDamages, Min[meanOfDamages]]]];
    minMeanP = potentialPs[[minMeanIndex]],
    tiedPs = Part[potentialPs, Flatten[minMeanIndices]];
    minMeanP = tieBreak[tiedPs, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]
  ];
  
  gpt = getGpt[m];
  projectedGenerators = minMeanP.gpt;
  ptm.projectedGenerators // N
];

optimizeGtmLeastAbsolutes[m_, tima_, d_, ma_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    optimizeGtmSimplex[m, tima, d, ma, ptm, weighted, weightingDirection, complexityWeighting, complexityPower, getSumOfAbsolutesDamage];

getTid[p_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  e * w
];

Square[n_] := n^2;

getSumOfAbsolutesDamage[p_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    Total[Map[Abs, getTid[p, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

getSumOfSquaresDamage[p_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    Total[Map[Square, getTid[p, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

getMaxDamage[p_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    Max[Map[Abs, getTid[p, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

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

tieBreak[tiedPs_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{meanOfDamages},
  meanOfDamages = Map[getSumOfSquaresDamage[#, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]&, tiedPs];
  
  tiedPs[[First[First[Position[meanOfDamages, Min[meanOfDamages]]]]]]
];

getGpt[m_] := Module[{ma, decomp, left, snf, right, gpt},
  ma = getA[m];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  gpt = right.Transpose[snf].left;
  
  gpt
];

getPtm[d_] := Log[2, getPrimes[d]];

getDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  padD[Map[rationalToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;

getComplexity[pcv_, ptm_, complexityWeighting_, complexityPower_] := Module[{weightedPcv},
  weightedPcv = If[complexityWeighting == "P", pcv * ptm, pcv];
  
  Norm[weightedPcv, complexityPower]
];

getW[tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{w},
  w = If[
    weighted,
    Map[getComplexity[#, ptm, complexityWeighting, complexityPower]&, tima],
    Map[1&, tima]
  ];
  
  If[weightingDirection == "regressive", 1 / w, w]
];
