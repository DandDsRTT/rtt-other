Options[optimizeGtm] = {
  "meanPower" -> \[Infinity],
  "weighted" -> False,
  "weightingDirection" -> "regressive",
  "complexityWeighting" -> "P",
  "complexityPower" -> 1,
  "tim" -> Null
};

optimizeGtm[m_, OptionsPattern[]] := Module[{meanPower, weighted, weightingDirection, complexityWeighting, complexityPower, tima},
  meanPower = OptionValue["meanPower"];
  weighted = OptionValue["weighted"];
  weightingDirection = OptionValue["weightingDirection"];
  complexityWeighting = OptionValue["complexityWeighting"];
  complexityPower = OptionValue["complexityPower"];
  tima = If[OptionValue["tim"] === Null, getDiamond[getD[m]], If[Length[OptionValue["tim"]] == 0, {}, getA[OptionValue["tim"]]]];
  
  1200 * If[
    meanPower == \[Infinity],
    optimizeGtmMinimax[m, tima, weighted, weightingDirection, complexityWeighting, complexityPower],
    If[
      meanPower == 2,
      optimizeGtmLeastSquares[m, tima, weighted, weightingDirection, complexityWeighting, complexityPower],
      optimizeGtmLeastAbsolutes[m, tima, weighted, weightingDirection, complexityWeighting, complexityPower]
    ]
  ]
];

optimizeGtmMinimax[m_, tima_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := If[
  weighted == True && weightingDirection == "regressive" && Length[tima] == 0,
  optimizeGtmMinimaxPLimit[m, complexityWeighting, complexityPower],
  If[
    weighted == False,
    optimizeGtmMinimaxConsonanceSetAnalytical[m, tima, weighted, weightingDirection, complexityWeighting, complexityPower],
    optimizeGtmMinimaxConsonanceSetNumerical[m, tima, weighted, weightingDirection, complexityWeighting, complexityPower]
  ]
];

optimizeGtmMinimaxPLimit[m_, complexityWeighting_ , complexityPower_] := If[
  complexityPower == 2,
  optimizeGtmMinimaxPLimitAnalyticalPseudoInverse[m, complexityWeighting],
  optimizeGtmMinimaxPLimitNumericalLinearProgramming[m, complexityWeighting , complexityPower]
];

optimizeGtmMinimaxPLimitAnalyticalPseudoInverse[m_, complexityWeighting_] := Module[{d, ma, weightingMatrix, ptm, gtm, g},
  d = getD[m];
  ma = getA[getM[m]];
  weightingMatrix = getWeightingMatrix[d, complexityWeighting]; (*TODO: maybe consider replacing this with p⨀ if possible, like as is done in `getComplexity` below *)
  ptm = weightingMatrix.Log[2, getPrimes[d]];
  
  g = PseudoInverse[ma.weightingMatrix];
  gtm = ptm.g;
  
  gtm // N
];

optimizeGtmMinimaxPLimitNumericalLinearProgramming[m_, complexityWeighting_, complexityPower_] := Module[{d, ma, weightingMatrix, ptm, gtm, tm, e, solution},
  d = getD[m];
  ma = getA[m];
  weightingMatrix = getWeightingMatrix[d, complexityWeighting];
  ptm = Log[2, getPrimes[d]];
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[m]}];
  
  tm = gtm.ma;
  e = tm - ptm;
  solution = NMinimize[Norm[e.weightingMatrix, dualPower[complexityPower]], gtm, Method -> "NelderMead", WorkingPrecision -> 15];
  gtm /. Last[solution] // N
];

getWeightingMatrix[d_, complexityWeighting_] := If[
  complexityWeighting == "P",
  DiagonalMatrix[1 / getPtm[d]],
  If[
    complexityWeighting == "F",
    DiagonalMatrix[Table[1, d]],
    Error
  ]
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];

optimizeGtmMinimaxConsonanceSetAnalytical[m_, tima_, weighted_, weightingDirection_, complexityWeighting_ , complexityPower_ ] := Module[
  (* TODO: DRY up with optimizeGtmLeastAbsolutes *)
  {
    r,
    d,
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
  
  r = getR[m]; (*TODO: since we use r here maybe use it elsewhere too even though not stictly necessary *)
  d = getD[m];
  ptm = getPtm[d];
  
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromMAndUnchangedIntervals[m, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  meanOfDamages = Map[getMaxDamage[#, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]&, potentialPs];
  
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

optimizeGtmMinimaxConsonanceSetNumerical[m_, tima_, weighted_, weightingDirection_, complexityWeighting_ , complexityPower_ ] := Module[
  {
    d,
    ma,
    ptm,
    gtm,
    mappedTima,
    pureTimaSizes,
    w,
    solution
  },
  
  d = getD[m];
  ma = getA[m];
  ptm = getPtm[d];
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[m]}];
  
  mappedTima = Transpose[ ma.Transpose[tima]];
  pureTimaSizes = Map[ptm.#&, tima];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  
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

optimizeGtmLeastSquares[m_, inputTima_, weighted_, weightingDirection_ , complexityWeighting_, complexityPower_] := Module[{d, ma, ptm, tima, w, unchangedIntervals},
  d = getD[m];
  ma = getA[m];
  ptm = getPtm[d]; (* TODO these three recur a lot, perhaps DRY up their computation *)
  
  w = getW[inputTima, weighted, weightingDirection, complexityWeighting, complexityPower];
  tima = inputTima * w;
  unchangedIntervals = ma.Transpose[tima].tima;
  
  ptm.Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]] // N
];

optimizeGtmLeastAbsolutes[m_, tima_, weighted_, weightingDirection_ , complexityWeighting_ , complexityPower_ ] := Module[
  {
    r,
    d,
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
  
  r = getR[m]; (*TODO: since we use r here maybe use it elsewhere too even though not stictly necessary *)
  d = getD[m];
  ptm = getPtm[d];
  
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromMAndUnchangedIntervals[m, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  meanOfDamages = Map[getSumOfAbsolutesDamage[#, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]&, potentialPs];
  
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

getSumOfAbsolutesDamage[p_, tima_, ptm_, weighted_, weightingDirection_ , complexityWeighting_ , complexityPower_] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  Total[Map[Abs, e * w]]
];

getSumOfSquaresDamage[p_, tima_, ptm_, weighted_, weightingDirection_ , complexityWeighting_ , complexityPower_] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower]; (* TODO: DRY up with getSumOfAbsolutesDamage *)
  
  Total[Map[#^2&, e * w]]
];

getMaxDamage[p_, tima_, ptm_, weighted_, weightingDirection_ , complexityWeighting_ , complexityPower_] := Module[{e, w},
  e = N[ptm.p.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower]; (* TODO: DRY up with getSumOfAbsolutesDamage *)
  
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

tieBreak[tiedPs_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_ ] := Module[{meanOfDamages},
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

getPtm[d_] := Map[Log2, Map[Prime, Range[d]]];

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

getComplexity[pcv_, complexityWeighting_ , complexityPower_ ] := Module[{d, weightedPcv},
  d = Length[pcv];
  weightedPcv = If[
    complexityWeighting == "F",
    pcv,
    If[
      complexityWeighting == "P",
      pcv * getPtm[d]
    ]
  ];
  
  Norm[weightedPcv, complexityPower]
];

getW[tima_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{w},
  w = If[
    weighted,
    Map[getComplexity[#, complexityWeighting, complexityPower]&, tima],
    Map[1&, tima]
  ];
  
  If[weightingDirection == "regressive", 1 / w, w]
];
