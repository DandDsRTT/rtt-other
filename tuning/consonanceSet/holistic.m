optimizeGtm[m_, meanPower_, weighted_ : False, weightingDirection_ : "regressive", complexityWeighting_ : "P", complexityPower_ : 1, tim_ : Null] := Module[{},
  Print[meanPower, " ", weighted, " ", weightingDirection, " ", complexityWeighting, " ", complexityPower, " ", tim];
  1200 * If[
    meanPower == \[Infinity],
    optimizeGtmMinimax[m, tim, weighted, weightingDirection, complexityWeighting, complexityPower],
    If[
      meanPower == 2,
      optimizeGtmLeastSquares[m, tim, weighted, weightingDirection, complexityWeighting, complexityPower],
      optimizeGtmLeastAbsolutes[m, tim, weighted, weightingDirection, complexityWeighting, complexityPower]
    ]
  ]
];

optimizeGtmMinimax[m_, tim_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := If[
  weighted == True && weightingDirection == "regressive" && Length[tim] == 0,
  optimizeGtmMinimaxPLimit[m, complexityWeighting, complexityPower],
  optimizeGtmMinimaxConsonanceSet[m, tim, weighted, weightingDirection, complexityWeighting, complexityPower]
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

optimizeGtmMinimaxConsonanceSet[m_, tim_, weighted_, weightingDirection_, complexityWeighting_ , complexityPower_ ] := Module[
  {
    d,
    ma,
    ptm,
    gtm,
    tima,
    mappedTima,
    pureTimaSizes,
    w,
    solution
  },
  
  d = getD[m];
  ma = getA[m];
  ptm = getPtm[d];
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[m]}];
  
  tima = If[tim === Null, getDiamond[d], getA[tim]];
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

optimizeGtmLeastSquares[m_, tim_, weighted_, weightingDirection_ , complexityWeighting_, complexityPower_] := Module[{d, ma, ptm, tima, w, unchangedIntervals},
  d = getD[m];
  ma = getA[m];
  ptm = getPtm[d]; (* TODO these three recur a lot, perhaps DRY up their computation *)
  
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  tima = tima * w;
  unchangedIntervals = ma.Transpose[tima].tima;
  
  ptm.Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]] // N
];

optimizeGtmLeastAbsolutes[m_, tim_, weighted_, weightingDirection_ , complexityWeighting_ , complexityPower_ ] := Module[
  {
    r,
    d,
    ptm,
    tima,
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
  
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  
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


(* TODO: these can begin to become the tests *)

m = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)
m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)
(*m = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}; (*miracle *)
m = {{{1, 2, 1,1}, {0, -1, 3, 4}}, "co"}; (*pelogic *)
m = {{{1, 2, 3}, {0, -3, -5}}, "co"}; (*porcupine*)
m = {{{5, 8, 12}, {0, 0, -1}}, "co"}; (* blackwood *)*)


optimizeGtm[m, \[Infinity]]

optimizeGtm[m, \[Infinity], True, "regressive", "F", 1, {}]
optimizeGtm[m, \[Infinity], True, "regressive", "F", 2, {}]
optimizeGtm[m, \[Infinity], True, "regressive", "P", 1, {}]
optimizeGtm[m, \[Infinity], True, "regressive", "P", 2, {}]

optimizeGtm[m, \[Infinity], True, "regressive", "F"]
optimizeGtm[m, \[Infinity], True, "regressive", "F", 2]
optimizeGtm[m, \[Infinity], True]
optimizeGtm[m, \[Infinity], True, "regressive", "P", 2]

optimizeGtm[m, \[Infinity], True, "progressive", "F"]
optimizeGtm[m, \[Infinity], True, "progressive", "F", 2]
optimizeGtm[m, \[Infinity], True, "progressive"]
optimizeGtm[m, \[Infinity], True, "progressive", "P", 2]


optimizeGtm[m, 2]

optimizeGtm[m, 2, True, "regressive", "F"]
optimizeGtm[m, 2, True, "regressive", "F", 2]
optimizeGtm[m, 2, True]
optimizeGtm[m, 2, True, "regressive", "P", 2]

optimizeGtm[m, 2, True, "progressive", "F"]
optimizeGtm[m, 2, True, "progressive", "F", 2]
optimizeGtm[m, 2, True, "progressive"]
optimizeGtm[m, 2, True, "progressive", "P", 2]


optimizeGtm[m, 1]

optimizeGtm[m, 1, True, "regressive", "F"]
optimizeGtm[m, 1, True, "regressive", "F", 2]
optimizeGtm[m, 1, True]
optimizeGtm[m, 1, True, "regressive", "P", 2]

optimizeGtm[m, 1, True, "progressive", "F"]
optimizeGtm[m, 1, True, "progressive", "F", 2]
optimizeGtm[m, 1, True, "progressive"]
optimizeGtm[m, 1, True, "progressive", "P", 2]
