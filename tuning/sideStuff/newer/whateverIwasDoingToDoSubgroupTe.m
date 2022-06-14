(*
  
  GENERATORS PREIMAGE TRANSVERSAL
  
  
  getGeneratorsPreimageTransversal[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns a generators preimage transversal 
  (for each generator, one JI interval that maps to it).
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        getGeneratorsPreimageTransversal[meantoneM]
    
  Out   {{{1, 0, 0}, {-1, 1, 0}}, "contra"}
  
*)
getGeneratorsPreimageTransversal[t_] := Module[{ma, decomp, left, snf, right, generatorsPreimageTransversal},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorsPreimageTransversal = right.Transpose[snf].left;
  
  {Transpose[generatorsPreimageTransversal], "contra"}
];


(*
  
  TUNING
  
  
  optimizeGeneratorsTuningMap[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimal generator tuning map.
  
  The tuning may be specified by name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM]
    
  Out   {1200., 696.578}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "originalTuningSchemeName" -> "TOP"]
    
  Out   {1201.7, 697.564}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "systematicTuningSchemeName" -> "minisos-MEC"]
    
  Out   {1198.24, 695.294}
*)
Options[optimizeGeneratorsTuningMap] = tuningSchemeProperties;
optimizeGeneratorsTuningMap[t_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    systematicTuningSchemeName,
    originalTuningSchemeName,
    tuningSchemeProperties
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  weighted = OptionValue["weighted"];
  weightingDirection = OptionValue["damageWeightingSlope"];
  complexityWeighting = OptionValue["originalComplexityName"];
  complexityPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  systematicTuningSchemeName = OptionValue["systematicTuningSchemeName"];
  originalTuningSchemeName = OptionValue["originalTuningSchemeName"];
  
  tuningSchemeProperties = processTuningSchemeOptions[t, optimizationPower, weighted, weightingDirection, complexityWeighting, complexityPower, tim, originalTuningSchemeName, systematicTuningSchemeName];
  optimizationPower = First[tuningSchemeProperties];
  
  Print["tim?", tim];
  
  1200 * If[
    optimizationPower == \[Infinity],
    optimizeGtmMinimax[tuningSchemeProperties],
    If[
      optimizationPower == 2,
      optimizeGtmLeastSquares[tuningSchemeProperties],
      optimizeGtmLeastAbsolutes[tuningSchemeProperties]
    ]
  ]
];




(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGtmMinimax[{optimizationPower_, tima_, d_, t_, ptm_, weightingDirection_, complexityWeighting_, complexityPower_}] := If[
  weighted == True && weightingDirection == "simplicityWeighted" && Length[tima] == 0,
  optimizeGtmMinimaxPLimit[d, t, ptm, complexityWeighting, complexityPower],
  If[
    weighted == False,
    optimizeGtmMinimaxConsonanceSetAnalytical[optimizationPower, tima, d, t, ptm, weightingDirection, complexityWeighting, complexityPower],
    optimizeGtmMinimaxConsonanceSetNumerical[tima, d, t, ptm, weightingDirection, complexityWeighting, complexityPower]
  ]
];


(* INFINITE-TARGET-SET MINIMAX *)

optimizeGtmMinimaxPLimit[d_, t_, ptm_, complexityWeighting_, complexityPower_] := If[
  complexityPower == 2,
  optimizeGtmMinimaxPLimitPseudoInverseAnalytical[d, t, ptm, complexityWeighting],
  optimizeGtmMinimaxPLimitLinearProgrammingNumerical[d, t, ptm, complexityWeighting, complexityPower]
];

optimizeGtmMinimaxPLimitPseudoInverseAnalytical[d_, t_, ptm_, complexityWeighting_] := Module[{w, tima, weightedTima, unchangedIntervals, g, gtm},
  w = If[complexityWeighting == "logProduct", 1 / ptm, Table[1, d]];
  tima = IdentityMatrix[d];
  
  optimizeGtmWithPseudoInverse[tima, w, t, ptm]
];

optimizeGtmMinimaxPLimitLinearProgrammingNumerical[d_, t_, ptm_, complexityWeighting_, complexityPower_] := Module[{gtm, ma, tm, e, solution},
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tm = gtm.ma;
  e = If[complexityWeighting == "logProduct", tm / ptm - Table[1, d], tm - ptm];
  
  solution = NMinimize[Norm[e, dualPower[complexityPower]], gtm, Method -> "NelderMead", WorkingPrecision -> 15];
  gtm /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];


(* FINITE-TARGET-SET MINIMAX *)

optimizeGtmMinimaxConsonanceSetAnalytical[optimizationPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower, getMaxDamage];

optimizeGtmMinimaxConsonanceSetNumerical[tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[
  {
    gtm,
    ma,
    mappedTima,
    pureTimaSizes,
    w,
    solution
  },
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  mappedTima = Transpose[ma.Transpose[tima]];
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


(* MINISOS *)

optimizeGtmLeastSquares[{optimizationPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] := Module[{w, weightedTima, unchangedIntervals, g, gtm},
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  optimizeGtmWithPseudoInverse[tima, w, t, ptm]
];

optimizeGtmWithPseudoInverse[tima_, w_, t_, ptm_] := Module[{ma, weightedTima, unchangedIntervals, g, gtm},
  ma = getA[getM[t]];
  
  
  
  (*SOME  BULLSHIT I DUNNO *)
  (*
  {{1,0,0,0},{0,FractionBox[RowBox[{"Log", "[", "2", "]"}], RowBox[{"2", " ", RowBox[{"Log", "[", "3", "]"}]}]],0,0},{0,0,FractionBox[RowBox[{"Log", "[", "2", "]"}], RowBox[{"Log", "[", "5", "]"}]],0},{0,-FractionBox[RowBox[{"Log", "[", "2", "]"}], RowBox[{"2", " ", RowBox[{"Log", "[", "7", "]"}]}]],0,FractionBox[RowBox[{"Log", "[", "2", "]"}], RowBox[{"Log", "[", "7", "]"}]]}};
  Print["what is w", w, DiagonalMatrix[w]];
  weightedTima = tima .DiagonalMatrix[w];
  Print["weightedTima, what we're actually going to use: ", tima .DiagonalMatrix[w], " what wegihtedTima used to be: ", tima *w];
  *)
  
  
  (* SUBGROUP TE *)
  
  weightedTima = tima.Inverse[{{1.`, 0.`, 0.`, 0.`}, {0.`, 3.1699250014423126`, 0.`, 0.`}, {0.`, 0.`, 2.321928094887362`, 0.`}, {0.`, 1.5849625007211563`, 0.`, 2.807354922057604`}}];
  Print["subgroup already N'd:", Inverse[{{1.`, 0.`, 0.`, 0.`}, {0.`, 3.1699250014423126`, 0.`, 0.`}, {0.`, 0.`, 2.321928094887362`, 0.`}, {0.`, 1.5849625007211563`, 0.`, 2.807354922057604`}}]];
  Print["subgroup recreated, N'd: ", Inverse[{{1, 0, 0, 0}, {0, 2 * Log[2, 3], 0, 0}, {0, 0, Log[2, 5], 0}, {0, Log[2, 3], 0, Log[2, 7]}}] // N];
  Print["subgroup recreated, not N'd: ", Inverse[{{1, 0, 0, 0}, {0, 2 * Log[2, 3], 0, 0}, {0, 0, Log[2, 5], 0}, {0, Log[2, 3], 0, Log[2, 7]}}]];
  
  (* INHARMONIC TE *)
  (* weightedTima = tima.DiagonalMatrix[w];*)
  Print["inharmonic, N'd: ", DiagonalMatrix[w] // N];
  Print["inharmonic, not N'd: ", DiagonalMatrix[w]];
  
  
  
  (* SOME OTHER BULLSHIT I DUNNO *)
  (*{{1,0,0,0},{0,1.585,0,0},{0,0,2.232,0},{0,1.585,0,2.807}};*)(*{{1.`,0.`,0.`,0.`},{0.`,0.13123897852228736`,0.`,-0.06344178007370475`},{0.`,0.`,0.18548229767394472`,0.`},{0.`,-0.06344178007370475`,0.`,0.1268835601474095`}};*)
  (* Print["but now hahahahahah weightedTima", weightedTima];*)
  
  
  
  
  
  
  unchangedIntervals = ma.Transpose[weightedTima].weightedTima;
  g = Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]];
  gtm = ptm.g;
  Print["i want my MTV", ptm.{{1.`, 0.`, 0.`, 0.`}, {0.`, 0.13123897852228736`, 0.`, -0.06344178007370475`}, {0.`, 0.`, 0.18548229767394472`, 0.`}, {0.`, -0.06344178007370475`, 0.`, 0.1268835601474095`}}];
  gtm // N
  (*thing = ma.weightedTima;
  ptm.weightedTima.PseudoInverse[thing] // N*)
];


(* MINISUM *)

optimizeGtmLeastAbsolutes[{optimizationPower_, tima_, d_, t_, ptm_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, weightingDirection, complexityWeighting, complexityPower, getSumOfAbsolutesDamage];


(* SIMPLEX (USED BY ANALYTICAL MINIMAX AND MINISUM) *)

optimizeGtmSimplex[optimizationPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_, damageMean_] := Module[
  {
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialPs,
    potentialTms,
    meanOfDamages,
    minMeanIndices,
    minMeanIndex,
    tiedTms,
    tiedPs,
    minMeanP,
    generatorsPreimageTransversal,
    projectedGenerators
  },
  
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromUnchangedIntervals[t, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  potentialTms = Map[ptm.#&, potentialPs];
  meanOfDamages = Map[damageMean[#, {optimizationPower, tima, d, t, ptm, weightingDirection, complexityWeighting, complexityPower}]&, potentialTms];
  
  minMeanIndices = Position[meanOfDamages, Min[meanOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    
    minMeanIndex = First[First[Position[meanOfDamages, Min[meanOfDamages]]]];
    minMeanP = potentialPs[[minMeanIndex]],
    
    tiedTms = Part[potentialTms, Flatten[minMeanIndices]];
    tiedPs = Part[potentialPs, Flatten[minMeanIndices]];
    minMeanIndex = tieBreak[tiedTms, optimizationPower, tima, d, t, ptm, weightingDirection, complexityWeighting, complexityPower];
    minMeanP = tiedPs[[minMeanIndex]]
  ];
  
  generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[t]]];
  projectedGenerators = minMeanP.generatorsPreimageTransversal;
  ptm.projectedGenerators // N
];

getPFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[{commaEigenvectors, eigenvectors, diagonalEigenvalueMatrix},
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueMatrix = getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueMatrix.Inverse[eigenvectors]]
];

getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];


(* TARGET INTERVAL SETS *)

getDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  padVectorsWithZerosUpToD[Map[quotientToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;


(* DAMAGE *)

Options[getDamage] = tuningSchemeProperties;
getDamage[t_, gtm_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean,
    ma,
    tm,
    tuningSchemeProperties
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  weightingDirection = OptionValue["damageWeightingSlope"];
  complexityWeighting = OptionValue["originalComplexityName"];
  complexityPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  damage = OptionValue["damage"];
  tuning = OptionValue["originalTuningSchemeName"];
  mean = OptionValue["optimization"];
  
  tuningSchemeProperties = processTuningSchemeOptions[t, optimizationPower, weighted, weightingDirection, complexityWeighting, complexityPower, tim, damage, tuning, mean, True];
  optimizationPower = First[tuningSchemeProperties];
  ma = getA[getM[t]];
  
  tm = (gtm / 1200).ma;
  
  If[
    optimizationPower == \[Infinity],
    getMaxDamage[tm, tuningSchemeProperties],
    If[
      optimizationPower == 2,
      getSumOfSquaresDamage[tm, tuningSchemeProperties],
      getSumOfAbsolutesDamage[tm, tuningSchemeProperties]
    ]
  ]
];

getTid[tm_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{e, w},
  e = N[tm.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  e * w
];

Square[n_] := n^2;

getSumOfAbsolutesDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Total[Map[Abs, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

getSumOfSquaresDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Total[Map[Square, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

getMaxDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Max[Map[Abs, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

tieBreak[tiedTms_, optimizationPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{meanOfDamages},
  meanOfDamages = Map[getSumOfSquaresDamage[#, {optimizationPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}]&, tiedTms];
  
  First[First[Position[meanOfDamages, Min[meanOfDamages]]]]
];


(* SHARED *)

tuningSchemeProperties = {
  "optimizationPower" -> \[Infinity],
  "weighted" -> False,
  "damageWeightingSlope" -> "simplicityWeighted",
  "originalComplexityName" -> "logProduct",
  "complexityNormPower" -> 1,
  "tim" -> Null,
  "damage" -> "",
  "optimization" -> "",
  "originalTuningSchemeName" -> ""
};

processTuningSchemeOptions[t_, inputMeanPower_, inputWeighted_, inputWeightingDirection_, inputComplexityWeighting_, inputComplexityPower_, inputTim_, inputDamage_, inputTuning_, inputMean_, forDamage_ : False] := Module[
  {
    tima,
    damageParts,
    d,
    ptm,
    optimizationPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean
  },
  
  optimizationPower = inputMeanPower;
  weighted = inputWeighted;
  weightingDirection = inputWeightingDirection;
  complexityWeighting = inputComplexityWeighting;
  complexityPower = inputComplexityPower;
  tim = inputTim;
  damage = inputDamage;
  tuning = inputTuning;
  mean = inputMean;
  
  If[
    tuning === "TOP",
    damage = "S"; tim = {},
    If[
      tuning === "TE",
      damage = "ES"; tim = {},
      If[
        tuning === "Partch",
        damage = "C",
        If[
          tuning === "Frobenius",
          damage = "MES"; tim = {},
          If[
            tuning === "least squares",
            optimizationPower = 2,
            If[
              tuning === "least absolutes",
              optimizationPower = 1,
              If[
                tuning === "Tenney least squares",
                optimizationPower = 2; damage = "S"
              ]
            ]
          ]
        ]
      ]
    ]
  ];
  
  damageParts = StringPartition[damage, 1];
  If[
    Length[damageParts] === 3,
    weighted = True;
    weightingDirection = "complexityWeighted";
    complexityWeighting = damageParts[[2]];
    complexityPower = ToExpression[damageParts[[3]]],
    If[
      Length[damageParts] === 2,
      weighted = True;
      weightingDirection = "simplicityWeighted";
      complexityWeighting = damageParts[[1]];
      complexityPower = ToExpression[damageParts[[2]]];
    ]
  ];
  
  If[
    mean === "minisum",
    optimizationPower = 1,
    If[
      mean === "minisos",
      optimizationPower = 2,
      If[
        mean === "minimax",
        optimizationPower = \[Infinity]
      ]
    ]
  ];
  
  d = getD[t];
  ptm = getPrimesTuningMap[d];
  
  tima = If[tim === Null, getDiamond[d], If[Length[tim] == 0, If[forDamage, getA[getC[t]], {}], getA[tim]]];
  Print["tima", tima];
  
  {optimizationPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}
];

(*getPrimesTuningMap[d_] := Log[2, getPrimes[d]];*)
getPrimesTuningMap[d_] := Log[2, {2, 9, 5, 21}];

getW[tima_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{w},
  w = If[
    weighted,
    Map[getComplexity[#, complexityWeighting, complexityPower]&, tima],
    Map[1&, tima]
  ];
  
  If[weightingDirection == "simplicityWeighted", 1 / w, w]
];

getComplexity[pcv_, complexityWeighting_, complexityPower_] := Module[{weightedPcv},
  weightedPcv = If[complexityWeighting == "logProduct", pcv * getPrimesTuningMap[Length[pcv]], pcv];
  Norm[weightedPcv, complexityPower]
];


optimizeGeneratorsTuningMap[{{{12, 19, 28}}, "co"}, {"tim" -> {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, "contra"}}]

optimizeGeneratorsTuningMap[{{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co"}, {"originalTuningSchemeName" -> "TE"}]

changeIntervalBasis[{{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}}, {2, 3, 5, 7}]
changeIntervalBasis[{{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 3, 5, 7}}, {2, 9, 5, 21}]

weightedTranslation = {{1, 0, 0, 0}, {0, 2 * Log[2, 3], 0, 0}, {0, 0, 1 * Log[2, 5], 0}, {0, 1 * Log[2, 3], 0, 1 * Log[2, 7]}} // N

weightedTranslation.Transpose[weightedTranslation]
(*Transpose[weightedTranslation] *weightedTranslation
Transpose[weightedTranslation]*Transpose[weightedTranslation]*)
Inverse[%]

{{1, 0, 0, 0}, {0, 2 * Log[2, 3], 0, 0}, {0, 0, 1 * Log[2, 5], 0}, {0, 1 * Log[2, 3], 0, 1 * Log[2, 7]}}
Inverse[%]
% // N
