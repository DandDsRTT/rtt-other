(*
  
  GENERATORS PREIMAGE TRANSVERSAL
  
  
  getGpt[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns a generators preimage transversal 
  (for each generator, one JI interval that maps to it).
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        getGpt[meantoneM]
    
  Out   {{{1, 0, 0}, {-1, 1, 0}}, "contra"}
  
*)
getGpt[t_] := Module[{ma, decomp, left, snf, right, gpt},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  gpt = right.Transpose[snf].left;
  
  {Transpose[gpt], "contra"}
];


(*
  
  TUNING
  
  
  optimizeGtm[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimal generator tuning map.
  
  The tuning may be specified by name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM]
    
  Out   {1200., 696.578}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "tuning" -> "Tenney"]
    
  Out   {1201.7, 697.564}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "mean" -> "RMS", "damage" -> "pF2"]
    
  Out   {1198.24, 695.294}
*)
Options[optimizeGtm] = tuningOptions;
optimizeGtm[t_, OptionsPattern[]] := Module[
  {
    meanPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean,
    tuningOptions
  },
  
  meanPower = OptionValue["meanPower"];
  weighted = OptionValue["weighted"];
  weightingDirection = OptionValue["weightingDirection"];
  complexityWeighting = OptionValue["complexityWeighting"];
  complexityPower = OptionValue["complexityPower"];
  tim = OptionValue["tim"];
  damage = OptionValue["damage"];
  tuning = OptionValue["tuning"];
  mean = OptionValue["mean"];
  
  tuningOptions = processTuningOptions[t, meanPower, weighted, weightingDirection, complexityWeighting, complexityPower, tim, damage, tuning, mean];
  meanPower = First[tuningOptions];
  
  1200 * If[
    meanPower == \[Infinity],
    optimizeGtmMinimax[tuningOptions],
    If[
      meanPower == 2,
      optimizeGtmLeastSquares[tuningOptions],
      optimizeGtmLeastAbsolutes[tuningOptions]
    ]
  ]
];




(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGtmMinimax[{meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] := If[
  weighted == True && weightingDirection == "regressive" && Length[tima] == 0,
  optimizeGtmMinimaxPLimit[d, t, ptm, complexityWeighting, complexityPower],
  If[
    weighted == False,
    optimizeGtmMinimaxConsonanceSetAnalytical[meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower],
    optimizeGtmMinimaxConsonanceSetNumerical[tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]
  ]
];


(* MINIMAX P-LIMIT *)

optimizeGtmMinimaxPLimit[d_, t_, ptm_, complexityWeighting_, complexityPower_] := If[
  complexityPower == 2,
  optimizeGtmMinimaxPLimitPseudoInverseAnalytical[d, t, ptm, complexityWeighting],
  optimizeGtmMinimaxPLimitLinearProgrammingNumerical[d, t, ptm, complexityWeighting, complexityPower]
];

optimizeGtmMinimaxPLimitPseudoInverseAnalytical[d_, t_, ptm_, complexityWeighting_] := Module[{w, tima, weightedTima, unchangedIntervals, g, gtm},
  w = If[complexityWeighting == "P", 1 / ptm, Table[1, d]];
  tima = IdentityMatrix[d];
  
  optimizeGtmWithPseudoInverse[tima, w, t, ptm]
];

optimizeGtmMinimaxPLimitLinearProgrammingNumerical[d_, t_, ptm_, complexityWeighting_, complexityPower_] := Module[{gtm, ma, tm, e, solution},
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tm = gtm.ma;
  e = If[complexityWeighting == "P", tm / ptm - Table[1, d], tm - ptm];
  
  solution = NMinimize[Norm[e, dualPower[complexityPower]], gtm, Method -> "NelderMead", WorkingPrecision -> 15];
  gtm /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];


(* MINIMAX CONSONANCE-SET *)

optimizeGtmMinimaxConsonanceSetAnalytical[meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    optimizeGtmSimplex[meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower, getMaxDamage];

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


(* LEAST-SQUARES *)

optimizeGtmLeastSquares[{meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] := Module[{w, weightedTima, unchangedIntervals, g, gtm},
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  optimizeGtmWithPseudoInverse[tima, w, t, ptm]
];

optimizeGtmWithPseudoInverse[tima_, w_, t_, ptm_] := Module[{ma, weightedTima, unchangedIntervals, g, gtm},
  ma = getA[getM[t]];
  weightedTima = tima * w;
  unchangedIntervals = ma.weightedTima;
  g = PseudoInverse[unchangedIntervals];
  gtm = ptm.g;
  gtm // N
];


(* LEAST-ABSOLUTES *)

optimizeGtmLeastAbsolutes[{meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    optimizeGtmSimplex[meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower, getSumOfAbsolutesDamage];


(* SIMPLEX (USED BY ANALYTICAL MINIMAX AND LEAST-ABSOLUTES) *)

optimizeGtmSimplex[meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_, damageMean_] := Module[
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
    gpt,
    projectedGenerators
  },
  
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromUnchangedIntervals[t, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  potentialTms = Map[ptm.#&, potentialPs];
  meanOfDamages = Map[damageMean[#, {meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}]&, potentialTms];
  
  minMeanIndices = Position[meanOfDamages, Min[meanOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    
    minMeanIndex = First[First[Position[meanOfDamages, Min[meanOfDamages]]]];
    minMeanP = potentialPs[[minMeanIndex]],
    
    tiedTms = Part[potentialTms, Flatten[minMeanIndices]];
    tiedPs = Part[potentialPs, Flatten[minMeanIndices]];
    minMeanIndex = tieBreak[tiedTms, meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower];
    minMeanP = tiedPs[[minMeanIndex]]
  ];
  
  gpt = Transpose[getA[getGpt[t]]];
  projectedGenerators = minMeanP.gpt;
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
  
  padD[Map[rationalToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;


(* DAMAGE *)

Options[getDamage] = tuningOptions;
getDamage[t_, gtm_, OptionsPattern[]] := Module[
  {
    meanPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean,
    ma,
    tm,
    tuningOptions
  },
  
  meanPower = OptionValue["meanPower"];
  weighted = OptionValue["weighted"];
  weightingDirection = OptionValue["weightingDirection"];
  complexityWeighting = OptionValue["complexityWeighting"];
  complexityPower = OptionValue["complexityPower"];
  tim = OptionValue["tim"];
  damage = OptionValue["damage"];
  tuning = OptionValue["tuning"];
  mean = OptionValue["mean"];
  
  tuningOptions = processTuningOptions[t, meanPower, weighted, weightingDirection, complexityWeighting, complexityPower, tim, damage, tuning, mean, True];
  meanPower = First[tuningOptions];
  ma = getA[getM[t]];
  
  tm = (gtm / 1200).ma;
  
  If[
    meanPower == \[Infinity],
    getMaxDamage[tm, tuningOptions],
    If[
      meanPower == 2,
      getSumOfSquaresDamage[tm, tuningOptions],
      getSumOfAbsolutesDamage[tm, tuningOptions]
    ]
  ]
];

getTid[tm_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{e, w},
  e = N[tm.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  e * w
];

Square[n_] := n^2;

getSumOfAbsolutesDamage[tm_, {meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Total[Map[Abs, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

getSumOfSquaresDamage[tm_, {meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Total[Map[Square, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

getMaxDamage[tm_, {meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Max[Map[Abs, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

tieBreak[tiedTms_, meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{meanOfDamages},
  meanOfDamages = Map[getSumOfSquaresDamage[#, {meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}]&, tiedTms];
  
  First[First[Position[meanOfDamages, Min[meanOfDamages]]]]
];


(* SHARED *)

tuningOptions = {
  "meanPower" -> \[Infinity],
  "weighted" -> False,
  "weightingDirection" -> "regressive",
  "complexityWeighting" -> "P",
  "complexityPower" -> 1,
  "tim" -> Null,
  "damage" -> "",
  "mean" -> "",
  "tuning" -> ""
};

processTuningOptions[t_, inputMeanPower_, inputWeighted_, inputWeightingDirection_, inputComplexityWeighting_, inputComplexityPower_, inputTim_, inputDamage_, inputTuning_, inputMean_, forDamage_ : False] := Module[
  {
    tima,
    damageParts,
    d,
    ptm,
    meanPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean
  },
  
  meanPower = inputMeanPower;
  weighted = inputWeighted;
  weightingDirection = inputWeightingDirection;
  complexityWeighting = inputComplexityWeighting;
  complexityPower = inputComplexityPower;
  tim = inputTim;
  damage = inputDamage;
  tuning = inputTuning;
  mean = inputMean;
  
  If[
    tuning === "Tenney",
    damage = "P1"; tim = {},
    If[
      tuning === "Breed",
      damage = "P2"; tim = {},
      If[
        tuning === "Partch",
        damage = "pP1",
        If[
          tuning === "Euclidean",
          damage = "F2"; tim = {},
          If[
            tuning === "least squares",
            meanPower = 2,
            If[
              tuning === "least absolutes",
              meanPower = 1,
              If[
                tuning === "Tenney least squares",
                meanPower = 2; damage = "P1"
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
    weightingDirection = "progressive";
    complexityWeighting = damageParts[[2]];
    complexityPower = ToExpression[damageParts[[3]]],
    If[
      Length[damageParts] === 2,
      weighted = True;
      weightingDirection = "regressive";
      complexityWeighting = damageParts[[1]];
      complexityPower = ToExpression[damageParts[[2]]];
    ]
  ];
  
  If[
    mean === "AAV",
    meanPower = 1,
    If[
      mean === "RMS",
      meanPower = 2,
      If[
        mean === "MAV",
        meanPower = \[Infinity]
      ]
    ]
  ];
  
  d = getD[t];
  ptm = getPtm[d];
  
  tima = If[tim === Null, getDiamond[d], If[Length[tim] == 0, If[forDamage, getA[getC[t]], {}], getA[tim]]];
  
  {meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}
];

getPtm[d_] := Log[2, getPrimes[d]];

getW[tima_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{w},
  w = If[
    weighted,
    Map[getComplexity[#, complexityWeighting, complexityPower]&, tima],
    Map[1&, tima]
  ];
  
  If[weightingDirection == "regressive", 1 / w, w]
];

getComplexity[pcv_, complexityWeighting_, complexityPower_] := Module[{weightedPcv},
  weightedPcv = If[complexityWeighting == "P", pcv * getPtm[Length[pcv]], pcv];
  Norm[weightedPcv, complexityPower]
];
