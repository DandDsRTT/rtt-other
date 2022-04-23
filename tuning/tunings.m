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
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 695.294}
*)
Options[optimizeGtm] = tuningOptions;
optimizeGtm[t_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    tuningOptions,
    optimizedGtm,
    tPossiblyWithChangedIntervalBasis
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, tim, tuningIntervalBasis, systematicTuningName, originalTuningName, pureOctaveStretch];
  optimizationPower = Part[tuningOptions, 1];
  tPossiblyWithChangedIntervalBasis = Part[tuningOptions, 4];
  pureOctaveStretch = Part[tuningOptions, 9];
  
  optimizedGtm = 1200 * If[
    optimizationPower == \[Infinity],
    optimizeGtmMinimax[tuningOptions],
    If[
      optimizationPower == 2,
      optimizeGtmMinisos[tuningOptions],
      optimizeGtmMinisum[tuningOptions]
    ]
  ];
  
  If[
    !isStandardPrimeLimitB[getB[t]] && tuningIntervalBasis == "primes",
    optimizedGtm = retrievePrimesIntervalBasisGtm[optimizedGtm, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    pureOctaveStretch,
    optimizedGtm = getPureOctaveStretchedGtm[optimizedGtm, t]
  ];
  
  optimizedGtm
];


(*
  optimizeTm[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimal tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTm[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 1899.26, 2790.25}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTm[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 1899.26, 2790.25} 
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTm[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 1893.54, 2781.18} 
*)
Options[optimizeTm] = tuningOptions;
optimizeTm[t_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  
  optimizeGtm[t, {
    "optimizationPower" -> optimizationPower,
    "damageWeightingSlope" -> damageWeightingSlope,
    "complexityUnitsMultiplier" -> complexityUnitsMultiplier,
    "complexityNormPower" -> complexityNormPower,
    "tim" -> tim,
    "tuningIntervalBasis" -> tuningIntervalBasis,
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "pureOctaveStretch" -> pureOctaveStretch
  }].getA[getM[t]]
];


(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGtmMinimax[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] := If[
  damageWeightingSlope == "simplicityWeighted" && Length[tima] == 0,
  optimizeGtmTargetingAll[d, t, ptm, complexityUnitsMultiplier, complexityNormPower],
  If[
    damageWeightingSlope == "unweighted",
    optimizeGtmMinimaxTargetingListAnalytical[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch],
    optimizeGtmTargetingListNumerical[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
  ]
];


(* TARGETING-ALL MINIMAX *)

optimizeGtmTargetingAll[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_] := If[
  complexityNormPower == 2,
  If[
    complexityUnitsMultiplier == "logOddLimit",
    (* covers KE *)
    optimizeGtmTargetingAllPseudoInverseAnalyticalKe[d, t, ptm, complexityUnitsMultiplier],
    (* covers TE, Frobenius, WE, BE *) (* TODO: I think it might actually be the right idea in this case to comment the heck out of this code, every place any tuning goes *)
    optimizeGtmTargetingAllPseudoInverseAnalytical[d, t, ptm, complexityUnitsMultiplier]
  ],
  (* covers TOP, L1 version of Frobenius, Weil, Kees, BOP *)
  optimizeGtmTargetingAllNumerical[d, t, ptm, complexityUnitsMultiplier, complexityNormPower]
];

optimizeGtmTargetingAllPseudoInverseAnalytical[d_, t_, ptm_, complexityUnitsMultiplier_] := Module[{unitsMultiplierMatrix, timaAsPrimesIdentityMatrix, damageWeightingSlope, complexityNormPower},
  timaAsPrimesIdentityMatrix = IdentityMatrix[d];
  complexityNormPower = 2; (* TODO: is it cleaner to just pass this in ? *)
  unitsMultiplierMatrix = getUnitsMultiplierMatrix[t, timaAsPrimesIdentityMatrix, complexityUnitsMultiplier, complexityNormPower];
  optimizeGtmWithPseudoInverse[timaAsPrimesIdentityMatrix, unitsMultiplierMatrix, t, ptm]
];

optimizeGtmTargetingAllPseudoInverseAnalyticalKe[d_, t_, ptm_, complexityUnitsMultiplier_] := Module[{},
  {1, 2, 3, 4, 5} (* TODO: I think this is where to implement this, following CTE style instructions *)
];

optimizeGtmTargetingAllNumerical[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{gtm, ma, tm},
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tm = gtm.ma;
  
  If[
    complexityUnitsMultiplier == "logIntegerLimit" || complexityUnitsMultiplier == "logOddLimit",
    (* covers Weil and Kees *)
    optimizeGtmTargetingAllNumericalCustomDualNorm[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm],
    (* covers TOP, BOP, and L1-version of Frobenius *)
    optimizeGtmTargetingAllNumericalSimpleDualNorm[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm]
  ]
];

optimizeGtmTargetingAllNumericalSimpleDualNorm[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := If[
  hasNonUniqueTuning[getM[t]],
  optimizeGtmTargetingAllNumericalSimpleDualNormNonUnique[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm],
  optimizeGtmTargetingAllNumericalSimpleDualNormUnique[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm]
];

optimizeGtmTargetingAllNumericalSimpleDualNormUnique[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[
  {eₚ, unitsMultiplierMatrix, timaAsPrimesIdentityMatrix, solution, optimizationPower},
  
  eₚ = tm - ptm;
  timaAsPrimesIdentityMatrix = IdentityMatrix[d];
  unitsMultiplierMatrix = getUnitsMultiplierMatrix[t, timaAsPrimesIdentityMatrix, complexityUnitsMultiplier, complexityNormPower];
  eₚ = unitsMultiplierMatrix.eₚ;
  optimizationPower = dualPower[complexityNormPower];
  
  solution = NMinimize[Norm[eₚ, optimizationPower], gtm, WorkingPrecision -> 128];
  
  gtm /. Last[solution] // N
];

(* TODO: might be able to DRY it up with optimizeGtmTargetingListNumerical, or at least correlate their implementations as much as possible to illuminate the patterns, as you did with getW and getUnitsMultiplierMatrix *)
optimizeGtmTargetingAllNumericalSimpleDualNormNonUnique[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[
  {
    eₚ,
    unitsMultiplierMatrix,
    timaAsPrimesIdentityMatrix,
    solution,
    previousSolution,
    optimizationPower,
    previousPrimesErrorMagnitude,
    primesErrorMagnitude,
    normPowerPower,
    normPower
  },
  
  eₚ = tm - ptm;
  timaAsPrimesIdentityMatrix = IdentityMatrix[d];
  unitsMultiplierMatrix = getUnitsMultiplierMatrix[t, timaAsPrimesIdentityMatrix, complexityUnitsMultiplier, complexityNormPower];
  eₚ = unitsMultiplierMatrix.eₚ;
  optimizationPower = dualPower[complexityNormPower];
  previousPrimesErrorMagnitude = \[Infinity];
  primesErrorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  
  While[
    normPowerPower <= 10 && previousPrimesErrorMagnitude - primesErrorMagnitude > 0,
    
    previousPrimesErrorMagnitude = primesErrorMagnitude;
    previousSolution = solution;
    solution = NMinimize[Norm[eₚ, normPower], gtm, WorkingPrecision -> 128];
    primesErrorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  gtm /. Last[previousSolution] // N
];

optimizeGtmTargetingAllNumericalCustomDualNorm[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[
  {augmentedThing, almostL1Norm, solution, middleMan, minimizeSetup},
  
  middleMan = tm / ptm - Table[1, d];
  augmentedThing = AppendTo[middleMan, 0];
  almostL1Norm = Max[augmentedThing] - Min[augmentedThing];
  minimizeSetup = If[complexityUnitsMultiplier == "logOddLimit", {almostL1Norm, augmentedThing[[1]] == 0}, almostL1Norm];
  solution = NMinimize[{almostL1Norm, augmentedThing[[1]] == 0}, gtm, WorkingPrecision -> 128];
  
  gtm /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];


(* TARGETING-LIST MINIMAX *)

optimizeGtmMinimaxTargetingListAnalytical[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch];


(* MINISOS *)

optimizeGtmMinisos[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] := Module[{W},
  W = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  optimizeGtmWithPseudoInverse[tima, W, t, ptm]
];



(* MINISUM *)

optimizeGtmMinisum[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch];


(* SOLVER (USED BY NUMERICAL MINIMAX AND MINISUM) *)

optimizeGtmTargetingListNumerical[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := If[
  hasNonUniqueTuning[getM[t]],
  optimizeGtmTargetingListNumericalUnique[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower],
  optimizeGtmTargetingListNumericalNonUnique[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
];

optimizeGtmTargetingListNumericalUnique[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[
  {gtm, ma, mappedTima, pureTimaSizes, W, solution, e},
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTimaSizes = Map[ptm.#&, tima];
  W = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  mappedTima = Transpose[ma.Transpose[tima]];
  e = Flatten[MapIndexed[
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
      ] * Part[Part[W, tiIndex, tiIndex]]
    ],
    mappedTima
  ]];
  
  solution = NMinimize[Norm[e, optimizationPower], gtm, WorkingPrecision -> 128];
  
  gtm /. Last[solution] // N
];

optimizeGtmTargetingListNumericalNonUnique[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[
  {
    gtm,
    ma,
    mappedTima,
    pureTimaSizes,
    W,
    solution,
    errorMagnitude,
    previousErrorMagnitude,
    normPower,
    normPowerPower,
    e,
    previousSolution
  },
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTimaSizes = Map[ptm.#&, tima];
  W = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  mappedTima = Transpose[ma.Transpose[tima]];
  
  previousErrorMagnitude = \[Infinity];
  errorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  e = Flatten[MapIndexed[
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
      ] * Part[Part[W, tiIndex, tiIndex]]
    ],
    mappedTima
  ]];
  
  While[
    normPowerPower <= 10 && previousErrorMagnitude - errorMagnitude > 0,
    
    previousErrorMagnitude = errorMagnitude;
    previousSolution = solution;
    solution = NMinimize[Norm[e, normPower], gtm, WorkingPrecision -> 128];
    errorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  gtm /. Last[previousSolution] // N
];


(* SIMPLEX (USED BY ANALYTICAL MINIMAX AND MINISUM) *)

optimizeGtmSimplex[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_] := Module[
  {
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialPs,
    potentialTms,
    powerSumOfDamages,
    minMeanIndices,
    minMeanIndex,
    minMeanP,
    gpt,
    projectedGenerators,
    damagePowerSum
  },
  
  damagePowerSum = If[optimizationPower == 1, getSumDamage, getMaxDamage];
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromUnchangedIntervals[t, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  potentialTms = Map[ptm.#&, potentialPs];
  powerSumOfDamages = Map[damagePowerSum[#, {optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch}]&, potentialTms];
  
  minMeanIndices = Position[powerSumOfDamages, Min[powerSumOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    
    minMeanIndex = First[First[Position[powerSumOfDamages, Min[powerSumOfDamages]]]];
    minMeanP = potentialPs[[minMeanIndex]];
    gpt = Transpose[getA[getGpt[t]]];
    projectedGenerators = minMeanP.gpt;
    ptm.projectedGenerators // N,
    
    optimizeGtmTargetingListNumerical[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
  ]
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
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    ma,
    tm,
    tuningOptions
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, tim, tuningIntervalBasis, systematicTuningName, originalTuningName, pureOctaveStretch, True];
  optimizationPower = Part[tuningOptions, 1];
  
  ma = getA[getM[t]];
  tm = (gtm / 1200).ma;
  
  If[
    optimizationPower == \[Infinity],
    getMaxDamage[tm, tuningOptions],
    If[
      optimizationPower == 2,
      get2SumDamage[tm, tuningOptions],
      getSumDamage[tm, tuningOptions]
    ]
  ]
];

getTid[t_, tm_, tima_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{e, W},
  e = N[tm.Transpose[tima]] - N[ptm.Transpose[tima]];
  W = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  Abs[e].W
];

Square[n_] := n^2;

getSumDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    Total[getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]];

get2SumDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    Total[Square[getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]]];

getSumDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    Max[getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGtm[optimizedGtm_, originalT_, t_] := Module[
  {ma, optimizedTm, gpt, f},
  
  ma = getA[getM[t]];
  optimizedTm = optimizedGtm.ma;
  gpt = Transpose[getA[getGpt[originalT]]];
  f = Transpose[getF[originalT]];
  
  optimizedTm.f.gpt
];


(* PURE-OCTAVE STRETCH *)

getPureOctaveStretchedGtm[optimizedGtm_, t_] := Module[{periodsPerOctave},
  periodsPerOctave = First[First[getA[getM[t]]]];
  
  (1200 / periodsPerOctave) * (optimizedGtm / First[optimizedGtm])
];



(* SHARED *)

tuningOptions = {
  "tim" -> Null, (* trait 0 *)
  "optimizationPower" -> Null, (* trait 1 *)
  "damageWeightingSlope" -> "", (* trait 2 *)
  "complexityNormPower" -> 1, (* trait 3 *)
  "complexityUnitsMultiplier" -> "logProduct", (* trait 4 *)
  "tuningIntervalBasis" -> "primes",
  "systematicTuningName" -> "",
  "originalTuningName" -> "",
  "pureOctaveStretch" -> False
};

processTuningOptions[
  t_,
  inputOptimizationPower_,
  inputDamageWeightingSlope_,
  inputComplexityUnitsMultiplier_,
  inputComplexityNormPower_,
  inputTim_,
  inputTuningIntervalBasis_,
  inputSystematicTuningName_,
  inputOriginalTuningName_,
  inputPureOctaveStretch_,
  forDamage_ : False
] := Module[
  {
    tima,
    d,
    ptm,
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    tPossiblyWithChangedIntervalBasis,
    b,
    ir
  },
  
  optimizationPower = inputOptimizationPower;
  damageWeightingSlope = inputDamageWeightingSlope;
  complexityUnitsMultiplier = inputComplexityUnitsMultiplier;
  complexityNormPower = inputComplexityNormPower;
  tim = inputTim;
  tuningIntervalBasis = inputTuningIntervalBasis;
  systematicTuningName = inputSystematicTuningName;
  originalTuningName = inputOriginalTuningName;
  pureOctaveStretch = inputPureOctaveStretch;
  
  If[
    originalTuningName === "minimax",
    optimizationPower = 1; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "TOP" || originalTuningName === "TIPTOP",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logProduct";
  ];
  If[
    originalTuningName === "TE" || originalTuningName === "Tenney-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logProduct";
  ];
  If[
    originalTuningName === "Frobenius",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "noop";
  ];
  If[
    originalTuningName === "BOP",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "product";
  ];
  If[
    originalTuningName === "BE" || originalTuningName === "Benedetti-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "product";
  ];
  If[
    originalTuningName === "Kees",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "KE" || originalTuningName === "Kees-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "Weil",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logIntegerLimit";
  ];
  If[
    originalTuningName === "WE" || originalTuningName === "Weil-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logIntegerLimit";
  ];
  If[
    originalTuningName === "POTOP" || originalTuningName === "POTT",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logProduct"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "POTE",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logProduct"; pureOctaveStretch = True;
  ];
  
  If[
    StringMatchQ[systematicTuningName, "*S*"],
    damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningName, "*C*"],
    damageWeightingSlope = "complexityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningName, "*U*"],
    damageWeightingSlope = "unweighted";
  ];
  If[
    StringMatchQ[systematicTuningName, "*E*"],
    complexityNormPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningName, "*T*"],
    complexityNormPower = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*M*"],
    complexityUnitsMultiplier = "noop";
  ];
  If[
    StringMatchQ[systematicTuningName, "*O*"],
    complexityUnitsMultiplier = "logProduct";
  ];
  If[
    StringMatchQ[systematicTuningName, "*minimax*"],
    optimizationPower = \[Infinity];
  ];
  If[
    StringMatchQ[systematicTuningName, "*minisos*"],
    optimizationPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningName, "*minisum*"],
    optimizationPower = 1;
  ];
  
  If[!NumericQ[optimizationPower] && optimizationPower != \[Infinity], Throw["no optimization power"]];
  If[damageWeightingSlope == "", Throw["no damage weighting slope"]];
  
  b = getB[t];
  If[
    !isStandardPrimeLimitB[b] && tuningIntervalBasis == "primes",
    
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getDp[b]];
    commaBasisInPrimeLimitIntervalBasis = changeB[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    ir = getIrForC[b, primeLimitIntervalBasis];
    tima = If[tim === Null, getDiamond[d], If[Length[tim] == 0, If[forDamage, ir.getA[getC[t]], {}], ir.getA[tim]]];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    d = getD[tPossiblyWithChangedIntervalBasis];
    ptm = getPtm[tPossiblyWithChangedIntervalBasis],
    
    tPossiblyWithChangedIntervalBasis = t;
    d = getD[tPossiblyWithChangedIntervalBasis];
    ptm = getPtm[tPossiblyWithChangedIntervalBasis];
    tima = If[tim === Null, getDiamond[d], If[Length[tim] == 0, If[forDamage, getA[getC[t]], {}], getA[tim]]];
  ];
  
  {optimizationPower, tima, d, tPossiblyWithChangedIntervalBasis, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch}
];

getPtm[t_] := Log[2, getB[t]];

getW[t_, tima_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{W},
  W = If[
    damageWeightingSlope != "unweighted",
    DiagonalMatrix[Map[getComplexity[#, t, complexityUnitsMultiplier, complexityNormPower]&, tima]],
    IdentityMatrix[Length[tima]]
  ];
  
  If[damageWeightingSlope == "simplicityWeighted", PseudoInverse[W], W]
];

getUnitsMultiplierMatrix[t_, timaAsPrimesIdentityMatrix_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{unitsMultiplierMatrix},
  unitsMultiplierMatrix = getThing[t, complexityUnitsMultiplier]; (* TODO dont need args *)
  
  PseudoInverse[unitsMultiplierMatrix] (* getW may be simplicity-weighted or complexity-weighted, but this is always essentially "simplicity-weighted" *)
];

(* TODO: verify that these weighting matrices result in the same formulae as appear in the "vector i form" of my spreadsheet: https://docs.google.com/spreadsheets/d/1BBcUCoe6seCC1PM2qaByyiMNNLdxkEsx5X_-XJ9BdpE/edit#gid=694229653 *)
getComplexity[pcv_, t_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{d, W},
  d = getD[t];
  W = getThing[t, complexityUnitsMultiplier];
  Norm[W.pcv, complexityNormPower]
];
getThing[t_, complexityUnitsMultiplier_] := If[ (* note this is a different W than the one in getW, it is nested, this is to weight the quantities of the PC-vectors before taking a norm and getting an interval complexity *)
  (* when used by getUnitsMultiplierMatrix for optimizeGtmTargetingAllNumericalSimpleDualNorm, covers TOP; when used by getUnitsMultiplierMatrix for optimizeGtmTargetingAllPseudoInverseAnalytical, covers TE; when used by getW by optimizeGtmMinisos, optimizeGtmTargetingListNumerical, or by getTid by getSumDamage or getMaxDamage by optimizeGtmSimplex, covers any targeting-list tuning using this as its damage's complexity *)
  complexityUnitsMultiplier == "logProduct", \[AliasDelimiter]\[AliasDelimiter]
      DiagonalMatrix[Map[getLogProductComplexity[#, t]&, IdentityMatrix[getD[t]]]], (* TODO: clean up d, t etc*)
  
  If[
    (* when used by getUnitsMultiplierMatrix for optimizeGtmTargetingAllNumericalSimpleDualNorm, covers BOP; when used by getUnitsMultiplierMatrix for optimizeGtmTargetingAllPseudoInverseAnalytical, covers BE *)
    complexityUnitsMultiplier == "product",
    DiagonalMatrix[Map[getProductComplexity[#, t]&, IdentityMatrix[getD[t]]]],
    
    If[
      (* also covers TOP, TE, etc. (equivalent to "logProduct") *)
      complexityUnitsMultiplier == "logSopfr",
      DiagonalMatrix[Map[getLogSopfrComplexity[#, t]&, IdentityMatrix[getD[t]]]],
      
      If[
        (* also covers BOP, BE, etc. (equivalent to "product") *)
        complexityUnitsMultiplier == "sopfr",
        DiagonalMatrix[Map[getSopfrComplexity[#, t]&, IdentityMatrix[getD[t]]]],
        
        If[
          (* when Weil needs its dual norm, we actually go into optimizeGtmTargetingAllNumericalCustomDualNorm, where it's implemented separately (the min - max thing); when used by getUnitsMultiplierMatrix for optimizeGtmTargetingAllPseudoInverseAnalytical, covers WE; when used by getW by optimizeGtmMinisos, optimizeGtmTargetingListNumerical, or by getTid by getSumDamage or getMaxDamage by optimizeGtmSimplex, should cover any targeting-list tuning using this as its damage's complexity *)
          complexityUnitsMultiplier == "logIntegerLimit",
          Join[DiagonalMatrix[Map[getLogProductComplexity[#, t]&, IdentityMatrix[getD[t]]]], {Map[getLogProductComplexity[#, meantone]&, IdentityMatrix[getD[t]]]}] / 2,
          
          If[
            (* when Kees needs its dual norm, we actually go into optimizeGtmTargetingAllNumericalCustomDualNorm, where it's implemented separately (the min - max thing) with pure-octave constraint on the solver; ### still need to cover KE; when used by getW by optimizeGtmMinisos, optimizeGtmTargetingListNumerical, or by getTid by getSumDamage or getMaxDamage by optimizeGtmSimplex, should cover any targeting-list tuning using this as its damage's complexity *)
            complexityUnitsMultiplier == "logOddLimit",
            Join[DiagonalMatrix[Map[getLogProductComplexity[#, t]&, DiagonalMatrix[Join[{0}, Table[1, getD[t] - 1]]]]], {Map[getLogProductComplexity[#, meantone]&, DiagonalMatrix[Join[{0}, Table[1, getD[t] - 1]]]]}] / 2, (* TODO: DRY up this DiagonalMatrix[Join[{0}, Table[1, d - 1]]] and IdentityMatrix[d] thing... the former is like a "no-twos primes" or soemthing *)
            
            (* when used by getUnitsMultiplierMatrix for optimizeGtmTargetingAllNumericalSimpleDualNorm, covers L1 version of Frobenius; when used by getUnitsMultiplierMatrix for optimizeGtmTargetingAllPseudoInverseAnalytical, covers Frobenius *)
            DiagonalMatrix[Map[1&, IdentityMatrix[getD[t]]]] (* TODO: should be copfrComplexity? *)
          ]
        ]
      ]
    ]
  ]
];
(* TODO: I feel like I might be being a bit wasteful / redundant / overwrought in how I've implemented this ... *)
getCopfrComplexity[pcv_, t_] := Total[Map[If[Abs[# > 0], 1, 0]&, pcv]];
(* AKA "Benedetti height" *)
getProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getB[t], pcv}];
(* AKA "Tenney height" *)
getLogProductComplexity[pcv_, t_] := Log[2, getProductComplexity[pcv, t]];
(* AKA "Wilson height", can also be used to find BOP tuning *)
getSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getB[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find TOP tuning *)
getLogSopfrComplexity[pcv_, t_] := Log[2, getSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getIntegerLimitComplexity[pcv_, t_] := Module[{rational},
  rational = pcvToRational[pcv];
  Max[Numerator[rational], Denominator[rational]]
];
(* AKA "logarithmic Weil height", used for "Weil tuning" *)
getLogIntegerLimitComplexity[pcv_, t_] := Log[2, getIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
noTwos[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getOddLimitComplexity[pcv_, t_] := getIntegerLimitComplexity[noTwos[pcv], t];
(* AKA "Kees expressibility" , used for "Kees tuning" *)
getLogOddLimitComplexity[pcv_, t_] := Log[2, getOddLimitComplexity[pcv, t]];

optimizeGtmWithPseudoInverse[timaOrPrimesIdentityMatrix_, WorUnitsMultiplierMatrix_, t_, ptm_] := Module[{ma, weightedTimaMapped, g, gtm},
  ma = getA[getM[t]];
  weightedTimaMapped = ma.Transpose[timaOrPrimesIdentityMatrix].WorUnitsMultiplierMatrix;
  (* TODO: if we're constantly transposing tima, maybe just do it once up front? or have getA respect the co/contra? *)
  g = Transpose[timaOrPrimesIdentityMatrix].WorUnitsMultiplierMatrix.Transpose[weightedTimaMapped].Inverse[weightedTimaMapped.Transpose[weightedTimaMapped]];
  gtm = ptm.g;
  gtm // N
];


(* non-unique tuning *)

hasNonUniqueTuning[m_] := getR[m] > 1 && (hasIndependentGenerator[m] || primesInLockedRatio[m]);

countNonzeroElements[l_] := Count[l, element_ /; element != 0];
whichGeneratorIsTheSingleOneApproximatingThisPrime[generatorsApproximatingPrime_] := First[Position[generatorsApproximatingPrime, x_ /; x > 0, 1, 1]];

primesInLockedRatio[m_] := Module[
  {
    canonicalM,
    generatorsApproximatingEachPrime,
    countGeneratorsInvolvedInApproximatingEachPrime,
    whetherPrimesAreApproximatedBySingleGeneratorOrNot,
    indexesOfPrimesApproximatedBySingleGenerators,
    perGeneratorHowManyPrimesAreApproximatedOnlyByIt,
    index,
    hmmm, (* TODO: clean up *)
    whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator
  },
  canonicalM = canonicalForm[m];
  
  generatorsApproximatingEachPrime = Transpose[getA[canonicalM]];
  countGeneratorsInvolvedInApproximatingEachPrime = Map[countNonzeroElements, generatorsApproximatingEachPrime];
  whetherPrimesAreApproximatedBySingleGeneratorOrNot = Map[# == 1&, countGeneratorsInvolvedInApproximatingEachPrime];
  indexesOfPrimesApproximatedBySingleGenerators = {};
  MapIndexed[If[#1 == True, AppendTo[indexesOfPrimesApproximatedBySingleGenerators, #2] ]&, whetherPrimesAreApproximatedBySingleGeneratorOrNot];
  
  perGeneratorHowManyPrimesAreApproximatedOnlyByIt = Association[];
  Map[
    Function[{indexOfPrimeApproximatedBySingleGenerator},
      hmmm = First[Part[generatorsApproximatingEachPrime, indexOfPrimeApproximatedBySingleGenerator]];
      index = whichGeneratorIsTheSingleOneApproximatingThisPrime[hmmm];
      If[
        KeyExistsQ[perGeneratorHowManyPrimesAreApproximatedOnlyByIt, index],
        perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] = perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] + 1,
        perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] = 1
      ];
    ],
    indexesOfPrimesApproximatedBySingleGenerators
  ];
  whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator = Map[# > 1&, Values[perGeneratorHowManyPrimesAreApproximatedOnlyByIt]];
  AnyTrue[whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator, TrueQ]
];

hasIndependentGenerator[m_] := Module[{},
  canonicalM = canonicalForm[m];
  
  AnyTrue[getA[canonicalM], TrueQ[Total[Abs[#]] == 1]&]
];
