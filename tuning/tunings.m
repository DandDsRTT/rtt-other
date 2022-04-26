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
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 695.294}
*)
Options[optimizeGeneratorsTuningMap] = tuningOptions;
optimizeGeneratorsTuningMap[t_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    targetedIntervals,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    unchangedIntervals,
    tuningOptions,
    optimizedGeneratorsTuningMap,
    tPossiblyWithChangedIntervalBasis
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  targetedIntervals = OptionValue["targetedIntervals"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  unchangedIntervals = OptionValue["unchangedIntervals"];
  
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, targetedIntervals, tuningIntervalBasis, systematicTuningName, originalTuningName, pureOctaveStretch, unchangedIntervals, False];
  
  optimizationPower = Part[tuningOptions, 1];
  tPossiblyWithChangedIntervalBasis = Part[tuningOptions, 4];
  pureOctaveStretch = Part[tuningOptions, 9];
  
  optimizedGeneratorsTuningMap = 1200 * If[
    optimizationPower == \[Infinity],
    optimizeGeneratorsTuningMapMinimax[tuningOptions],
    If[
      optimizationPower == 2,
      optimizeGeneratorsTuningMapMinisos[tuningOptions],
      optimizeGeneratorsTuningMapMinisum[tuningOptions]
    ]
  ];
  
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningIntervalBasis == "primes",
    optimizedGeneratorsTuningMap = retrievePrimesIntervalBasisGeneratorsTuningMap[optimizedGeneratorsTuningMap, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    pureOctaveStretch,
    optimizedGeneratorsTuningMap = getPureOctaveStretchedGeneratorsTuningMap[optimizedGeneratorsTuningMap, t]
  ];
  
  optimizedGeneratorsTuningMap
];


(*
  optimizeTuningMap[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimal tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 1899.26, 2790.25}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 1899.26, 2790.25} 
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 1893.54, 2781.18} 
*)
Options[optimizeTuningMap] = tuningOptions; (* TODO: oh what if we just moved this below the tuningOptions def instead ? *)
optimizeTuningMap[t_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    targetedIntervals,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    unchangedIntervals
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  targetedIntervals = OptionValue["targetedIntervals"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  unchangedIntervals = OptionValue["unchangedIntervals"];
  
  optimizeGeneratorsTuningMap[t, {
    "optimizationPower" -> optimizationPower,
    "damageWeightingSlope" -> damageWeightingSlope,
    "complexityUnitsMultiplier" -> complexityUnitsMultiplier,
    "complexityNormPower" -> complexityNormPower,
    "targetedIntervals" -> targetedIntervals,
    "tuningIntervalBasis" -> tuningIntervalBasis,
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "pureOctaveStretch" -> pureOctaveStretch,
    "unchangedIntervals" -> unchangedIntervals
  }].getA[getM[t]]
];


(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGeneratorsTuningMapMinimax[{optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_}] := If[
  damageWeightingSlope == "simplicityWeighted" && Length[targetedIntervalsA] == 0,
  optimizeGeneratorsTuningMapTargetingAll[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower, unchangedIntervals],
  
  If[
    damageWeightingSlope == "unweighted",
    optimizeGeneratorsTuningMapMinimaxTargetingListAnalytical[optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch, unchangedIntervals],
    optimizeGeneratorsTuningMapTargetingListNumerical[optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
  ]
];


(* TARGETING-ALL MINIMAX *)

optimizeGeneratorsTuningMapTargetingAll[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_, unchangedIntervals_] := If[
  complexityNormPower == 2,
  If[
    Length[unchangedIntervals] > 0,
    
    (* covers CTE *)
    optimizeGeneratorsTuningMapCte[d, t, primesTuningMap, complexityUnitsMultiplier, unchangedIntervals, complexityNormPower],
    
    If[
      complexityUnitsMultiplier == "logOddLimit",
      
      (* covers KE *)
      optimizeGeneratorsTuningMapKe[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower],
      
      (* covers TE, Frobenius, WE, BE *)
      optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower]
    ]
  ],
  
  (* covers TOP, L1 version of Frobenius, Weil, Kees, BOP *)
  optimizeGeneratorsTuningMapTargetingAllNumerical[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower]
];

optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{unitsCounterMultiplierA, targetedIntervalsAsPrimesIdentityA},
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  unitsCounterMultiplierA = getUnitsCounterMultiplierA[t, targetedIntervalsAsPrimesIdentityA, complexityUnitsMultiplier, complexityNormPower];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[targetedIntervalsAsPrimesIdentityA, unitsCounterMultiplierA, t, primesTuningMap]
];

optimizeGeneratorsTuningMapKe[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_] := Module[ (* TODO: wait this can't be right... it's not actually doing anything L2-ish, right? *)
  {generatorsTuningMap, ma, tuningMap, solution, augmentedThing, almostL1Norm, middleMan, minimizeSetup},
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tuningMap = generatorsTuningMap.ma;
  
  middleMan = tuningMap / primesTuningMap - Table[1, d];
  augmentedThing = AppendTo[middleMan, 0]; (* maybe this 0 is the "junk" *)
  almostL1Norm = Max[augmentedThing] - Min[augmentedThing];
  minimizeSetup = {almostL1Norm, augmentedThing[[1]] == 0};
  solution = NMinimize[minimizeSetup, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

optimizeGeneratorsTuningMapTargetingAllNumerical[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{generatorsTuningMap, ma, tuningMap},
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tuningMap = generatorsTuningMap.ma;
  
  If[
    complexityUnitsMultiplier == "logIntegerLimit" || complexityUnitsMultiplier == "logOddLimit",
    
    (* covers Weil and Kees *)
    optimizeGeneratorsTuningMapTargetingAllNumericalCustomDualNorm[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower, generatorsTuningMap, tuningMap],
    
    (* covers TOP, BOP, and L1-version of Frobenius *)
    optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNorm[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower, generatorsTuningMap, tuningMap]
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNorm[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_, generatorsTuningMap_, tuningMap_] := If[
  hasNonUniqueTuning[getM[t]],
  optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNormNonUnique[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower, generatorsTuningMap, tuningMap],
  optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNormUnique[d, t, primesTuningMap, complexityUnitsMultiplier, complexityNormPower, generatorsTuningMap, tuningMap]
];

optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNormUnique[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_, generatorsTuningMap_, tuningMap_] := Module[
  {primesErrorMap, unitsCounterMultiplierA, targetedIntervalsAsPrimesIdentityA, solution, optimizationPower},
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  unitsCounterMultiplierA = getUnitsCounterMultiplierA[t, targetedIntervalsAsPrimesIdentityA, complexityUnitsMultiplier, complexityNormPower];
  
  optimizationPower = dualPower[complexityNormPower];
  
  solution = NMinimize[Norm[unitsCounterMultiplierA.primesErrorMap, optimizationPower], generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

(* TODO: might be able to DRY it up with optimizeGeneratorsTuningMapTargetingListNumerical, or at least correlate their implementations as much as possible to illuminate the patterns, as you did with getDamagesWeightingA and getUnitsCounterMultiplierA *)
optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNormNonUnique[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_, generatorsTuningMap_, tuningMap_] := Module[
  {
    primesErrorMap,
    unitsCounterMultiplierA,
    targetedIntervalsAsPrimesIdentityA,
    solution,
    previousSolution,
    optimizationPower,
    previousPrimesErrorMagnitude,
    primesErrorMagnitude,
    normPowerPower,
    normPower
  },
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  unitsCounterMultiplierA = getUnitsCounterMultiplierA[t, targetedIntervalsAsPrimesIdentityA, complexityUnitsMultiplier, complexityNormPower];
  primesErrorMap = unitsCounterMultiplierA.primesErrorMap;
  optimizationPower = dualPower[complexityNormPower];
  previousPrimesErrorMagnitude = \[Infinity];
  primesErrorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  
  While[
    normPowerPower <= 10 && previousPrimesErrorMagnitude - primesErrorMagnitude > 0,
    
    previousPrimesErrorMagnitude = primesErrorMagnitude;
    previousSolution = solution;
    solution = NMinimize[Norm[primesErrorMap, normPower], generatorsTuningMap, WorkingPrecision -> 128];
    primesErrorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  generatorsTuningMap /. Last[previousSolution] // N
];

optimizeGeneratorsTuningMapTargetingAllNumericalCustomDualNorm[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, complexityNormPower_, generatorsTuningMap_, tuningMap_] := Module[
  {augmentedThing, almostL1Norm, solution, middleMan, minimizeSetup},
  
  middleMan = tuningMap / primesTuningMap - Table[1, d];
  augmentedThing = AppendTo[middleMan, 0];
  almostL1Norm = Max[augmentedThing] - Min[augmentedThing];
  minimizeSetup = If[complexityUnitsMultiplier == "logOddLimit", {almostL1Norm, augmentedThing[[1]] == 0}, almostL1Norm];
  solution = NMinimize[minimizeSetup, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];


(* TARGETING-LIST MINIMAX *)

optimizeGeneratorsTuningMapMinimaxTargetingListAnalytical[optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_] :=
    optimizeGeneratorsTuningMapSimplex[optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch, unchangedIntervals];


(* MINISOS *)

optimizeGeneratorsTuningMapMinisos[{optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_}] := Module[{damagesWeightingA},
  damagesWeightingA = getDamagesWeightingA[t, targetedIntervalsA, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[targetedIntervalsA, damagesWeightingA, t, primesTuningMap]
];



(* MINISUM *)

optimizeGeneratorsTuningMapMinisum[{optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_}] :=
    optimizeGeneratorsTuningMapSimplex[optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch, unchangedIntervals];


(* SOLVER (USED BY NUMERICAL MINIMAX AND MINISUM) *)

optimizeGeneratorsTuningMapTargetingListNumerical[optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := If[
  hasNonUniqueTuning[getM[t]],
  optimizeGeneratorsTuningMapTargetingListNumericalUnique[optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower],
  optimizeGeneratorsTuningMapTargetingListNumericalNonUnique[optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
];

optimizeGeneratorsTuningMapTargetingListNumericalUnique[optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[
  {generatorsTuningMap, ma, mappedTargetedIntervalsA, pureTargetedIntervalsASizes, damagesWeightingA, solution, targetedIntervalErrorsL},
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTargetedIntervalsASizes = Map[primesTuningMap.#&, targetedIntervalsA];
  damagesWeightingA = getDamagesWeightingA[t, targetedIntervalsA, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  mappedTargetedIntervalsA = Transpose[ma.Transpose[targetedIntervalsA]];
  targetedIntervalErrorsL = Flatten[MapIndexed[
    Function[
      {mappedTargetedInterval, targetedIntervalIndex},
      Abs[
        Total[
          MapThread[
            Function[
              {mappedTargetedIntervalEntry, gtmEntry},
              mappedTargetedIntervalEntry * gtmEntry
            ],
            {mappedTargetedInterval, generatorsTuningMap}
          ]
        ] - pureTargetedIntervalsASizes[[targetedIntervalIndex]]
      ] * Part[Part[damagesWeightingA, targetedIntervalIndex, targetedIntervalIndex]]
    ],
    mappedTargetedIntervalsA
  ]];
  
  solution = NMinimize[Norm[targetedIntervalErrorsL, optimizationPower], generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

optimizeGeneratorsTuningMapTargetingListNumericalNonUnique[optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[
  {
    generatorsTuningMap,
    ma,
    mappedTargetedIntervalsA,
    pureTargetedIntervalsASizes,
    damagesWeightingA,
    solution,
    errorMagnitude,
    previousErrorMagnitude,
    normPower,
    normPowerPower,
    targetedIntervalErrorsL,
    previousSolution
  },
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTargetedIntervalsASizes = Map[primesTuningMap.#&, targetedIntervalsA];
  damagesWeightingA = getDamagesWeightingA[t, targetedIntervalsA, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  mappedTargetedIntervalsA = Transpose[ma.Transpose[targetedIntervalsA]];
  
  previousErrorMagnitude = \[Infinity];
  errorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  targetedIntervalErrorsL = Flatten[MapIndexed[
    Function[
      {mappedTargetedInterval, targetedIntervalIndex},
      Abs[
        Total[
          MapThread[
            Function[
              {mappedTargetedIntervalEntry, gtmEntry},
              mappedTargetedIntervalEntry * gtmEntry
            ],
            {mappedTargetedInterval, generatorsTuningMap}
          ]
        ] - pureTargetedIntervalsASizes[[targetedIntervalIndex]]
      ] * Part[Part[damagesWeightingA, targetedIntervalIndex, targetedIntervalIndex]]
    ],
    mappedTargetedIntervalsA
  ]];
  
  While[
    normPowerPower <= 10 && previousErrorMagnitude - errorMagnitude > 0,
    
    previousErrorMagnitude = errorMagnitude;
    previousSolution = solution;
    solution = NMinimize[Norm[targetedIntervalErrorsL, normPower], generatorsTuningMap, WorkingPrecision -> 128];
    errorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  generatorsTuningMap /. Last[previousSolution] // N
];


(* SIMPLEX (USED BY ANALYTICAL MINIMAX AND MINISUM) *)

optimizeGeneratorsTuningMapSimplex[optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_] := Module[
  {
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialProjectionAs,
    potentialTuningMaps,
    powerSumOfDamages,
    minMeanIndices,
    minMeanIndex,
    minMeanP,
    generatorsPreimageTransversal,
    projectedGenerators,
    damagePowerSum
  },
  
  damagePowerSum = If[optimizationPower == 1, getSumDamage, getMaxDamage];
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[targetedIntervalsA]], {r}];
  potentialUnchangedIntervalSets = Map[Map[targetedIntervalsA[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialProjectionAs = Select[Map[getProjectionAFromUnchangedIntervals[t, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  potentialTuningMaps = Map[primesTuningMap.#&, potentialProjectionAs];
  powerSumOfDamages = Map[damagePowerSum[#, {optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch, unchangedIntervals}]&, potentialTuningMaps];
  
  minMeanIndices = Position[powerSumOfDamages, Min[powerSumOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    
    minMeanIndex = First[First[Position[powerSumOfDamages, Min[powerSumOfDamages]]]];
    minMeanP = potentialProjectionAs[[minMeanIndex]];
    generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[t]]];
    projectedGenerators = minMeanP.generatorsPreimageTransversal;
    primesTuningMap.projectedGenerators // N,
    
    optimizeGeneratorsTuningMapTargetingListNumerical[optimizationPower, targetedIntervalsA, d, t, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
  ]
];

getProjectionAFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[{commaEigenvectors, eigenvectors, diagonalEigenvalueA},
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueA = getDiagonalEigenvalueA[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueA.Inverse[eigenvectors]]
];

getDiagonalEigenvalueA[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
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

Options[getDamage] = tuningOptions;
getDamage[t_, generatorsTuningMap_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    targetedIntervals,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    unchangedIntervals,
    ma,
    tuningMap,
    tuningOptions
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  targetedIntervals = OptionValue["targetedIntervals"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  unchangedIntervals = OptionValue["unchangedIntervals"];
  
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, targetedIntervals, tuningIntervalBasis, systematicTuningName, originalTuningName, pureOctaveStretch, unchangedIntervals, True];
  optimizationPower = Part[tuningOptions, 1];
  
  ma = getA[getM[t]];
  tuningMap = (generatorsTuningMap / 1200).ma;
  
  If[
    optimizationPower == \[Infinity],
    getMaxDamage[tuningMap, tuningOptions],
    If[
      optimizationPower == 2,
      get2SumDamage[tuningMap, tuningOptions],
      getSumDamage[tuningMap, tuningOptions]
    ]
  ]
];

getTid[t_, tuningMap_, targetedIntervalsA_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[
  {targetedIntervalErrorsList, targetedIntervalDamagesList, damageWeightingA},
  
  targetedIntervalErrorsList = N[tuningMap.Transpose[targetedIntervalsA]] - N[primesTuningMap.Transpose[targetedIntervalsA]];
  targetedIntervalDamagesList = Abs[targetedIntervalErrorsList];
  damageWeightingA = getDamagesWeightingA[t, targetedIntervalsA, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  targetedIntervalDamagesList.damageWeightingA
];

Square[n_] := n^2;

getSumDamage[tuningMap_, {optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_}] :=
    Total[getTid[t, tuningMap, targetedIntervalsA, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]];

get2SumDamage[tuningMap_, {optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_}] :=
    Total[Square[getTid[t, tuningMap, targetedIntervalsA, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]]];

getMaxDamage[tuningMap_, {optimizationPower_, targetedIntervalsA_, d_, t_, primesTuningMap_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_, unchangedIntervals_}] :=
    Max[getTid[t, tuningMap, targetedIntervalsA, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGeneratorsTuningMap[optimizedGeneratorsTuningMap_, originalT_, t_] := Module[
  {ma, optimizedTm, generatorsPreimageTransversal, f},
  
  ma = getA[getM[t]];
  optimizedTm = optimizedGeneratorsTuningMap.ma;
  generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[originalT]]];
  f = Transpose[getFormalPrimesA[originalT]];
  
  optimizedTm.f.generatorsPreimageTransversal
];


(* PURE-OCTAVE STRETCH *)

getPureOctaveStretchedGeneratorsTuningMap[optimizedGeneratorsTuningMap_, t_] := Module[{periodsPerOctave},
  periodsPerOctave = First[First[getA[getM[t]]]];
  
  (1200 / periodsPerOctave) * (optimizedGeneratorsTuningMap / First[optimizedGeneratorsTuningMap])
];



(* SHARED *)

tuningOptions = {
  "targetedIntervals" -> Null, (* trait 0 *)
  "optimizationPower" -> Null, (* trait 1 *)
  "damageWeightingSlope" -> "", (* trait 2 *)
  "complexityNormPower" -> 1, (* trait 3 *)
  "complexityUnitsMultiplier" -> "logProduct", (* trait 4 *)
  "tuningIntervalBasis" -> "primes",
  "systematicTuningName" -> "",
  "originalTuningName" -> "",
  "pureOctaveStretch" -> False,
  "unchangedIntervals" -> {}
};

processTuningOptions[
  t_,
  inputOptimizationPower_,
  inputDamageWeightingSlope_,
  inputComplexityUnitsMultiplier_,
  inputComplexityNormPower_,
  inputTargetedIntervals_,
  inputTuningIntervalBasis_,
  inputSystematicTuningName_,
  inputOriginalTuningName_,
  inputPureOctaveStretch_,
  inputUnchangedIntervals_,
  forDamage_ : False
] := Module[
  {
    targetedIntervalsA,
    d,
    primesTuningMap,
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    targetedIntervals,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    unchangedIntervals,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    tPossiblyWithChangedIntervalBasis,
    intervalBasis,
    intervalRebase
  },
  
  optimizationPower = inputOptimizationPower;
  damageWeightingSlope = inputDamageWeightingSlope;
  complexityUnitsMultiplier = inputComplexityUnitsMultiplier;
  complexityNormPower = inputComplexityNormPower;
  targetedIntervals = inputTargetedIntervals;
  tuningIntervalBasis = inputTuningIntervalBasis;
  systematicTuningName = inputSystematicTuningName;
  originalTuningName = inputOriginalTuningName;
  pureOctaveStretch = inputPureOctaveStretch;
  unchangedIntervals = inputUnchangedIntervals;
  
  If[
    originalTuningName === "minimax",
    optimizationPower = \[Infinity]; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "TOP" || originalTuningName === "TIPTOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logProduct";
  ];
  If[
    originalTuningName === "TE" || originalTuningName === "Tenney-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logProduct";
  ];
  If[
    originalTuningName === "Frobenius",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "noop";
  ];
  If[
    originalTuningName === "BOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "product";
  ];
  If[
    originalTuningName === "BE" || originalTuningName === "Benedetti-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "product";
  ];
  If[
    originalTuningName === "Kees",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "KE" || originalTuningName === "Kees-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "Weil",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logIntegerLimit";
  ];
  If[
    originalTuningName === "WE" || originalTuningName === "Weil-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logIntegerLimit";
  ];
  If[
    originalTuningName === "POTOP" || originalTuningName === "POTT",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logProduct"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "POTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logProduct"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "CTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logProduct"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
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
  
  (* note: this has to go below the systematic tuning name gating, so that targetedIntervals has a change to be set to {} *)
  intervalBasis = getIntervalBasis[t];
  If[
    !isStandardPrimeLimitIntervalBasis[intervalBasis] && tuningIntervalBasis == "primes",
    
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getIntervalBasisDimension[intervalBasis]];
    commaBasisInPrimeLimitIntervalBasis = changeIntervalBasis[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    intervalRebase = getIntervalRebaseForC[intervalBasis, primeLimitIntervalBasis];
    targetedIntervalsA = If[targetedIntervals === Null, getDiamond[d], If[Length[targetedIntervals] == 0, If[forDamage, intervalRebase.getA[getC[t]], {}], intervalRebase.getA[targetedIntervals]]];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    d = getD[tPossiblyWithChangedIntervalBasis];
    primesTuningMap = getPrimesTuningMap[tPossiblyWithChangedIntervalBasis],
    
    tPossiblyWithChangedIntervalBasis = t;
    d = getD[tPossiblyWithChangedIntervalBasis];
    primesTuningMap = getPrimesTuningMap[tPossiblyWithChangedIntervalBasis];
    targetedIntervalsA = If[targetedIntervals === Null, getDiamond[d], If[Length[targetedIntervals] == 0, If[forDamage, getA[getC[t]], {}], getA[targetedIntervals]]];
  ];
  
  If[!NumericQ[optimizationPower] && optimizationPower != \[Infinity], Throw["no optimization power"]];
  If[damageWeightingSlope == "", Throw["no damage weighting slope"]];
  
  {optimizationPower, targetedIntervalsA, d, tPossiblyWithChangedIntervalBasis, primesTuningMap, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch, unchangedIntervals}
];

getPrimesTuningMap[t_] := Log[2, getIntervalBasis[t]];

getDamagesWeightingA[t_, targetedIntervalsA_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{damagesWeightingA},
  damagesWeightingA = If[
    damageWeightingSlope != "unweighted",
    DiagonalMatrix[Map[getComplexity[#, t, complexityUnitsMultiplier, complexityNormPower]&, targetedIntervalsA]],
    IdentityMatrix[Length[targetedIntervalsA]]
  ];
  
  If[damageWeightingSlope == "simplicityWeighted", PseudoInverse[damagesWeightingA], damagesWeightingA]
];

(* getDamagesWeightingA may be simplicity-weighted or complexity-weighted, but this is always essentially "simplicity-weighted" *)
getUnitsCounterMultiplierA[t_, targetedIntervalsAsPrimesIdentityA_, complexityUnitsMultiplier_, complexityNormPower_] :=
    PseudoInverse[getUnitsMultiplierA[t, complexityUnitsMultiplier]];

getComplexity[pcv_, t_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{unitsMultiplierA},
  (* TODO: I'm afraid this might be computing over and over... might be a good way to save some computation *)
  unitsMultiplierA = getUnitsMultiplierA[t, complexityUnitsMultiplier];
  
  Norm[unitsMultiplierA.pcv, complexityNormPower]
];

(* TODO: verify that these  matrices result in the same formulae as appear in the "vector i form" of my spreadsheet: 
https://docs.google.com/spreadsheets/d/1BBcUCoe6seCC1PM2qaByyiMNNLdxkEsx5X_-XJ9BdpE/edit#gid=694229653 *)
(* note this is different than the damagesWeightingA gotten by getDamagesWeightingA, this is nested within it;
this is to weight the quantities of the PC-vectors before taking a norm and getting an interval complexity, 
which are then all taken for each interval and assembled as damagesWeightingA *)
getUnitsMultiplierA[t_, complexityUnitsMultiplier_] := Module[{},
  (* TODO: maybe this is actually a sort of F, formal primes matrix? 
  or maybe eventually this is just going to be an additional one of the transformation matrices per property *)
  baseA = If[
    complexityUnitsMultiplier == "logOddLimit", (* TODO: eventually this will just be the other property *)
    DiagonalMatrix[Join[{0}, Table[1, getD[t] - 1]]],
    IdentityMatrix[getD[t]]
  ];
  
  If[
    (* when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNorm, covers TOP; 
    when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers TE; 
    when used by getDamagesWeightingA by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
    or by getTid by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
    covers any targeting-list tuning using this as its damage's complexity *)
    complexityUnitsMultiplier == "logProduct",
    DiagonalMatrix[Map[getLogProductComplexity[#, t]&, baseA]],
    
    If[
      (* when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNorm, covers BOP;
      when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers BE *)
      complexityUnitsMultiplier == "product",
      DiagonalMatrix[Map[getProductComplexity[#, t]&, baseA]],
      
      If[
        (* also covers TOP, TE, etc. (equivalent to "logProduct") *)
        complexityUnitsMultiplier == "logSopfr",
        DiagonalMatrix[Map[getLogSopfrComplexity[#, t]&, baseA]],
        
        If[
          (* also covers BOP, BE, etc. (equivalent to "product") *)
          complexityUnitsMultiplier == "sopfr",
          DiagonalMatrix[Map[getSopfrComplexity[#, t]&, baseA]],
          
          If[
            (* when Weil needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalCustomDualNorm, 
            where it's implemented separately (the min - max thing); 
            when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers WE; 
            when used by getDamagesWeightingA by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
            or by getTid by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
            should cover any targeting-list tuning using this as its damage's complexity *)
            complexityUnitsMultiplier == "logIntegerLimit",
            Join[DiagonalMatrix[Map[getLogProductComplexity[#, t]&, baseA]], {Map[getLogProductComplexity[#, meantone]&, baseA]}] / 2,
            
            If[
              (* when Kees needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalCustomDualNorm, 
              where it's implemented separately (the min - max thing) with pure-octave constraint on the solver; 
              ### still need to cover KE; when used by getDamagesWeightingA by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
              or by getTid by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
              should cover any targeting-list tuning using this as its damage's complexity *)
              complexityUnitsMultiplier == "logOddLimit",
              Join[DiagonalMatrix[Map[getLogProductComplexity[#, t]&, baseA]], {Map[getLogProductComplexity[#, t]&, baseA]}] / 2,
              
              (* when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllNumericalSimpleDualNorm, 
              covers L1 version of Frobenius; 
              when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, 
              covers Frobenius *)
              DiagonalMatrix[Map[1&, baseA]] (* TODO: should be copfrComplexity? *)
            ]
          ]
        ]
      ]
    ]
  ]
];
getCopfrComplexity[pcv_, t_] := Total[Map[If[Abs[# > 0], 1, 0]&, pcv]];
(* AKA "Benedetti height" *)
getProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getIntervalBasis[t], pcv}];
(* AKA "Tenney height" *)
getLogProductComplexity[pcv_, t_] := Log[2, getProductComplexity[pcv, t]];
(* AKA "Wilson height", can also be used to find BOP tuning *)
getSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getIntervalBasis[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find TOP tuning *)
getLogSopfrComplexity[pcv_, t_] := Log[2, getSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getIntegerLimitComplexity[pcv_, t_] := Module[{quotient},
  quotient = pcvToQuotient[pcv];
  Max[Numerator[quotient], Denominator[quotient]]
];
(* AKA "logarithmic Weil height", used for "Weil tuning" *)
getLogIntegerLimitComplexity[pcv_, t_] := Log[2, getIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
noTwos[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getOddLimitComplexity[pcv_, t_] := getIntegerLimitComplexity[noTwos[pcv], t];
(* AKA "Kees expressibility" , used for "Kees tuning" *)
getLogOddLimitComplexity[pcv_, t_] := Log[2, getOddLimitComplexity[pcv, t]];

optimizeGeneratorsTuningMapWithPseudoInverse[timaOrPrimesIdentityA_, WorUnitsMultiplierA_, t_, primesTuningMap_] := Module[{ma, weightedTargetedIntervalsAMapped, generatorsA, generatorsTuningMap},
  ma = getA[getM[t]];
  weightedTargetedIntervalsAMapped = ma.Transpose[timaOrPrimesIdentityA].WorUnitsMultiplierA;
  (* TODO: if we're constantly transposing targetedIntervalsA, maybe just do it once up front? or have getA respect the co/contra? *)
  generatorsA = Transpose[timaOrPrimesIdentityA].WorUnitsMultiplierA.Transpose[weightedTargetedIntervalsAMapped].Inverse[weightedTargetedIntervalsAMapped.Transpose[weightedTargetedIntervalsAMapped]];
  generatorsTuningMap = primesTuningMap.generatorsA;
  generatorsTuningMap // N
];

(* TODO eventually this should be able to just be a generic solver *)
(* TODO for now it just assumes unchangedIntervals is prime 2 and that's it, but eventually it can become an actual matrix like targetedIntervals *)
optimizeGeneratorsTuningMapCte[d_, t_, primesTuningMap_, complexityUnitsMultiplier_, unchangedIntervals_, complexityNormPower_] := Module[
  {generatorsTuningMap, ma, tuningMap, primesErrorMap, solution, optimizationPower, targetedIntervalsAsPrimesIdentityA, unitsCounterMultiplierA},
  
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  unitsCounterMultiplierA = getUnitsCounterMultiplierA[t, targetedIntervalsAsPrimesIdentityA, complexityUnitsMultiplier, complexityNormPower]; (* TODO: instead of counter-multiplier can it be like, units multiplier matrix for interval complexity, and units multiplier matrix for primes error magnitude? *)
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tuningMap = generatorsTuningMap.ma;
  
  primesErrorMap = tuningMap - primesTuningMap;
  primesErrorMap = unitsCounterMultiplierA.primesErrorMap;
  optimizationPower = 2;
  
  solution = NMinimize[{Norm[primesErrorMap, optimizationPower], primesErrorMap[[1]] == 0}, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
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
