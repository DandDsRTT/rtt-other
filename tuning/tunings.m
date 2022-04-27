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
optimizeGeneratorsTuningMap[t_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    tuningOptions,
    optimizedGeneratorsTuningMap,
    tPossiblyWithChangedIntervalBasis,
    forDamage
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait -1 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  
  forDamage = False;
  
  tuningOptions = processTuningOptions[
    t,
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    forDamage
  ];
  
  optimizationPower = Part[tuningOptions, 3];
  tPossiblyWithChangedIntervalBasis = Part[tuningOptions, 10];
  pureOctaveStretch = Part[tuningOptions, 13];
  
  optimizedGeneratorsTuningMap = 1200 * If[
    optimizationPower == \[Infinity],
    
    (* covers minimax, TOP, TE, L1-style Frobenius, Frobenius, BOP, BE, Weil, WE, Kees, KE, CTE, POTOP, POTE *)
    optimizeGeneratorsTuningMapMinimax[tuningOptions],
    
    If[
      optimizationPower == 2,
      
      (* covers minisos *)
      optimizeGeneratorsTuningMapMinisos[tuningOptions],
      
      (* covers minisum *)
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
  
  SetAccuracy[optimizedGeneratorsTuningMap, 4]
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
optimizeTuningMap[t_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait -1 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  
  SetAccuracy[optimizeGeneratorsTuningMap[t, {
    "unchangedIntervals" -> unchangedIntervals, (* trait -1 *)
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> complexityNegateLogPrimeCoordination, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityMakeOdd" -> complexityMakeOdd, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis,
    "pureOctaveStretch" -> pureOctaveStretch,
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "originalComplexityName" -> originalComplexityName,
    "systematicComplexityName" -> systematicComplexityName
  }].getA[getM[t]], 4]
];


(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGeneratorsTuningMapMinimax[{
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  pureOctaveStretch_
}] := If[
  damageWeightingSlope == "simplicityWeighted" && Length[targetedIntervalsA] == 0,
  
  (* covers TOP, TE, L1-style Frobenius, Frobenius, BOP, BE, Weil, WE, Kees, KE, CTE, POTOP, POTE *)
  optimizeGeneratorsTuningMapTargetingAll[
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ],
  
  If[
    damageWeightingSlope == "unweighted",
    
    (* covers unweighted minimax *)
    optimizeGeneratorsTuningMapMinimaxAnalytical[
      unchangedIntervals, (* trait -1 *)
      targetedIntervalsA, (* trait 0 *)
      optimizationPower, (* trait 1 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd, (* trait 4d *)
      t,
      d,
      primesTuningMap
    ],
    
    (* covers weighted minimax *)
    optimizeGeneratorsTuningMapTargetingListNumerical[
      unchangedIntervals, (* trait -1 *)
      targetedIntervalsA, (* trait 0 *)
      optimizationPower, (* trait 1 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd, (* trait 4d *)
      t,
      d,
      primesTuningMap
    ]
  ]
];


(* TARGETING-ALL *)

optimizeGeneratorsTuningMapTargetingAll[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := If[
  complexityNormPower == 2 && Length[unchangedIntervals] == 0 && complexityMakeOdd == False,
  
  (* covers TE, Frobenius, WE, BE *)
  optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ],
  
  (* covers TOP, L1 version of Frobenius, BOP, Weil, Kees, KE, CTE *)
  optimizeGeneratorsTuningMapTargetingAllNumerical[
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[{dualMultiplier, targetedIntervalsAsPrimesIdentityA},
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  
  dualMultiplier = getDualMultiplier[
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[
    targetedIntervalsAsPrimesIdentityA,
    dualMultiplier,
    t,
    primesTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumerical[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {generatorsTuningMap, ma, tuningMap},
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tuningMap = generatorsTuningMap.ma;
  
  If[
    complexitySizeFactor != 0 && complexityNormPower == 1,
    
    (* covers Weil and Kees *)
    optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
      unchangedIntervals, (* trait -1 *)
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd, (* trait 4d *)
      t,
      d,
      primesTuningMap,
      tuningMap,
      generatorsTuningMap
    ],
    
    (* covers TOP, BOP, and L1-version of Frobenius, and KE and CTE even though they have a complexityNormPower of 2 *)
    optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
      unchangedIntervals, (* trait -1 *)
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd, (* trait 4d *)
      t,
      d,
      primesTuningMap,
      tuningMap,
      generatorsTuningMap
    ]
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := If[
  complexityNormPower != 2 && hasNonUniqueTuning[getM[t]],
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormWithoutUniqueResult[
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t,
    d,
    primesTuningMap,
    tuningMap,
    generatorsTuningMap
  ],
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t,
    d,
    primesTuningMap,
    tuningMap,
    generatorsTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := Module[
  {
    primesErrorMap,
    dualMultiplier,
    targetedIntervalsAsPrimesIdentityA,
    solution,
    optimizationPower,
    dualMultipliedPrimesErrorMap,
    minimizedNorm,
    periodsPerOctave
  },
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  
  dualMultiplier = getDualMultiplier[
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
  periodsPerOctave = getPeriodsPerOctave[t];
  
  dualMultipliedPrimesErrorMap = primesErrorMap.dualMultiplier;
  optimizationPower = dualPower[complexityNormPower];
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {Norm[dualMultipliedPrimesErrorMap, optimizationPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    Norm[dualMultipliedPrimesErrorMap, optimizationPower]
  ];
  solution = NMinimize[ minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormWithoutUniqueResult[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := Module[
  {
    primesErrorMap,
    dualMultiplier,
    targetedIntervalsAsPrimesIdentityA,
    solution,
    previousSolution,
    optimizationPower,
    previousPrimesErrorMagnitude,
    primesErrorMagnitude,
    normPowerPower,
    normPower,
    dualMultipliedPrimesErrorMap,
    minimizedNorm,
    periodsPerOctave
  },
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  dualMultiplier = getDualMultiplier[
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
  dualMultipliedPrimesErrorMap = primesErrorMap.dualMultiplier;
  optimizationPower = dualPower[complexityNormPower];
  previousPrimesErrorMagnitude = \[Infinity];
  primesErrorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  periodsPerOctave = getPeriodsPerOctave[t];
  
  While[
    normPowerPower <= 10 && previousPrimesErrorMagnitude != primesErrorMagnitude && previousPrimesErrorMagnitude - primesErrorMagnitude > 0,
    
    previousPrimesErrorMagnitude = primesErrorMagnitude;
    previousSolution = solution;
    minimizedNorm = If[
      Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
      {Norm[dualMultipliedPrimesErrorMap, optimizationPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
      Norm[dualMultipliedPrimesErrorMap, optimizationPower]
    ];
    solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
    primesErrorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  generatorsTuningMap /. Last[previousSolution] // N
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := Module[
  {adjustedPrimesErrorMap, solution, minimizedNorm, periodsPerOctave},
  
  adjustedPrimesErrorMap = tuningMap / primesTuningMap - Table[1, d];
  adjustedPrimesErrorMap = AppendTo[adjustedPrimesErrorMap, 0];
  periodsPerOctave = getPeriodsPerOctave[t];
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {Max[adjustedPrimesErrorMap] - Min[adjustedPrimesErrorMap], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    Max[adjustedPrimesErrorMap] - Min[adjustedPrimesErrorMap]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];

(* getDamageWeights may be simplicity-weighted or complexity-weighted, 
but this is always essentially "simplicity-weighted" *)
getDualMultiplier[
  targetedIntervalsAsPrimesIdentityA_, (* trait 0 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_
] := PseudoInverse[getComplexityMultiplier[
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd, (* trait 4d *)
  t
]];


(* TARGETING-LIST *)

optimizeGeneratorsTuningMapMinimaxAnalytical[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := optimizeGeneratorsTuningMapSimplex[
  unchangedIntervals, (* trait -1 *)
  targetedIntervalsA, (* trait 0 *)
  optimizationPower, (* trait 1 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd, (* trait 4d *)
  t,
  d,
  primesTuningMap
];

optimizeGeneratorsTuningMapMinisos[{
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  pureOctaveStretch_
}] := Module[
  {damageWeights},
  
  damageWeights = getDamageWeights[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[targetedIntervalsA, damageWeights, t, primesTuningMap]
];

optimizeGeneratorsTuningMapMinisum[{
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  pureOctaveStretch_
}] := optimizeGeneratorsTuningMapSimplex[
  unchangedIntervals, (* trait -1 *)
  targetedIntervalsA, (* trait 0 *)
  optimizationPower, (* trait 1 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd, (* trait 4d *)
  t,
  d,
  primesTuningMap
];

(* NUMERICAL - USED FOR WEIGHTED MINIMAX, AND FALLBACK FROM SIMPLEX WHEN RESULT IS NON-UNIQUE *)

optimizeGeneratorsTuningMapTargetingListNumerical[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := If[
  hasNonUniqueTuning[getM[t]],
  optimizeGeneratorsTuningMapTargetingListNumericalUnique[
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ],
  optimizeGeneratorsTuningMapTargetingListNumericalNonUnique[
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingListNumericalUnique[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {
    generatorsTuningMap,
    ma,
    mappedTargetedIntervalsA,
    pureTargetedIntervalsASizes,
    damageWeights,
    solution,
    targetedIntervalErrorsL,
    minimizedNorm
  },
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTargetedIntervalsASizes = Map[primesTuningMap.#&, targetedIntervalsA];
  damageWeights = getDamageWeights[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
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
      ] * Part[Part[damageWeights, targetedIntervalIndex, targetedIntervalIndex]]
    ],
    mappedTargetedIntervalsA
  ]];
  
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {Norm[targetedIntervalErrorsL, optimizationPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    Norm[targetedIntervalErrorsL, optimizationPower]
  ];
  
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

optimizeGeneratorsTuningMapTargetingListNumericalNonUnique[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {
    generatorsTuningMap,
    ma,
    mappedTargetedIntervalsA,
    pureTargetedIntervalsASizes,
    damageWeights,
    solution,
    errorMagnitude,
    previousErrorMagnitude,
    normPower,
    normPowerPower,
    targetedIntervalErrorsL,
    previousSolution,
    minimizedNorm
  },
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTargetedIntervalsASizes = Map[primesTuningMap.#&, targetedIntervalsA];
  damageWeights = getDamageWeights[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
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
      ] * Part[Part[damageWeights, targetedIntervalIndex, targetedIntervalIndex]]
    ],
    mappedTargetedIntervalsA
  ]];
  
  While[
    normPowerPower <= 10 && previousErrorMagnitude - errorMagnitude > 0,
    
    previousErrorMagnitude = errorMagnitude;
    previousSolution = solution;
    minimizedNorm = If[
      Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
      {Norm[targetedIntervalErrorsL, normPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
      Norm[targetedIntervalErrorsL, normPower]
    ];
    solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
    errorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  generatorsTuningMap /. Last[previousSolution] // N
];


(* ANALYTICAL  *)

optimizeGeneratorsTuningMapSimplex[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialProjectionAs,
    potentialTuningMaps,
    potentialTuningMapDamages,
    minDamageTuningMapIndices,
    minDamageTuningMapIndex,
    minDamageProjectionA,
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
  potentialProjectionAs = Select[Map[
    getProjectionAFromUnchangedIntervals[t, #]&,
    filteredNormalizedPotentialUnchangedIntervalSets
  ], Not[# === Null]&];
  potentialTuningMaps = Map[primesTuningMap.#&, potentialProjectionAs];
  potentialTuningMapDamages = Map[damagePowerSum[
    #,
    t,
    primesTuningMap,
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]&, potentialTuningMaps];
  
  minDamageTuningMapIndices = Position[potentialTuningMapDamages, Min[potentialTuningMapDamages]];
  If[
    Length[minDamageTuningMapIndices] == 1,
    
    (* result is unique; done *)
    minDamageTuningMapIndex = First[First[Position[potentialTuningMapDamages, Min[potentialTuningMapDamages]]]];
    minDamageProjectionA = potentialProjectionAs[[minDamageTuningMapIndex]];
    generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[t]]];
    projectedGenerators = minDamageProjectionA.generatorsPreimageTransversal;
    primesTuningMap.projectedGenerators // N,
    
    (* result is not unique; fallback to numerical solution *)
    optimizeGeneratorsTuningMapTargetingListNumerical[
      unchangedIntervals, (* trait -1 *)
      targetedIntervalsA, (* trait 0 *)
      optimizationPower, (* trait 1 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd, (* trait 4d *)
      t,
      d,
      primesTuningMap
    ]
  ]
];

getProjectionAFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[
  {commaEigenvectors, eigenvectors, diagonalEigenvalueA},
  
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueA = getDiagonalEigenvalueA[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueA.Inverse[eigenvectors]]
];

getDiagonalEigenvalueA[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];

optimizeGeneratorsTuningMapWithPseudoInverse[
  potentiallyPrimesIdentityTargetedIntervalsA_,
  damageWeightingOrDualMultiplier_,
  t_,
  primesTuningMap_
] := Module[{ma, weightedOrMultipliedTargetedIntervalsAMapped, generatorsA, generatorsTuningMap},
  ma = getA[getM[t]];
  weightedOrMultipliedTargetedIntervalsAMapped = ma.
      Transpose[potentiallyPrimesIdentityTargetedIntervalsA].
      damageWeightingOrDualMultiplier;
  generatorsA = Transpose[potentiallyPrimesIdentityTargetedIntervalsA].
      damageWeightingOrDualMultiplier.
      Transpose[weightedOrMultipliedTargetedIntervalsAMapped].
      Inverse[
        weightedOrMultipliedTargetedIntervalsAMapped.Transpose[weightedOrMultipliedTargetedIntervalsAMapped]
      ];
  generatorsTuningMap = primesTuningMap.generatorsA;
  generatorsTuningMap // N
];


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

getDamage[t_, generatorsTuningMap_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    ma,
    tuningMap,
    primesTuningMap,
    tuningOptions,
    targetedIntervalsA,
    forDamage
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait -1 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  
  forDamage = True;
  
  tuningOptions = processTuningOptions[
    t,
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    forDamage
  ];
  optimizationPower = Part[tuningOptions, 1];
  targetedIntervalsA = Part[tuningOptions, 2];
  
  ma = getA[getM[t]];
  tuningMap = (generatorsTuningMap / 1200).ma;
  primesTuningMap = getPrimesTuningMap[t];
  
  If[
    optimizationPower == \[Infinity],
    getMaxDamage[
      tuningMap,
      t,
      primesTuningMap,
      targetedIntervalsA, (* trait 0 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexitySizeFactor, (* trait 4b *)
      complexityPrimePower, (* trait 4c *)
      complexityMakeOdd (* trait 4d *)
    ],
    If[
      optimizationPower == 2,
      get2SumDamage[
        tuningMap,
        t,
        primesTuningMap,
        targetedIntervalsA, (* trait 0 *)
        damageWeightingSlope, (* trait 2 *)
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexitySizeFactor, (* trait 4b *)
        complexityPrimePower, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ],
      getSumDamage[
        tuningMap,
        t,
        primesTuningMap,
        targetedIntervalsA, (* trait 0 *)
        damageWeightingSlope, (* trait 2 *)
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexitySizeFactor, (* trait 4b *)
        complexityPrimePower, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ]
  ]
];

getTargetedIntervalDamages[
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_,
  tuningMap_,
  primesTuningMap_
] := Module[
  {targetedIntervalErrorsList, targetedIntervalDamagesList, damageWeights},
  
  targetedIntervalErrorsList = N[tuningMap.Transpose[targetedIntervalsA]] - N[primesTuningMap.Transpose[targetedIntervalsA]];
  targetedIntervalDamagesList = Abs[targetedIntervalErrorsList];
  damageWeights = getDamageWeights[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
  
  targetedIntervalDamagesList.damageWeights
];

Square[n_] := n^2;

getSumDamage[
  tuningMap_,
  t_,
  primesTuningMap_,
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Total[getTargetedIntervalDamages[
  targetedIntervalsA, (* trait 0 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexitySizeFactor, (* trait 4b *)
  complexityPrimePower, (* trait 4c *)
  complexityMakeOdd, (* trait 4d *)
  t,
  tuningMap,
  primesTuningMap
]];

get2SumDamage[
  tuningMap_,
  t_,
  primesTuningMap_,
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Total[Square[getTargetedIntervalDamages[
  targetedIntervalsA, (* trait 0 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexitySizeFactor, (* trait 4b *)
  complexityPrimePower, (* trait 4c *)
  complexityMakeOdd, (* trait 4d *)
  t,
  tuningMap,
  primesTuningMap
]]];

getMaxDamage[
  tuningMap_,
  t_,
  primesTuningMap_,
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Max[getTargetedIntervalDamages[
  targetedIntervalsA, (* trait 0 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexitySizeFactor, (* trait 4b *)
  complexityPrimePower, (* trait 4c *)
  complexityMakeOdd, (* trait 4d *)
  t,
  tuningMap,
  primesTuningMap
]];

getDamageWeights[
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_
] := Module[{damageWeights},
  damageWeights = If[
    damageWeightingSlope == "unweighted",
    
    IdentityMatrix[Length[targetedIntervalsA]],
    
    DiagonalMatrix[Map[getComplexity[
      #,
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd, (* trait 4d *)
      t
    ]&, targetedIntervalsA]]
  ];
  
  If[
    damageWeightingSlope == "simplicityWeighted",
    PseudoInverse[damageWeights],
    damageWeights
  ]
];


(* COMPLEXITY *)

getComplexity[
  pcv_,
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_
] := Module[{complexityMultiplier},
  complexityMultiplier = getComplexityMultiplier[
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    t
  ];
  
  Norm[complexityMultiplier.pcv, complexityNormPower]
];

(* Note that we don't actually use any of these functions directly; they're just around to test understanding *)
getPcvCopfrComplexity[pcv_, t_] := Total[Map[If[Abs[# > 0], 1, 0]&, pcv]];
(* AKA "Benedetti height" *)
getPcvProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getIntervalBasis[t], pcv}];
(* AKA "Tenney height" *)
getPcvLogProductComplexity[pcv_, t_] := Log[2, getPcvProductComplexity[pcv, t]];
(* AKA "Wilson height", can also be used to find BOP tuning *)
getPcvSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getIntervalBasis[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find TOP tuning *)
getPcvLogSopfrComplexity[pcv_, t_] := Log[2, getPcvSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getPcvIntegerLimitComplexity[pcv_, t_] := Module[{quotient},
  quotient = pcvToQuotient[pcv];
  Max[Numerator[quotient], Denominator[quotient]]
];
(* AKA "logarithmic Weil height", used for "Weil tuning" *)
getPcvLogIntegerLimitComplexity[pcv_, t_] := Log[2, getPcvIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
removePowersOfTwoFromPcv[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getPcvOddLimitComplexity[pcv_, t_] := getPcvIntegerLimitComplexity[removePowersOfTwoFromPcv[pcv], t];
(* AKA "Kees expressibility" , used for "Kees tuning" *)
getPcvLogOddLimitComplexity[pcv_, t_] := Log[2, getPcvOddLimitComplexity[pcv, t]];


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


(* TUNING UNIQUENESS *)

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
      index = whichGeneratorIsTheSingleOneApproximatingThisPrime[First[Part[generatorsApproximatingEachPrime, indexOfPrimeApproximatedBySingleGenerator]]];
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


(* SHARED *)

tuningOptions = {
  "unchangedIntervals" -> {}, (* trait -1 *)
  "targetedIntervals" -> Null, (* trait 0 *)
  "optimizationPower" -> Null, (* trait 1 *)
  "damageWeightingSlope" -> "", (* trait 2 *)
  "complexityNormPower" -> 1, (* trait 3 *)
  "complexityNegateLogPrimeCoordination" -> False, (* trait 4a: False = do nothing, True = negate the multiplication by logs of primes *)
  "complexityPrimePower" -> 0, (* trait 4b: what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when log prime coordination is negated and otherwise defaults; 1 = product complexity, equiv to sopfr when log prime coordination is negated and otherwise defaults; >1 = pth power of those *)
  "complexitySizeFactor" -> 0, (* trait 4c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = Weil style, etc. *)
  "complexityMakeOdd" -> False, (* trait 4d: False = do nothing, True = achieve Kees from Weil, KE from WE, etc. *)
  "tuningIntervalBasis" -> "primes",
  "pureOctaveStretch" -> False,
  "systematicTuningName" -> "",
  "originalTuningName" -> "",
  "systematicComplexityName" -> "",
  "originalComplexityName" -> ""
};
Options[optimizeTuningMap] = tuningOptions;
Options[optimizeGeneratorsTuningMap] = tuningOptions;
Options[getDamage] = tuningOptions;

processTuningOptions[
  t_,
  inputUnchangedIntervals_, (* trait -1 *)
  inputTargetedIntervals_, (* trait 0 *)
  inputOptimizationPower_, (* trait 1 *)
  inputDamageWeightingSlope_, (* trait 2 *)
  inputComplexityNormPower_, (* trait 3 *)
  inputComplexityNegateLogPrimeCoordination_, (* trait 4a *)
  inputComplexityPrimePower_, (* trait 4b *)
  inputComplexitySizeFactor_, (* trait 4c *)
  inputComplexityMakeOdd_, (* trait 4d *)
  inputTuningIntervalBasis_,
  inputPureOctaveStretch_,
  inputSystematicTuningName_,
  inputOriginalTuningName_,
  inputSystematicComplexityName_,
  inputOriginalComplexityName_,
  forDamage_ : False
] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    primesTuningMap,
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA,
    d,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    intervalBasis,
    intervalRebase
  },
  
  unchangedIntervals = inputUnchangedIntervals; (* trait -1 *)
  targetedIntervals = inputTargetedIntervals; (* trait 0 *)
  optimizationPower = inputOptimizationPower; (* trait 1 *)
  damageWeightingSlope = inputDamageWeightingSlope; (* trait 2 *)
  complexityNormPower = inputComplexityNormPower; (* trait 3 *)
  complexityNegateLogPrimeCoordination = inputComplexityNegateLogPrimeCoordination; (* trait 4a *)
  complexityPrimePower = inputComplexityPrimePower; (* trait 4b *)
  complexitySizeFactor = inputComplexitySizeFactor; (* trait 4c *)
  complexityMakeOdd = inputComplexityMakeOdd; (* trait 4d *)
  tuningIntervalBasis = inputTuningIntervalBasis;
  pureOctaveStretch = inputPureOctaveStretch;
  systematicTuningName = inputSystematicTuningName;
  originalTuningName = inputOriginalTuningName;
  systematicComplexityName = inputSystematicComplexityName;
  originalComplexityName = inputOriginalComplexityName;
  
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
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    originalTuningName === "TE" || originalTuningName === "Tenney-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E";
  ];
  If[
    originalTuningName === "Frobenius",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "NE";
  ];
  If[
    originalTuningName === "BOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "PN";
  ];
  If[
    originalTuningName === "BE" || originalTuningName === "Benedetti-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "PNE";
  ];
  If[
    originalTuningName === "Weil",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";systematicComplexityName = "Z";
  ];
  If[
    originalTuningName === "WE" || originalTuningName === "Weil-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "ZE";
  ];
  If[
    originalTuningName === "Kees",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "QZ";
  ];
  If[
    originalTuningName === "KE" || originalTuningName === "Kees-Euclidean",
    (* Note how this tuning works by enforcing an unchanged octave via a solver constraint, rather than through the complexity units multiplier *)
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "ZE"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
  ];
  If[
    originalTuningName === "POTOP" || originalTuningName === "POTT",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "POTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "CTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "E"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
  ];
  
  (* trait 1 *)
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
  
  (* trait 2 *)
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
  
  (* trait 3 - same as complexity systematic name parts *)
  If[
    StringMatchQ[systematicTuningName, "*E*"] || StringMatchQ[systematicComplexityName, "*E*"],
    complexityNormPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningName, "*T*"] || StringMatchQ[systematicComplexityName, "*T*"],
    complexityNormPower = 1;
  ];
  
  (* trait 4 - same as complexity systematic name parts  *)
  If[
    StringMatchQ[systematicTuningName, "*N*"] || StringMatchQ[systematicComplexityName, "*N*"],
    complexityNegateLogPrimeCoordination = True;
  ];
  If[
    StringMatchQ[systematicTuningName, "*P*"] || StringMatchQ[systematicComplexityName, "*P*"],
    complexityPrimePower = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Z*"] || StringMatchQ[systematicComplexityName, "*Z*"],
    complexitySizeFactor = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Q*"] || StringMatchQ[systematicComplexityName, "*Q*"],
    complexityMakeOdd = True;
  ];
  
  (* Note: we can't implement product complexity with the current design, and don't intend to revise.
   This is because product complexity is realized from a PC-vector as a product of terms,
    raised to the powers of the absolute values of the entries. But this design only multiplies entries and sums them. 
    Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    originalComplexityName === "copfr" || originalComplexityName === "l1Norm",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "sopfr" || originalComplexityName === "wilsonHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 1; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "integerLimit" || originalComplexityName === "weilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "oddLimit" || originalComplexityName === "keesHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "logProduct" || originalComplexityName === "tenneyHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logIntegerLimit" || originalComplexityName === "logarithmicWeilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logOddLimit" || originalComplexityName === "keesExpressibility",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "rososcopfr" || originalComplexityName === "l2Norm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "rosossopfr",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 1; complexityMakeOdd = False;
  ];
  (* (following the pattern here, this one might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ]; *)
  (* (following the pattern here, this one might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ]; *)
  If[
    originalComplexityName === "tenneyEuclideanHeight",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "weilEuclideanNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "keesEuclideanSeminorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    originalComplexityName === "carlsNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 2; complexityMakeOdd = False;
  ];
  
  (* This has to go below the systematic tuning name gating, so that targetedIntervals has a change to be set to {} *)
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
  
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tPossiblyWithChangedIntervalBasis,
    d,
    primesTuningMap,
    pureOctaveStretch
  }
];

getPrimesTuningMap[t_] := Log[2, getIntervalBasis[t]];

(* This is different than the damageWeights, this is nested within it;
this is to weight the quantities of the PC-vectors before taking a norm and getting an interval complexity, 
which are then all taken for each interval and assembled as damageWeights *)
getComplexityMultiplier[
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  t_
] := Module[{complexityMultiplier},
  (* When used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm, covers TOP; 
when used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers TE; 
when used by getDamageWeights by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
or by getTargetedIntervalDamages by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
covers any targeting-list tuning using this as its damage's complexity *)
  complexityMultiplier = getLogPrimeCoordinationA[t];
  
  If[
    (* When used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm,
    covers L1 version of Frobenius;
    when used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical,
    covers Frobenius *)
    complexityNegateLogPrimeCoordination == True,
    complexityMultiplier = complexityMultiplier.Inverse[getLogPrimeCoordinationA[t]]
  ];
  
  If[
    (* When used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm, covers BOP;
    when used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers BE *)
    complexityPrimePower > 0,
    complexityMultiplier = complexityMultiplier.DiagonalMatrix[Power[getIntervalBasis[t], complexityPrimePower]]
  ];
  
  If[
    (* When Weil needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the min - max thing); 
    when used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers WE or KE
    (surprisingly KE does not use the below; it instead uses this and applies an unchanged octave constraint); 
    when used by getDamageWeights by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
    or by getTargetedIntervalDamages by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
    should cover any targeting-list tuning using this as its damage's complexity *)
    complexitySizeFactor > 0,
    complexityMultiplier = (Join[IdentityMatrix[getD[t]], {Table[complexitySizeFactor, getD[t]]}] / (1 + complexitySizeFactor)).complexityMultiplier
  ];
  
  If[
    (* When Kees needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the min - max thing) with pure-octave constraint on the solver; 
    note again that this is is not used for KE; see note above;
    when used by getDamageWeights by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
    or by getTargetedIntervalDamages by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
    should cover any targeting-list tuning using this as its damage's complexity *)
    complexityMakeOdd == True,
    complexityMultiplier = complexityMultiplier.DiagonalMatrix[Join[{complexityMakeOdd}, Table[1, getD[t] - 1]]]
  ];
  
  complexityMultiplier
];

getLogPrimeCoordinationA[t_] := DiagonalMatrix[Log2[getIntervalBasis[t]]];

getPeriodsPerOctave[t_] := First[First[getA[getM[t]]]];
