(* TARGETING-ALL *)

optimizeGeneratorsTuningMapTargetingAll[{
  t_,
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  pureOctaveStretch_
}] := If[
  damageWeightingSlope != "simplicityWeighted",
  Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weighted."],
  If[
    optimizationPower != \[Infinity],
    Throw["It is not possible to optimize for minisum or minisos over all intervals, only minimax."],
    If[
      complexityNormPower == 2 && Length[unchangedIntervals] == 0 && complexityMakeOdd == False,
      
      (* covers TE, Frobenius, WE, BE *)
      optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
        t,
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ],
      
      (* covers TOP, L1 version of Frobenius, BOP, Weil, Kees, KE, CTE *)
      optimizeGeneratorsTuningMapTargetingAllNumerical[
        t,
        unchangedIntervals, (* trait -1 *)
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ]
  ]
];

optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
  t_,
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[{dualMultiplier, targetedIntervalsAsPrimesIdentityA},
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[
    t,
    targetedIntervalsAsPrimesIdentityA,
    dualMultiplier
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumerical[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := If[
  complexitySizeFactor != 0 && complexityNormPower == 1,
  
  (* covers Weil and Kees *)
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ],
  
  (* covers TOP, BOP, and L1-version of Frobenius, and KE and CTE even though they have a complexityNormPower of 2 *)
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := If[
  complexityNormPower != 2 && hasNonUniqueTuning[getM[t]],
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormWithoutUniqueResult[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ],
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
    primesErrorMap,
    dualMultiplier,
    targetedIntervalsAsPrimesIdentityA,
    solution,
    optimizationPower,
    dualMultipliedPrimesErrorMap,
    minimizedNorm,
    periodsPerOctave
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
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
  
  generatorsTuningMap /. Last[solution]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormWithoutUniqueResult[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
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
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
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
  
  generatorsTuningMap /. Last[previousSolution]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
    periodsPerOctave,
    adjustedPrimesErrorMap,
    minimizedNorm,
    solution
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  adjustedPrimesErrorMap = tuningMap / primesTuningMap - Table[1, getD[t]];
  adjustedPrimesErrorMap = AppendTo[adjustedPrimesErrorMap, 0];
  
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {Max[adjustedPrimesErrorMap] - Min[adjustedPrimesErrorMap], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    Max[adjustedPrimesErrorMap] - Min[adjustedPrimesErrorMap]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution]
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];

(* getDamageWeights may be simplicity-weighted or complexity-weighted, 
but this is always essentially "simplicity-weighted" *)
getDualMultiplier[
  t_,
  targetedIntervalsAsPrimesIdentityA_, (* trait 0 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := PseudoInverse[getComplexityMultiplier[
  t,
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd (* trait 4d *)
]];
