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
      
      (* TODO: this too needs to follow the pattern of optimizeGeneratorsTuningMapTargetingList *)
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
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormOfIntegerLimit[
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
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormNonUnique[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ],
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormUnique[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormUnique[
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
    tuningMap,
    
    targetedIntervalsAsPrimesIdentityA,
    
    damagesL,
    normFn,
    normPower,
    periodsPerOctave,
    minimizedNorm,
    solution
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  tuningMap = Part[tuningMappings, 3];
  
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  damagesL = getDualMultipliedPrimesErrorL[
    tuningMap,
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  normFn = Norm;
  normPower = dualPower[complexityNormPower];
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {normFn[damagesL, normPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    normFn[damagesL, normPower]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  generatorsTuningMap /. Last[solution]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormNonUnique[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {targetedIntervalsAsPrimesIdentityA, dualMultiplier},
  
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    (* always essentially simplicity-weighted *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t, targetedIntervalsAsPrimesIdentityA, dualMultiplier]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormOfIntegerLimit[
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
    tuningMap,
    
    damagesL,
    normFn,
    periodsPerOctave,
    minimizedNorm,
    solution
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  tuningMap = Part[tuningMappings, 3];
  
  damagesL = getAugmentedDualMultipliedPrimesErrorL[
    tuningMap,
    t
  ];
  normFn = dualNormOfIntegerLimit;
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {normFn[damagesL], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    normFn[damagesL]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  generatorsTuningMap /. Last[solution]
];

getAugmentedDualMultipliedPrimesErrorL[
  tuningMap_,
  t_
] := Module[
  {primesTuningMap, damagesL},
  
  (* TODO: for simplicity, keep it this way for now, 
  but I don't like the inconsistency in this dividing tuning map by primes tuning map and subtracting 1's...
  what's the difference between that and, well...
  confirm whether multiplying by the appropriate dual multiplier would work out the same *)
  primesTuningMap = getPrimesTuningMap[t];
  
  damagesL = tuningMap / primesTuningMap - Table[1, getD[t]];
  
  (* TODO: see if you can reconsolidate this now, that is no formerly-known-as "middleMan" shenanigans necessary
  but also you should extract following the pattern of the other two *)
  AppendTo[damagesL, 0]
];

(* as described here: https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space#Dual_Norms *)
dualNormOfIntegerLimit[vector_] := Max[vector] - Min[vector];

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

(* being dual multiplied, it's essentially weighted and thus essentially a damage, and we'll assign it to 
a variable with that name for purposes of generic implementation; compare with getTargetedIntervalDamagesL *)
getDualMultipliedPrimesErrorL[
  tuningMap_,
  t_,
  targetedIntervalsAsPrimesIdentityA_, (* trait 0 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {primesTuningMap, dualMultiplier},
  
  primesTuningMap = getPrimesTuningMap[t];
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    (* always essentially simplicity-weighted *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  Abs[(tuningMap - primesTuningMap).targetedIntervalsAsPrimesIdentityA.dualMultiplier]
];
