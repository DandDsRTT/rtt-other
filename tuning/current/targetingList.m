optimizeGeneratorsTuningMapTargetingList[{
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
  optimizationPower == \[Infinity],
  optimizeGeneratorsTuningMapMinimax[{
    t,
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    pureOctaveStretch
  }],
  If[
    optimizationPower == 2,
    optimizeGeneratorsTuningMapMinisos[{
      t,
      unchangedIntervals, (* trait -1 *)
      targetedIntervalsA, (* trait 0 *)
      optimizationPower, (* trait 1 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd, (* trait 4d *)
      pureOctaveStretch
    }],
    If[
      optimizationPower == 1,
      optimizeGeneratorsTuningMapMinisum[{
        t,
        unchangedIntervals, (* trait -1 *)
        targetedIntervalsA, (* trait 0 *)
        optimizationPower, (* trait 1 *)
        damageWeightingSlope, (* trait 2 *)
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd, (* trait 4d *)
        pureOctaveStretch
      }],
      optimizeGeneratorsTuningMapMinisop[{
        t,
        unchangedIntervals, (* trait -1 *)
        targetedIntervalsA, (* trait 0 *)
        optimizationPower, (* trait 1 *)
        damageWeightingSlope, (* trait 2 *)
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd, (* trait 4d *)
        pureOctaveStretch
      }]
    ]
  ]
];

optimizeGeneratorsTuningMapMinimax[{
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
}] := Module[
  {damageWeights},
  
  damageWeights = getDamageWeights[
    t,
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t, targetedIntervalsA, damageWeights]
];

optimizeGeneratorsTuningMapMinisos[{
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
}] := Module[
  {damageWeights},
  
  damageWeights = getDamageWeights[
    t,
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[t, targetedIntervalsA, damageWeights]
];

optimizeGeneratorsTuningMapMinisum[{
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
}] := optimizeGeneratorsTuningMapAnalyticalSumPolytope[
  t,
  unchangedIntervals, (* trait -1 *)
  targetedIntervalsA, (* trait 0 *)
  optimizationPower, (* trait 1 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd (* trait 4d *)
];

optimizeGeneratorsTuningMapMinisop[{
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
}] := optimizeGeneratorsTuningMapNumericalPowerSolver[
  t,
  unchangedIntervals, (* trait -1 *)
  targetedIntervalsA, (* trait 0 *)
  optimizationPower, (* trait 1 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd (* trait 4d *)
];
