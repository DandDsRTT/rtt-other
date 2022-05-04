optimizeGeneratorsTuningMapTargetingList[tuningOptions_] := Module[
  {unchangedIntervals, optimizationPower, complexityMakeOdd},
  
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"];
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"];
  
  If[
    optimizationPower == \[Infinity] && Length[unchangedIntervals] == 0 && complexityMakeOdd == False,
    optimizeGeneratorsTuningMapMinimax[tuningOptions],
    If[
      optimizationPower == 1 && Length[unchangedIntervals] == 0 && complexityMakeOdd == False ,
      optimizeGeneratorsTuningMapMinisum[tuningOptions],
      If[
        optimizationPower == 2 && Length[unchangedIntervals] == 0 && complexityMakeOdd == False ,
        optimizeGeneratorsTuningMapMinisos[tuningOptions],
        optimizeGeneratorsTuningMapMinisop[tuningOptions]
      ]
    ]
  ]
];

optimizeGeneratorsTuningMapMinimax[tuningOptions_] := Module[
  {t, targetedIntervalsA, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  damageWeights = getDamageWeights[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t, targetedIntervalsA, damageWeights]
];

optimizeGeneratorsTuningMapMinisum[tuningOptions_] := optimizeGeneratorsTuningMapAnalyticalSumPolytope[tuningOptions];

optimizeGeneratorsTuningMapMinisumNonunique[tuningOptions_] := Module[
  {
    t,
    unchangedIntervals,
    complexityMakeOdd,
    
    tuningMappings,
    generatorsTuningMap,
    tuningMap,
    
    damagesMagnitude,
    previousDamagesMagnitude,
    previousSolution,
    normPower,
    normPowerPower,
    
    damagesL,
    normFn,
    normPowerLimit,
    periodsPerOctave,
    minimizedNorm,
    solution
  },
  
  t = tuningOption[tuningOptions, "t"];
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"];
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"];
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  tuningMap = Part[tuningMappings, 3];
  
  damagesL = getTargetedIntervalDamagesL[tuningMap, tuningOptions];
  normFn = Norm;
  normPowerLimit = 1;
  
  damagesMagnitude = 1000000;
  previousDamagesMagnitude = \[Infinity];
  normPower = 2;
  normPowerPower = 1;
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  While[
    normPowerPower <= 6 && previousDamagesMagnitude - damagesMagnitude > 0,
    previousDamagesMagnitude = damagesMagnitude;
    previousSolution = solution;
    minimizedNorm = If[
      Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
      {normFn[damagesL, normPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
      normFn[damagesL, normPower]
    ];
    solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
    damagesMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = Power[2, 1 / normPowerPower];
  ];
  generatorsTuningMap /. Last[previousSolution]
];

optimizeGeneratorsTuningMapMinisos[tuningOptions_] := Module[
  {t, targetedIntervalsA, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  damageWeights = getDamageWeights[tuningOptions];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[t, targetedIntervalsA, damageWeights]
];

optimizeGeneratorsTuningMapMinisop[tuningOptions_] := Module[
  {
    t,
    optimizationPower,
    
    tuningMappings,
    tuningMap,
    
    targetedIntervalDamagesL
  },
  
  t = tuningOption[tuningOptions, "t"];
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
  
  tuningMappings = getTuningMappings[t];
  tuningMap = Part[tuningMappings, 3];
  
  targetedIntervalDamagesL = getTargetedIntervalDamagesL[tuningMap, tuningOptions];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[tuningOptions, targetedIntervalDamagesL, optimizationPower]
];
