optimizeGeneratorsTuningMapTargetingList[tuningOptions_] := Module[
  {unchangedIntervals, optimizationPower, complexityMakeOdd},
  
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"];
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"];
  
  If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
  
    (* no historically described tunings use this *)
    optimizeGeneratorsTuningMapMinisop[tuningOptions],
    
    If[
      optimizationPower == 2,
  
      (* covers least squares *)
      optimizeGeneratorsTuningMapMinisos[tuningOptions],
      
      If[
        optimizationPower == \[Infinity],
  
        (* covers minimax *)
        optimizeGeneratorsTuningMapMinimax[tuningOptions],
        
        If[
          optimizationPower == 1,
  
          (* no historically described tunings use this *)
          optimizeGeneratorsTuningMapMinisum[tuningOptions],
  
          (* no historically described tunings use this *)
          optimizeGeneratorsTuningMapMinisop[tuningOptions]
        ]
      ]
    ]
  ]
];

(* covers minimax *)
optimizeGeneratorsTuningMapMinimax[tuningOptions_] := Module[
  {t, targetedIntervalsA, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  damageWeights = getDamageWeights[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t, targetedIntervalsA, damageWeights]
];

(* no historically described tunings use this *)
optimizeGeneratorsTuningMapMinisum[tuningOptions_] := Module[
  {
    t,
    targetedIntervalsA,
    
    optimizationPower,
    tuningMappings,
    tuningMap,
    targetedIntervalDamagesL
  },
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  
  (* if the solution from the sum polytope is non-unique, fall back to a power limit solution *)
  Check[
    optimizeGeneratorsTuningMapAnalyticalSumPolytope[tuningOptions, targetedIntervalsA, getSumDamage],
    
    optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
    tuningMappings = getTuningMappings[t];
    tuningMap = Part[tuningMappings, 3];
    targetedIntervalDamagesL = getTargetedIntervalDamagesL[tuningMap, tuningOptions];
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[tuningOptions, targetedIntervalDamagesL, optimizationPower]
  ]
];

(* covers least squares *)
optimizeGeneratorsTuningMapMinisos[tuningOptions_] := Module[
  {t, targetedIntervalsA, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  damageWeights = getDamageWeights[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[t, targetedIntervalsA, damageWeights]
];

(* no historically described tunings use this *)
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
