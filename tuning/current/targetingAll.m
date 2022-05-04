(* TARGETING-ALL *)

(* compare with optimizeGeneratorsTuningMapTargetingList *)
optimizeGeneratorsTuningMapTargetingAll[tuningOptions_] := Module[
  {
    unchangedIntervals,
    optimizationPower,
    damageWeightingSlope,
    complexityNormPower,
    complexityMakeOdd
  },
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
  If[
    optimizationPower != \[Infinity],
    Throw["It is not possible to optimize for minisum or minisos over all intervals, only minimax."]
  ];
  damageWeightingSlope = tuningOption[tuningOptions, "damageWeightingSlope"];
  If[
    damageWeightingSlope != "simplicityWeighted",
    Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weighted."]
  ];
  
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"];
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"];
  
  If[
    complexityNormPower == 1 && Length[unchangedIntervals] == 0 && complexityMakeOdd == False , (* TODO: conditions could be cleaned up, here and in -list version of this fn *)
    
    (* covers TOP, L1 version of Frobenius, BOP, Weil, Kees *)
    optimizeGeneratorsTuningMapPrimesMaximumNorm[tuningOptions],
    
    If[
      complexityNormPower == \[Infinity] && Length[unchangedIntervals] == 0 && complexityMakeOdd == False,
      
      (* no described tunings use this as of yet *)
      optimizeGeneratorsTuningMapPrimesTaxicabNorm[tuningOptions],
      
      If[
        complexityNormPower == 2 && Length[unchangedIntervals] == 0 && complexityMakeOdd == False,
        
        (* covers TE, Frobenius, WE, BE *)
        optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions],
        
        (* covers KE, CTE *)
        optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions]
      ]
    ]
  ]
];

(* compare with optimizeGeneratorsTuningMapMinimax *)
optimizeGeneratorsTuningMapPrimesMaximumNorm[tuningOptions_] := Module[
  {t, targetedIntervalsAsPrimesIdentityA, dualMultiplier},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t, targetedIntervalsAsPrimesIdentityA, dualMultiplier]
];

(* TODO: should follow the pattern of optimizeGeneratorsTuningMapTargetingList,
 and have actual bindings for this analytical solution,
 which should be able to be generic to targeting-list and targeting-all situations,
  which is currently a problem both for it itself and its non-unique path *)
(* compare with optimizeGeneratorsTuningMapMinisum *)
optimizeGeneratorsTuningMapPrimesTaxicabNorm[tuningOptions_] := optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions];

(* compare with optimizeGeneratorsTuningMapMinisos *)
optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions_] := Module[
  {t, targetedIntervalsAsPrimesIdentityA, dualMultiplier},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[t, targetedIntervalsAsPrimesIdentityA, dualMultiplier]
];

(* compare with optimizeGeneratorsTuningMapMinisop *)
optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions_] := Module[
  {
    t,
    complexityNormPower,
    
    tuningMappings,
    tuningMap,
    
    dualMultipliedPrimesErrorL,
    primesErrorMagnitudeNormPower
  },
  
  t = tuningOption[tuningOptions, "t"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"];
  
  tuningMappings = getTuningMappings[t];
  tuningMap = Part[tuningMappings, 3];
  
  dualMultipliedPrimesErrorL = getDualMultipliedPrimesErrorL[tuningMap, tuningOptions];
  primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[tuningOptions, dualMultipliedPrimesErrorL, primesErrorMagnitudeNormPower]
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
(* that may be simplicity-weighted or complexity-weighted, but this is always essentially "simplicity-weighted" *)
getDualMultiplier[tuningOptions_] := Module[
  {
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  },
  
  t = tuningOption[tuningOptions, "t"];
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"]; (* trait 4d *)
  
  PseudoInverse[getComplexityMultiplier[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]]
];

(* being dual multiplied, it's essentially weighted and thus essentially a damage, and we'll assign it to 
a variable with that name for purposes of generic implementation; *)
(* compare with getTargetedIntervalDamagesL *)
getDualMultipliedPrimesErrorL[tuningMap_, tuningOptions_] := Module[
  {t, targetedIntervalsAsPrimesIdentityA, primesTuningMap, dualMultiplier},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  primesTuningMap = getPrimesTuningMap[t];
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  Abs[N[tuningMap - primesTuningMap, 256].Transpose[targetedIntervalsAsPrimesIdentityA].dualMultiplier]
];
