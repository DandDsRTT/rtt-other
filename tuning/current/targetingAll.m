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
  
  If[tuningOption[tuningOptions, "debug"], Print["targeting-all"]];
  
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
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    
    (* covers KE, CTE *)
    optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions],
    
    If[
      complexityNormPower == 2,
      
      (* covers TE, POTE, Frobenius, WE, BE *)
      optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions],
      
      If[
        complexityNormPower == 1,
        
        (* covers TOP, POTOP, L1 version of Frobenius, BOP, Weil, Kees *)
        optimizeGeneratorsTuningMapPrimesMaximumNorm[tuningOptions],
        
        If[
          complexityNormPower == \[Infinity],
          
          (* no historically described tunings use this *)
          optimizeGeneratorsTuningMapPrimesTaxicabNorm[tuningOptions],
          
          (* no historically described tunings use this *)
          optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions]
        ]
      ]
    ]
  ]
];

(* compare with optimizeGeneratorsTuningMapMinimax *)
(* covers TOP, POTOP, L1 version of Frobenius, BOP, Weil, Kees *)
optimizeGeneratorsTuningMapPrimesMaximumNorm[tuningOptions_] := Module[
  {t, targetedIntervalsAsPrimesIdentityA, dualMultiplier},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes maximum norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t, targetedIntervalsAsPrimesIdentityA, dualMultiplier]
];

(* compare with optimizeGeneratorsTuningMapMinisum *)
(* no historically described tunings use this *)
optimizeGeneratorsTuningMapPrimesTaxicabNorm[tuningOptions_] := Module[
  {
    t,
    targetedIntervalsAsPrimesIdentityA,
    
    complexityNormPower,
    tuningMappings,
    tuningMap,
    dualMultipliedPrimesErrorL,
    primesErrorMagnitudeNormPower
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes taxicab norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  
  (* if the solution from the sum polytope is non-unique, fall back to a power limit solution *)
  Check[
    optimizeGeneratorsTuningMapAnalyticalSumPolytope[t, targetedIntervalsA, getSumPrimesAbsError],
  
    If[tuningOption[tuningOptions, "debug"], Print["non-unique solution → power limit solver"]];
    complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"];
    tuningMappings = getTuningMappings[t];
    tuningMap = Part[tuningMappings, 3];
    dualMultipliedPrimesErrorL = getDualMultipliedPrimesErrorL[tuningMap, tuningOptions];
    primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[tuningOptions, dualMultipliedPrimesErrorL, primesErrorMagnitudeNormPower]
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisos *)
(* covers TE, POTE, Frobenius, WE, BE *)
optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions_] := Module[
  {t, targetedIntervalsAsPrimesIdentityA, dualMultiplier},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes Euclidean norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[t, targetedIntervalsAsPrimesIdentityA, dualMultiplier]
];

(* compare with optimizeGeneratorsTuningMapMinisop *)
(* covers KE, CTE *)
optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions_] := Module[
  {
    t,
    complexityNormPower,
    
    tuningMappings,
    tuningMap,
    
    dualMultipliedPrimesErrorL,
    primesErrorMagnitudeNormPower
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes power norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"];
  
  tuningMappings = getTuningMappings[t];
  tuningMap = Part[tuningMappings, 3];
  
  dualMultipliedPrimesErrorL = getDualMultipliedPrimesErrorL[tuningMap, tuningOptions];
  primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[tuningOptions, dualMultipliedPrimesErrorL, primesErrorMagnitudeNormPower]
];


(* TARGETING-ALL HELPER FUNCTIONS, MANY OF WHICH PARALLEL TARGETING-LIST FUNCTIONS *)

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

(* compare with getSumDamage *)
getSumPrimesAbsError[tuningMap_, tuningOptions_] := Total[getDualMultipliedPrimesErrorL[tuningMap, tuningOptions]];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];
