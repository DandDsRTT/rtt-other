(* TARGETING-ALL *)

(* covers targeting-all (includes 
minimax-S "TOP", minimax-ES "TE", minimax-NES "Frobenius", 
minimax-PNS "BOP", minimax-PNES "BE", 
minimax-ZS "Weil", minimax-ZES "WE", minimax-QZS "Kees", minimax-QZES "KE", 
unchanged-octave minimax-ES "CTE", 
pure-octave-stretched minimax-S "POTOP", pure-octave-stretched minimax-ES "POTE") *)
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
    
    (* covers minimax-QZES "KE", unchanged-octave minimax-ES "CTE" *)
    optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions],
    
    If[
      complexityNormPower == 2,
      
      (* covers minimax-ES "TE", minimax-NES "Frobenius", pure-octave-stretched minimax-ES "POTE", 
      minimax-ZES "WE", minimax-PNES "BE" *)
      optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions],
      
      If[
        complexityNormPower == 1,
        
        (* covers minimax-S "TOP", pure-octave-stretched minimax-S "POTOP", 
        minimax-PNS "BOP", minimax-ZS "Weil", minimax-QZS "Kees" *)
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
(* covers minimax-S "TOP", pure-octave-stretched minimax-S "POTOP", 
minimax-PNS "BOP", minimax-ZS "Weil", minimax-QZS "Kees" *)
optimizeGeneratorsTuningMapPrimesMaximumNorm[tuningOptions_] := Module[
  {t, complexitySizeFactor, targetedIntervalsAsPrimesIdentityA, dualMultiplier},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes maximum norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"];
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t, targetedIntervalsAsPrimesIdentityA, dualMultiplier, complexitySizeFactor]
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
    
    If[tuningOption[tuningOptions, "debug"], Print["non-unique solution \[RightArrow] power limit solver"]];
    complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"];
    tuningMappings = getTuningMappings[t];
    tuningMap = Part[tuningMappings, 3];
    dualMultipliedPrimesErrorL = getDualMultipliedPrimesErrorL[tuningMap, tuningOptions];
    primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[tuningOptions, dualMultipliedPrimesErrorL, primesErrorMagnitudeNormPower]
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisos *)
(* covers minimax-ES "TE", minimax-NES "Frobenius", pure-octave-stretched minimax-ES "POTE", 
minimax-ZES "WE", minimax-PNES "BE" *)
optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions_] := Module[
  {t(*, complexitySizeFactor*), targetedIntervalsAsPrimesIdentityA, dualMultiplier},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes Euclidean norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  (*  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"];*)
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[t, targetedIntervalsAsPrimesIdentityA, dualMultiplier(*, complexitySizeFactor*)]
];

(* compare with optimizeGeneratorsTuningMapMinisop *)
(* covers minimax-QZES "KE", unchanged-octave minimax-ES "CTE" *)
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
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  },
  
  t = tuningOption[tuningOptions, "t"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"]; (* trait 4d *)
  
  (* will be handled elsewhere in optimizeGeneratorsTuningMapSemianalyticalMaxPolytope *)
  If[complexityNormPower == 1, complexitySizeFactor = 0];
  
  (*  tuningInverse[getComplexityMultiplier[*)
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
  
  Abs[N[tuningMap - primesTuningMap, absoluteValuePrecision].Transpose[targetedIntervalsAsPrimesIdentityA].dualMultiplier]
];

(* compare with getSumDamage *)
getSumPrimesAbsError[tuningMap_, tuningOptions_] := Total[getDualMultipliedPrimesErrorL[tuningMap, tuningOptions]];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];
