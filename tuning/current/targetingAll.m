(* TARGETING-ALL *)

(* covers targeting-all (includes 
minimax-S "TOP", minimax-ES "TE", minimax-NES "Frobenius", 
minimax-PNS "BOP", minimax-PNES "BE", 
minimax-ZS "Weil", minimax-ZES "WE", minimax-QZS "Kees", minimax-QZES "KE", 
unchanged-octave minimax-ES "CTE", 
pure-octave-stretched minimax-S "POTOP", pure-octave-stretched minimax-ES "POTE") *)
(* compare with optimizeGeneratorsTuningMapTargetingList *)
optimizeGeneratorsTuningMapTargetingAll[tuningOptions_] := Module[
  {complexityNormPower, complexitySizeFactor, unchangedIntervals, optimumGeneratorsTuningMap},
  
  If[tuningOption[tuningOptions, "debug"], Print["targeting-all"]];
  
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  
  optimumGeneratorsTuningMap = If[
    Length[unchangedIntervals] > 0,
    
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
  ];
  
  If[
    complexitySizeFactor != 0,
    Drop[optimumGeneratorsTuningMap, -1],
    optimumGeneratorsTuningMap
  ]
];

(* compare with optimizeGeneratorsTuningMapMinimax *)
(* covers minimax-S "TOP", pure-octave-stretched minimax-S "POTOP", 
minimax-PNS "BOP", minimax-ZS "Weil", minimax-QZS "Kees" *)
optimizeGeneratorsTuningMapPrimesMaximumNorm[tuningOptions_] := Module[
  {targetingAllParts},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes maximum norm"]];
  
  targetingAllParts = getTargetingAllParts[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[targetingAllParts]
];

(* compare with optimizeGeneratorsTuningMapMinisum *)
(* no historically described tunings use this *)
optimizeGeneratorsTuningMapPrimesTaxicabNorm[tuningOptions_] := Module[
  {
    targetingAllParts,
    
    optimumGeneratorsTuningMap,
    
    t,
    complexityNormPower,
    unchangedIntervals,
    
    primesErrorMagnitudeNormPower,
    periodsPerOctave
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes taxicab norm"]];
  
  targetingAllParts = getTargetingAllParts[tuningOptions];
  
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMapAnalyticalSumPolytope[targetingAllParts];
  
  (* if the solution from the sum polytope is non-unique, it returns null, so we fall back to a power-limit solution *)
  If[
    optimumGeneratorsTuningMap === Null,
    
    If[tuningOption[tuningOptions, "debug"], Print["non-unique solution \[RightArrow] power limit solver"]];
    
    t = tuningOption[tuningOptions, "t"];
    complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
    unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
    
    primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
    periodsPerOctave = getPeriodsPerOctave[t];
    
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[
      targetingAllParts,
      primesErrorMagnitudeNormPower,
      unchangedIntervals,
      periodsPerOctave
    ],
    
    optimumGeneratorsTuningMap
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisos *)
(* covers minimax-ES "TE", minimax-NES "Frobenius", pure-octave-stretched minimax-ES "POTE", 
minimax-ZES "WE", minimax-PNES "BE" *)
optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions_] := Module[
  {targetingAllParts},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes Euclidean norm"]];
  
  targetingAllParts = getTargetingAllParts[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[targetingAllParts]
];

(* compare with optimizeGeneratorsTuningMapMinisop *)
(* covers minimax-QZES "KE", unchanged-octave minimax-ES "CTE" *)
optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions_] := Module[
  {
    targetingAllParts,
    
    t,
    complexityNormPower,
    unchangedIntervals,
    
    primesErrorMagnitudeNormPower,
    periodsPerOctave
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes power norm"]];
  
  targetingAllParts = getTargetingAllParts[tuningOptions];
  
  t = tuningOption[tuningOptions, "t"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  
  primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
  periodsPerOctave = getPeriodsPerOctave[t];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[
    targetingAllParts,
    primesErrorMagnitudeNormPower,
    unchangedIntervals,
    periodsPerOctave
  ]
];


(* TARGETING-ALL HELPER FUNCTIONS, MANY OF WHICH PARALLEL TARGETING-LIST FUNCTIONS *)

dualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getDualMultiplier[tuningOptions_] := Module[
  {
    t,
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    
    complexityMultiplier
  },
  
  t = tuningOption[tuningOptions, "t"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  (* when computing tunings (as opposed to complexities directly), complexity-make-odd is handled through constraints *)
  complexityMakeOdd = False; (* trait 4d *)
  
  complexityMultiplier = getComplexityMultiplier[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  (* always essentially simplicity weighted *)
  tuningInverse[complexityMultiplier]
];

getTargetingAllParts[tuningOptions_] := Module[
  {
    t,
    complexitySizeFactor,
    
    generatorsTuningMap,
    ma,
    primesTuningMap,
    
    dualMultiplier,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  {generatorsTuningMap, ma, primesTuningMap} = getTuningMappings[t];
  
  dualMultiplier = getDualMultiplier[tuningOptions];
  
  justSideMappingPart = getPrimesIdentityA[t];
  eitherSideIntervalsPart = Transpose[getPrimesIdentityA[t]];
  
  If[
    complexitySizeFactor != 0,
    
    AppendTo[generatorsTuningMap, Symbol["gW"]];
    
    ma = Map[Join[#, {0}]&, ma];
    AppendTo[ma, Join[Table[complexitySizeFactor, Last[Dimensions[ma]] - 1].getLogPrimeCoordinationA[t], {-1}]];
    
    AppendTo[primesTuningMap, 0];
    
    justSideMappingPart = basicComplexitySizeFactorAugmentation[justSideMappingPart];
    
    eitherSideIntervalsPart = basicComplexitySizeFactorAugmentation[eitherSideIntervalsPart];
    
    dualMultiplier = basicComplexitySizeFactorAugmentation[dualMultiplier];
  ];
  
  temperedSideGeneratorsPart = {generatorsTuningMap};
  temperedSideMappingPart = ma;
  justSideGeneratorsPart = {primesTuningMap};
  eitherSideMultiplierPart = dualMultiplier;
  
  {
    temperedSideGeneratorsPart, (* g *)
    temperedSideMappingPart, (* M *)
    justSideGeneratorsPart, (* p *)
    justSideMappingPart, (* I *)
    eitherSideIntervalsPart, (* I *)
    eitherSideMultiplierPart (* X⁻¹ *)
  }
];

basicComplexitySizeFactorAugmentation[a_] := Module[
  {augmentedA},
  
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  augmentedA
]
