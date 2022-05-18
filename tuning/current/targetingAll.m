(* TARGETING-ALL *)

(* covers targeting-all (includes 
minimax-S "TOP", minimax-ES "TE", minimax-NES "Frobenius", 
minimax-PNS "BOP", minimax-PNES "BE", 
minimax-ZS "Weil", minimax-ZES "WE", minimax-QZS "Kees", minimax-QZES "KE", 
unchanged-octave minimax-ES "CTE", 
pure-octave-stretched minimax-S "POTOP", pure-octave-stretched minimax-ES "POTE") *)
(* compare with optimizeGeneratorsTuningMapTargetingList *)
optimizeGeneratorsTuningMapTargetingAll[tuningOptions_] := Module[
  {unchangedIntervals, complexityNormPower, complexitySizeFactor, optimumGeneratorsTuningMap},
  
  If[tuningOption[tuningOptions, "debug"], Print["targeting-all"]];
  
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
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
  {
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes maximum norm"]];
  
  (* TODO: consolidate into a `getParts` function; also deconstructing in place like {jim, jen, jake} = bob actually works so maybe use that? *)
  temperedSideGeneratorsPart = getTargetingAllTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingAllTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingAllJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingAllJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingAllEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingAllEitherSideMultiplierPart[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[
    tuningOptions,
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisum *)
(* no historically described tunings use this *)
optimizeGeneratorsTuningMapPrimesTaxicabNorm[tuningOptions_] := Module[
  {
    t,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart,
    
    optimumGeneratorsTuningMap,
    
    complexityNormPower,
    primesErrorMagnitudeNormPower
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes taxicab norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  temperedSideGeneratorsPart = getTargetingAllTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingAllTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingAllJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingAllJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingAllEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingAllEitherSideMultiplierPart[tuningOptions];
  
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMapAnalyticalSumPolytope[
    tuningOptions,
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  ];
  
  (* if the solution from the sum polytope is non-unique, it returns null, so we fall back to a power-limit solution *)
  If[
    optimumGeneratorsTuningMap === Null,
    
    If[tuningOption[tuningOptions, "debug"], Print["non-unique solution \[RightArrow] power limit solver"]];
    complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
    primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[
      primesErrorMagnitudeNormPower,
      tuningOptions,
      temperedSideGeneratorsPart,
      temperedSideMappingPart,
      justSideGeneratorsPart,
      justSideMappingPart,
      eitherSideIntervalsPart,
      eitherSideMultiplierPart
    ],
    
    optimumGeneratorsTuningMap
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisos *)
(* covers minimax-ES "TE", minimax-NES "Frobenius", pure-octave-stretched minimax-ES "POTE", 
minimax-ZES "WE", minimax-PNES "BE" *)
optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions_] := Module[
  {
    t,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes Euclidean norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  temperedSideGeneratorsPart = getTargetingAllTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingAllTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingAllJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingAllJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingAllEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingAllEitherSideMultiplierPart[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[
    tuningOptions,
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisop *)
(* covers minimax-QZES "KE", unchanged-octave minimax-ES "CTE" *)
optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions_] := Module[
  {
    complexityNormPower,
    primesErrorMagnitudeNormPower,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes power norm"]];
  
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
  
  temperedSideGeneratorsPart = getTargetingAllTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingAllTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingAllJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingAllJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingAllEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingAllEitherSideMultiplierPart[tuningOptions];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[
    primesErrorMagnitudeNormPower,
    tuningOptions,
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  ]
];


(* TARGETING-ALL HELPER FUNCTIONS, MANY OF WHICH PARALLEL TARGETING-LIST FUNCTIONS *)

dualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* TODO: this has gone back to being more of a dualMultiplier *)
(* compare with getDamageWeights *)
getPrimeAbsErrorCounterweights[tuningOptions_] := Module[
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
  (* when computing tunings (as opposed to complexities directly), complexity-make-odd is handled through constraints *)
  complexityMakeOdd = False; (* trait 4d *)
  
  tuningInverse[getComplexityMultiplier[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]]
];

(* returns g or augmented thereof if using size factor for complexity *)
getTargetingAllTemperedSideGeneratorsPart[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, generatorsTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  
  If[
    complexitySizeFactor != 0,
    AppendTo[generatorsTuningMap, Symbol["gW"]];
  ];
  
  {generatorsTuningMap}
];

(* returns M or augmented thereof if using size factor for complexity *)
getTargetingAllTemperedSideMappingPart[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, ma},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  ma = Part[tuningMappings, 2];
  
  If[
    complexitySizeFactor != 0,
    ma = Map[Join[#, {0}]&, ma];
    AppendTo[ma, Join[Table[complexitySizeFactor, Last[Dimensions[ma]] - 1].getLogPrimeCoordinationA[t], {-1}]];
  ];
  
  ma
];

(* returns p or augmented thereof if using size factor for complexity *)
getTargetingAllJustSideGeneratorsPart[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, primesTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  primesTuningMap = Part[tuningMappings, 4];
  
  If[
    complexitySizeFactor != 0,
    AppendTo[primesTuningMap, 0]
  ];
  
  {primesTuningMap}
];

(* returns I or augmented thereof if using size factor for complexity *)
getTargetingAllJustSideMappingPart[tuningOptions_] := Module[
  {t, complexitySizeFactor, justMapping},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  justMapping = getPrimesIdentityA[t];
  
  If[
    complexitySizeFactor != 0,
    justMapping = Map[Join[#, {0}]&, justMapping];
    AppendTo[justMapping, Join[Table[0, Last[Dimensions[justMapping]] - 1], {1}]];
  ];
  
  justMapping
];

(* returns I or augmented thereof if using size factor for complexity *)
getTargetingAllEitherSideIntervalsPart[tuningOptions_] := Module[
  {t, complexitySizeFactor, intervalsSide},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  intervalsSide = Transpose[getPrimesIdentityA[t]];
  
  If[
    complexitySizeFactor != 0,
    intervalsSide = Map[Join[#, {0}]&, intervalsSide];
    AppendTo[intervalsSide, Join[Table[0, Last[Dimensions[intervalsSide]] - 1], {1}]]; (* TODO: DRY this sort of shenanigans up? *)
  ];
  
  intervalsSide
];

(* returns X⁻¹ or augmented thereof if using size factor for complexity *)
getTargetingAllEitherSideMultiplierPart[tuningOptions_] := Module[
  {complexitySizeFactor, primeAbsErrorCounterweights, intervalsSide},
  
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  primeAbsErrorCounterweights = getPrimeAbsErrorCounterweights[tuningOptions];
  
  If[
    complexitySizeFactor != 0,
    primeAbsErrorCounterweights = Map[Join[#, {0}]&, primeAbsErrorCounterweights];
    AppendTo[primeAbsErrorCounterweights, Join[Table[0, Last[Dimensions[primeAbsErrorCounterweights]] - 1], {1}]];
  ];
  
  primeAbsErrorCounterweights
];
