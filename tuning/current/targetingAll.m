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
  {mappedSide, justSide},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes maximum norm"]];
  
  mappedSide = getTargetingAllMappedSide[tuningOptions];
  justSide = getTargetingAllJustSide[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[
    tuningOptions,
    mappedSide,
    justSide
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisum *)
(* no historically described tunings use this *)
optimizeGeneratorsTuningMapPrimesTaxicabNorm[tuningOptions_] := Module[
  {
    t,
    
    mappedSide,
    justSide,
    intervalsPartOfMappedSide,
    mappingPartOfMappedSide,
    primesTuningMapPartOfJustSide,
    weightingPartOfMappedSide,
    
    optimumGeneratorsTuningMap,
    
    complexityNormPower,
    primesErrorMagnitudeNormPower,
    generatorsTuningMap
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes taxicab norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  mappedSide = getTargetingAllMappedSide[tuningOptions];
  justSide = getTargetingAllJustSide[tuningOptions];
  intervalsPartOfMappedSide = getTargetingAllIntervalsPartOfMappedSide[tuningOptions];
  mappingPartOfMappedSide = getTargetingAllMappingPartOfMappedSide[tuningOptions];
  primesTuningMapPartOfJustSide = getTargetingAllPrimesTuningMapPartOfJustSide[tuningOptions];
  weightingPartOfMappedSide = getTargetingAllWeightingPartOfMappedSide[tuningOptions];
  
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMapAnalyticalSumPolytope[
    tuningOptions,
    mappedSide,
    justSide,
    intervalsPartOfMappedSide,
    mappingPartOfMappedSide,
    primesTuningMapPartOfJustSide,
    weightingPartOfMappedSide
  ];
  
  (* if the solution from the sum polytope is non-unique, it returns null, so we fall back to a power-limit solution *)
  If[
    optimumGeneratorsTuningMap === Null,
    
    If[tuningOption[tuningOptions, "debug"], Print["non-unique solution \[RightArrow] power limit solver"]];
    complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
    primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
    generatorsTuningMap = getTargetingAllGeneratorsTuningMap[tuningOptions];
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[
      primesErrorMagnitudeNormPower,
      tuningOptions,
      mappedSide,
      justSide,
      generatorsTuningMap
    ],
    
    optimumGeneratorsTuningMap
  ]
];

(* compare with optimizeGeneratorsTuningMapMinisos *)
(* covers minimax-ES "TE", minimax-NES "Frobenius", pure-octave-stretched minimax-ES "POTE", 
minimax-ZES "WE", minimax-PNES "BE" *)
optimizeGeneratorsTuningMapPrimesEuclideanNorm[tuningOptions_] := Module[
  {t, mappedSide, justSide},
  
  If[tuningOption[tuningOptions, "debug"], Print["primes Euclidean norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  mappedSide = getTargetingAllMappedSide[tuningOptions];
  justSide = getTargetingAllJustSide[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[tuningOptions, mappedSide, justSide]
];

(* compare with optimizeGeneratorsTuningMapMinisop *)
(* covers minimax-QZES "KE", unchanged-octave minimax-ES "CTE" *)
optimizeGeneratorsTuningMapPrimesPowerNorm[tuningOptions_] := Module[
  {
    complexityNormPower,
    primesErrorMagnitudeNormPower,
    
    mappedSide,
    justSide,
    generatorsTuningMap
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes power norm"]];
  
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  primesErrorMagnitudeNormPower = dualPower[complexityNormPower];
  
  mappedSide = getTargetingAllMappedSide[tuningOptions];
  justSide = getTargetingAllJustSide[tuningOptions];
  generatorsTuningMap = getTargetingAllGeneratorsTuningMap[tuningOptions];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[
    primesErrorMagnitudeNormPower,
    tuningOptions,
    mappedSide,
    justSide,
    generatorsTuningMap
  ]
];


(* TARGETING-ALL HELPER FUNCTIONS, MANY OF WHICH PARALLEL TARGETING-LIST FUNCTIONS *)

dualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getPrimeAbsErrorCounterweights[tuningOptions_] := Module[
  {
    t,
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    primeAbsErrorCounterweights
  },
  
  t = tuningOption[tuningOptions, "t"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"]; (* trait 4d *)
  
  primeAbsErrorCounterweights = DiagonalMatrix[Map[Function[
    {targetedIntervalPcv},
    getComplexity[
      targetedIntervalPcv,
      t,
      complexityNormPower, (* trait 3 *)
      complexityNegateLogPrimeCoordination, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityMakeOdd (* trait 4d *)
    ]
  ], getPrimesIdentityA[t]]];
  
  (* always simplicity weighted *)
  (* note: this is pseudo not because of non-square, due to complexity size factor,
  but because of when complexity is odd and the top-left entry is a 0 so det is 0 so it's singular *)
  PseudoInverse[primeAbsErrorCounterweights]
];

(* only used by optimizeGeneratorsTuningMapNumericalPower(Limit)Solver *)
(* returns g or augmented thereof if using size factor for complexity *)
getTargetingAllGeneratorsTuningMap[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, generatorsTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  
  If[
    complexitySizeFactor != 0,
    AppendTo[generatorsTuningMap, Symbol["gW"]];
  ];
  
  generatorsTuningMap
];

(* returns MTW (where T is primes identity I, and W is prime abs error counterweights) 
or augmented thereof if using size factor for complexity *)
getTargetingAllMappedSide[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, ma, primeAbsErrorCounterweights, mappedSide},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  ma = Part[tuningMappings, 2];
  
  primeAbsErrorCounterweights = getPrimeAbsErrorCounterweights[tuningOptions];
  
  mappedSide = ma.Transpose[getPrimesIdentityA[t]].primeAbsErrorCounterweights;
  
  If[
    complexitySizeFactor != 0,
    mappedSide = Map[Join[#, {0}]&, mappedSide];
    AppendTo[mappedSide, Join[Table[complexitySizeFactor, Last[Dimensions[mappedSide]] - 1], {-1}]];
  ];
  
  mappedSide
];

(* returns pTW (where T is primes identity I, and W is prime abs error counterweights) 
or augmented thereof if using size factor for complexity *)
getTargetingAllJustSide[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, primesTuningMap, primeAbsErrorCounterweights, justSide},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  primesTuningMap = Part[tuningMappings, 4];
  
  primeAbsErrorCounterweights = getPrimeAbsErrorCounterweights[tuningOptions];
  
  justSide = primesTuningMap.Transpose[getPrimesIdentityA[t]].primeAbsErrorCounterweights;
  
  If[
    complexitySizeFactor != 0,
    AppendTo[justSide, 0]
  ];
  
  justSide
];

(* only used by optimizeGeneratorsTuningMapPrimesTaxicabNorm, the problem child *)
(* TODO: this is not only of the mapped side, it's shared with both.
 and ultimately of course the goal is to get rid of the plain mapped Side and justSIde fns if they can be composed with these 
 in order for mappingSide to be composed out of all of these parts, this will have to be the primes tuning map but scaled by k then augmented with -1
 see https://docs.google.com/spreadsheets/d/1dCBcpmwm8b7lbomSwM1CKXD06lYuPvRzHXSSpWhXUiI/edit#gid=991893487 for more info
 *)
getTargetingAllIntervalsPartOfMappedSide[tuningOptions_] := Module[
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
getTargetingAllWeightingPartOfMappedSide[tuningOptions_] := Module[
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
getTargetingAllMappingPartOfMappedSide[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, ma},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  ma = Part[tuningMappings, 2];
  
  If[
    complexitySizeFactor != 0,
    ma = Map[Join[#, {0}]&, ma];
    AppendTo[ma, Join[Table[complexitySizeFactor, Last[Dimensions[ma]] - 1], {-1}]];
  ];
  
  ma
];
getTargetingAllPrimesTuningMapPartOfJustSide[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, primesTuningMap, primeAbsErrorCounterweights, justSide},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  
  tuningMappings = getTuningMappings[t];
  primesTuningMap = Part[tuningMappings, 4];
  
  If[
    complexitySizeFactor != 0,
    AppendTo[primesTuningMap, 0]
  ];
  
  primesTuningMap
];
