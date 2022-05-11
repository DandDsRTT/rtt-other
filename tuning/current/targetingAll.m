(* TARGETING-ALL *)

(* covers targeting-all (includes 
minimax-S "TOP", minimax-ES "TE", minimax-NES "Frobenius", 
minimax-PNS "BOP", minimax-PNES "BE", 
minimax-ZS "Weil", minimax-ZES "WE", minimax-QZS "Kees", minimax-QZES "KE", 
unchanged-octave minimax-ES "CTE", 
pure-octave-stretched minimax-S "POTOP", pure-octave-stretched minimax-ES "POTE") *)
(* compare with optimizeGeneratorsTuningMapTargetingList *)
optimizeGeneratorsTuningMapTargetingAll[tuningOptions_] := Module[
  {unchangedIntervals, complexityNormPower},
  
  If[tuningOption[tuningOptions, "debug"], Print["targeting-all"]];
  
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  
  If[
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
    ma,
    intervals,
    primesTuningMap,
    
    optimumGeneratorsTuningMap,
    
    complexityNormPower,
    primesErrorMagnitudeNormPower,
    generatorsTuningMap
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["primes taxicab norm"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  mappedSide = getTargetingAllMappedSide[tuningOptions];
  justSide = getTargetingAllJustSide[tuningOptions];
  ma = getTargetingAllMa[tuningOptions];
  intervals = getTargetingAllIntervals[tuningOptions];
  primesTuningMap = getTargetingAllPrimesTuningMap[tuningOptions];
  
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMapAnalyticalSumPolytope[
    tuningOptions,
    mappedSide,
    justSide,
    ma,
    intervals,
    primesTuningMap
  ];
  
  (* if the solution from the sum polytope is non-unique, it returns null, so we fall back to a power-limit solution *)
  If[
    optimumGeneratorsTuningMap === Null,
    
    If[tuningOption[tuningOptions, "debug"], Print["non-unique solution \[RightArrow] power limit solver"]];
    complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
    (* TODO: this could also be wrapped in a targeting-all version to hide the differences
     I feel like this happens in more than one place
     but I'm also slightly hesitant about this because it feels of a slightly different sort of thing as the other
     targetingAll vs targetingList functions... this one would definitely only be used by the power and power limit solutions, at least*)
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

(* used by optimizeGeneratorsTuningMapPrimesTaxicabNorm (problem child) and optimizeGeneratorsTuningMapPrimesPowerNorm *)
(* returns g or augmented thereof if using size factor for complexity *)
getTargetingAllGeneratorsTuningMap[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, generatorsTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"];
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  
  If[
    complexitySizeFactor != 0,
    AppendTo[generatorsTuningMap, Symbol["gW"]];
  ];
  
  generatorsTuningMap
];

(* used by all four solutions!! *)
(* returns MTW (where T is primes identity I, and W is prime abs error counterweights) 
or augmented thereof if using size factor for complexity *)
getTargetingAllMappedSide[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, ma, primeAbsErrorCounterweights, mappedSide},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"];
  
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

(* used by all four solutions!! *)
(* returns pTW (where T is primes identity I, and W is prime abs error counterweights) 
or augmented thereof if using size factor for complexity *)
getTargetingAllJustSide[tuningOptions_] := Module[
  {t, complexitySizeFactor, tuningMappings, primesTuningMap, primeAbsErrorCounterweights, justSide},
  
  t = tuningOption[tuningOptions, "t"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"];
  
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

(* these three are all only used by optimizeGeneratorsTuningMapPrimesTaxicabNorm, the problem child *)
(* compare with getTargetingListPrimesTuningMap (it's identical, actually) *)
getTargetingAllPrimesTuningMap[tuningOptions_] := Module[
  {t, tuningMappings, primesTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  primesTuningMap = Part[tuningMappings, 4];
  
  primesTuningMap
];
(* compare with getTargetingListIntervals *)
getTargetingAllIntervals[tuningOptions_] := Module[
  {t},
  
  t = tuningOption[tuningOptions, "t"];
  
  getPrimesIdentityA[t]
];
(* compare with getTargetingListMapping (it's identical, actually) *)
getTargetingAllMa[tuningOptions_] := Module[
  {t, tuningMappings},
  
  t = tuningOption[tuningOptions, "t"];
  tuningMappings = getTuningMappings[t];
  
  Part[tuningMappings, 2]
];
