(* covers targeting-list (includes 
unchanged-octave diamond minimax-U "minimax", unchanged-octave diamond minisos-U "least squares") *)
optimizeGeneratorsTuningMapTargetingList[tuningOptions_] := Module[
  {unchangedIntervals, optimizationPower},
  
  If[tuningOption[tuningOptions, "debug"], Print["targeting-list"]];
  
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  
  If[
    Length[unchangedIntervals] > 0,
    
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

(* covers unchanged-octave diamond minimax-U "minimax" *)
optimizeGeneratorsTuningMapMinimax[tuningOptions_] := Module[
  {mappedSide, justSide},
  
  If[tuningOption[tuningOptions, "debug"], Print["minimax"]];
  
  mappedSide = getTargetingListMappedSide[tuningOptions];
  justSide = getTargetingListJustSide[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[
    tuningOptions,
    mappedSide,
    justSide
  ]
];

(* no historically described tunings use this *)
optimizeGeneratorsTuningMapMinisum[tuningOptions_] := Module[
  {
    t,
    
    mappedSide,
    justSide,
    ma,
    intervals,
    primesTuningMap,
    
    optimumGeneratorsTuningMap,
    
    optimizationPower,
    generatorsTuningMap
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisum"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  mappedSide = getTargetingListMappedSide[tuningOptions];
  justSide = getTargetingListJustSide[tuningOptions];
  ma = getTargetingListMa[tuningOptions];
  intervals = getTargetingListIntervals[tuningOptions];
  primesTuningMap = getTargetingListPrimesTuningMap[tuningOptions];
  
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
    optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
    generatorsTuningMap = getTargetingListGeneratorsTuningMap[tuningOptions];
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[
      optimizationPower,
      tuningOptions,
      mappedSide,
      justSide,
      generatorsTuningMap
    ],
    
    optimumGeneratorsTuningMap
  ]
];

(* covers unchanged-octave diamond minisos-U "least squares" *)
optimizeGeneratorsTuningMapMinisos[tuningOptions_] := Module[
  {t, mappedSide, justSide},
  
  If[tuningOption[tuningOptions, "debug"], Print["minisos"]];
  
  t = tuningOption[tuningOptions, "t"];
  mappedSide = getTargetingListMappedSide[tuningOptions];
  justSide = getTargetingListJustSide[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[tuningOptions, mappedSide, justSide]
];

(* no historically described tunings use this *)
optimizeGeneratorsTuningMapMinisop[tuningOptions_] := Module[
  {
    optimizationPower,
    
    mappedSide,
    justSide,
    generatorsTuningMap
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisop"]];
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  
  mappedSide = getTargetingListMappedSide[tuningOptions];
  justSide = getTargetingListJustSide[tuningOptions];
  generatorsTuningMap = getTargetingListGeneratorsTuningMap[tuningOptions];
  (* TODO: I thought this had to be power limit because it could be 1 or \[Infinity] for now 
  because it also goes in here if there are unchanged intervals... 
  and that we should make this change for the targeting-all analog function too... 
  or we could just have it be smart and only do power-limit if 1 or \[Infinity]... 
  which I briefly experimented upon here. but then something crazy happened! I had the case of 
  testClose[optimizeTuningMap, meantone, "systematicTuningName" -> "unchanged-octave diamond minimax-U", {1200.000, 1896.578, 2786.314}] 
  which was giving {1200.000, 1896.566, 2786.263} when it tried to do power limit solver because, 
  I assume it's because of the constraint, 
  the damage when the power was 2 which is where it starts off was the lowest, 
  and every increment toward power being 1 made the damage WORSE. 
  now if you think about it in this case when the constraint pure octave is on, 
  that means the solution is non-unique because it's just the g1 = 1200 cross-section of the graph you get with plotDamage at this time, 
  and there's a clear minimum that way... *)
  optimizeGeneratorsTuningMapNumericalPowerSolver[
    optimizationPower,
    tuningOptions,
    mappedSide,
    justSide,
    generatorsTuningMap
  ]
];

(* compare with getPrimeAbsErrorCounterweights *)
getDamageWeights[tuningOptions_] := Module[
  {
    t,
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    damageWeights
  },
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  damageWeightingSlope = tuningOption[tuningOptions, "damageWeightingSlope"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"]; (* trait 4d *)
  
  damageWeights = If[
    damageWeightingSlope == "unweighted",
    
    IdentityMatrix[Length[targetedIntervalsA]],
    
    DiagonalMatrix[Map[Function[
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
    ], targetedIntervalsA]]
  ];
  
  If[
    damageWeightingSlope == "simplicityWeighted",
    
    (* note: this is pseudo not because of non-square, due to complexity size factor,
    but because of when complexity is odd and the top-left entry is a 0 so det is 0 so it's singular *)
    PseudoInverse[damageWeights],
    
    damageWeights
  ]
];

(* used by optimizeGeneratorsTuningMapMinisum (problem child) and optimizeGeneratorsTuningMapMinisop *)
(* returns g *)
getTargetingListGeneratorsTuningMap[tuningOptions_] := Module[
  {t, tuningMappings, generatorsTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  
  generatorsTuningMap
];

(* used by all four solutions!! *)
(* returns MTW *)
getTargetingListMappedSide[tuningOptions_] := Module[
  {t, targetedIntervalsA, tuningMappings, ma, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  
  tuningMappings = getTuningMappings[t];
  ma = Part[tuningMappings, 2];
  
  damageWeights = getDamageWeights[tuningOptions];
  
  ma.Transpose[targetedIntervalsA].damageWeights
];

(* used by all four solutions!! *)
(* returns pTW *)
getTargetingListJustSide[tuningOptions_] := Module[
  {t, targetedIntervalsA, tuningMappings, primesTuningMap, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  
  tuningMappings = getTuningMappings[t];
  primesTuningMap = Part[tuningMappings, 4];
  
  damageWeights = getDamageWeights[tuningOptions];
  
  primesTuningMap.Transpose[targetedIntervalsA].damageWeights
];

(* these three are all only used by optimizeGeneratorsTuningMapMinisum, the problem child *)
(* compare with getTargetingAllPrimesTuningMap (it's identical, actually) *)
getTargetingListPrimesTuningMap[tuningOptions_] := Module[
  {t, tuningMappings, primesTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  primesTuningMap = Part[tuningMappings, 4];
  
  primesTuningMap
];
(* compare with getTargetingAllIntervals *)
getTargetingListIntervals[tuningOptions_] := Module[
  {},
  
  tuningOption[tuningOptions, "targetedIntervalsA"]
];
(* compare with getTargetingAllMapping (it's identical, actually) *)
getTargetingListMa[tuningOptions_] := Module[
  {t, tuningMappings},
  
  t = tuningOption[tuningOptions, "t"];
  tuningMappings = getTuningMappings[t];
  
  Part[tuningMappings, 2]
];
