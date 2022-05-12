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
    intervalsPartOfMappedSide,
    mappingPartOfMappedSide,
    primesTuningMapPartOfJustSide,
    weightingPartOfMappedSide,
    
    optimumGeneratorsTuningMap,
    
    optimizationPower,
    generatorsTuningMap
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisum"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  mappedSide = getTargetingListMappedSide[tuningOptions];
  justSide = getTargetingListJustSide[tuningOptions];
  intervalsPartOfMappedSide = getTargetingListIntervalsPartOfMappedSide[tuningOptions];
  mappingPartOfMappedSide = getTargetingListMappingPartOfMappedSide[tuningOptions];
  primesTuningMapPartOfJustSide = getTargetingListPrimesTuningMapPartOfJustSide[tuningOptions];
  weightingPartOfMappedSide = getTargetingListWeightingPartOfMappedSide[tuningOptions];
  
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
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  damageWeightingSlope = tuningOption[tuningOptions, "damageWeightingSlope"]; (* trait 2 *)
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

(* only used by optimizeGeneratorsTuningMapNumericalPower(Limit)Solver *)
(* returns g *)
getTargetingListGeneratorsTuningMap[tuningOptions_] := Module[
  {t, tuningMappings, generatorsTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  
  generatorsTuningMap
];

(* returns MTW *)
getTargetingListMappedSide[tuningOptions_] := Module[
  {t, targetedIntervalsA, tuningMappings, ma, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  
  tuningMappings = getTuningMappings[t];
  ma = Part[tuningMappings, 2];
  
  damageWeights = getDamageWeights[tuningOptions];
  
  ma.Transpose[targetedIntervalsA].damageWeights
];

(* returns pTW *)
getTargetingListJustSide[tuningOptions_] := Module[
  {t, targetedIntervalsA, tuningMappings, primesTuningMap, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  
  tuningMappings = getTuningMappings[t];
  primesTuningMap = Part[tuningMappings, 4];
  
  damageWeights = getDamageWeights[tuningOptions];
  
  primesTuningMap.Transpose[targetedIntervalsA].damageWeights
];

(* only used by optimizeGeneratorsTuningMapMinisum *)
getTargetingListIntervalsPartOfMappedSide[tuningOptions_] := Module[
  {targetedIntervalsA, damageWeights},
  
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  
  Transpose[targetedIntervalsA]
];
getTargetingListWeightingPartOfMappedSide[tuningOptions_] := Module[
  {},
  getDamageWeights[tuningOptions]
];
getTargetingListMappingPartOfMappedSide[tuningOptions_] := Module[
  {t, tuningMappings},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  
  Part[tuningMappings, 2]
];
getTargetingListPrimesTuningMapPartOfJustSide[tuningOptions_] := Module[
  {t, tuningMappings},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  
  Part[tuningMappings, 4]
];
