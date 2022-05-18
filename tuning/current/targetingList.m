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
  {
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minimax"]];
  
  temperedSideGeneratorsPart = getTargetingListTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingListTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingListJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingListJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingListEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingListEitherSideMultiplierPart[tuningOptions];
  
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

(* no historically described tunings use this *)
optimizeGeneratorsTuningMapMinisum[tuningOptions_] := Module[
  {
    t,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart,
    
    optimumGeneratorsTuningMap,
    
    optimizationPower
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisum"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  temperedSideGeneratorsPart = getTargetingListTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingListTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingListJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingListJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingListEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingListEitherSideMultiplierPart[tuningOptions];
  
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
    optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[
      optimizationPower,
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

(* covers unchanged-octave diamond minisos-U "least squares" *)
optimizeGeneratorsTuningMapMinisos[tuningOptions_] := Module[
  {
    t,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisos"]];
  
  t = tuningOption[tuningOptions, "t"];
  
  temperedSideGeneratorsPart = getTargetingListTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingListTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingListJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingListJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingListEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingListEitherSideMultiplierPart[tuningOptions];
  
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

(* no historically described tunings use this *)
optimizeGeneratorsTuningMapMinisop[tuningOptions_] := Module[
  {
    optimizationPower,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisop"]];
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  
  temperedSideGeneratorsPart = getTargetingListTemperedSideGeneratorsPart[tuningOptions];
  temperedSideMappingPart = getTargetingListTemperedSideMappingPart[tuningOptions];
  justSideGeneratorsPart = getTargetingListJustSideGeneratorsPart[tuningOptions];
  justSideMappingPart = getTargetingListJustSideMappingPart[tuningOptions];
  eitherSideIntervalsPart = getTargetingListEitherSideIntervalsPart[tuningOptions];
  eitherSideMultiplierPart = getTargetingListEitherSideMultiplierPart[tuningOptions];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[
    optimizationPower,
    tuningOptions,
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
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
    
    tuningInverse[damageWeights],
    
    damageWeights
  ]
];

(* returns g *)
getTargetingListTemperedSideGeneratorsPart[tuningOptions_] := Module[
  {t, tuningMappings, generatorsTuningMap},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  
  {generatorsTuningMap}
];

(* returns M *)
getTargetingListTemperedSideMappingPart[tuningOptions_] := Module[
  {t, tuningMappings},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  
  Part[tuningMappings, 2]
];

(* returns p *)
getTargetingListJustSideGeneratorsPart[tuningOptions_] := Module[
  {t, tuningMappings},
  
  t = tuningOption[tuningOptions, "t"];
  
  tuningMappings = getTuningMappings[t];
  
  {Part[tuningMappings, 4]}
];

(* returns I *)
getTargetingListJustSideMappingPart[tuningOptions_] := Module[
  {t},
  
  t = tuningOption[tuningOptions, "t"];
  
  getPrimesIdentityA[t]
];

(* returns T *)
getTargetingListEitherSideIntervalsPart[tuningOptions_] := Module[
  {targetedIntervalsA},
  
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  
  Transpose[targetedIntervalsA]
];

(* returns W *)
getTargetingListEitherSideMultiplierPart[tuningOptions_] := Module[
  {},
  
  getDamageWeights[tuningOptions]
];
