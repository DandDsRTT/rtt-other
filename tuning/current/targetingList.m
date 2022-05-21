(* covers targeting-list (includes 
unchanged-octave diamond minimax-U "minimax", unchanged-octave diamond minisos-U "least squares") *)
optimizeGeneratorsTuningMapTargetingList[tuningOptions_] := Module[
  {optimizationPower, unchangedIntervals},
  
  If[tuningOption[tuningOptions, "debug"], Print["targeting-list"]];
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  
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
  {parts},
  
  If[tuningOption[tuningOptions, "debug"], Print["minimax"]];
  
  parts = getParts[tuningOptions];
  
  optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[parts]
];

(* no historically described tunings use this *)
optimizeGeneratorsTuningMapMinisum[tuningOptions_] := Module[
  {
    parts,
    
    optimumGeneratorsTuningMap,
    
    t,
    optimizationPower,
    unchangedIntervals,
    
    periodsPerOctave
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisum"]];
  
  parts = getParts[tuningOptions];
  
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMapAnalyticalSumPolytope[parts];
  
  (* if the solution from the sum polytope is non-unique, it returns null, so we fall back to a power-limit solution *)
  If[
    optimumGeneratorsTuningMap === Null,
    
    If[tuningOption[tuningOptions, "debug"], Print["non-unique solution \[RightArrow] power limit solver"]];
    
    t = tuningOption[tuningOptions, "t"];
    optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
    unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
    
    periodsPerOctave = getPeriodsPerOctave[t];
    
    optimizeGeneratorsTuningMapNumericalPowerLimitSolver[
      parts,
      optimizationPower,
      unchangedIntervals,
      periodsPerOctave
    ],
    
    optimumGeneratorsTuningMap
  ]
];

(* covers unchanged-octave diamond minisos-U "least squares" *)
optimizeGeneratorsTuningMapMinisos[tuningOptions_] := Module[
  {parts},
  
  If[tuningOption[tuningOptions, "debug"], Print["minisos"]];
  
  parts = getParts[tuningOptions];
  
  optimizeGeneratorsTuningMapAnalyticalMagPseudoinverse[parts]
];

(* no historically described tunings use this *)
optimizeGeneratorsTuningMapMinisop[tuningOptions_] := Module[
  {
    parts,
    
    t,
    optimizationPower,
    unchangedIntervals,
    
    periodsPerOctave
  },
  
  If[tuningOption[tuningOptions, "debug"], Print["minisop"]];
  
  parts = getParts[tuningOptions];
  
  t = tuningOption[tuningOptions, "t"];
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  optimizeGeneratorsTuningMapNumericalPowerSolver[
    parts,
    optimizationPower,
    unchangedIntervals,
    periodsPerOctave
  ]
];

(* compare with getDualMultiplier *)
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

getParts[tuningOptions_] := Module[
  {
    t,
    targetedIntervalsA,
    
    generatorsTuningMap,
    ma,
    logPrimeCoordinationAndSummationMap,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  },
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap} = getTuningMappings[t];
  
  temperedSideGeneratorsPart = {generatorsTuningMap};
  temperedSideMappingPart = ma;
  justSideGeneratorsPart = {logPrimeCoordinationAndSummationMap};
  justSideMappingPart = getPrimesIdentityA[t];
  eitherSideIntervalsPart = Transpose[targetedIntervalsA];
  eitherSideMultiplierPart = getDamageWeights[tuningOptions];
  
  {
    temperedSideGeneratorsPart, (* g *)
    temperedSideMappingPart, (* M *)
    justSideGeneratorsPart, (* p *)
    justSideMappingPart, (* I *)
    eitherSideIntervalsPart, (* T *)
    eitherSideMultiplierPart (* W *)
  }
];
