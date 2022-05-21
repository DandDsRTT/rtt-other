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
    optimizationPower,
    debug,
    
    generatorsTuningMap,
    ma,
    logPrimeCoordinationAndSummationMap,
    
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart,
    powerPart,
    periodsPerOctavePart
  },
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  debug = tuningOption[tuningOptions, "debug"];
  
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap} = getTuningMappings[t];
  
  temperedSideGeneratorsPart = {generatorsTuningMap};
  temperedSideMappingPart = ma;
  justSideGeneratorsPart = {logPrimeCoordinationAndSummationMap};
  justSideMappingPart = getPrimesIdentityA[t];
  eitherSideIntervalsPart = Transpose[targetedIntervalsA];
  eitherSideMultiplierPart = getDamageWeights[tuningOptions];
  powerPart = optimizationPower;
  periodsPerOctavePart = getPeriodsPerOctave[t];
  
  If[
    debug == True,
    Print["temperedSideGeneratorsPart: ", temperedSideGeneratorsPart]; (* g *)
    Print["temperedSideMappingPart: ", temperedSideMappingPart]; (* M *)
    Print["justSideGeneratorsPart: ", justSideGeneratorsPart]; (* p *)
    Print["justSideMappingPart: ", justSideMappingPart]; (* I *)
    Print["eitherSideIntervalsPart: ", eitherSideIntervalsPart]; (* I *)
    Print["eitherSideMultiplierPart: ", eitherSideMultiplierPart]; (* X⁻¹ *)
    Print["powerPart: ", powerPart];
    Print["periodsPerOctavePart: ", periodsPerOctavePart];
  ];
  
  {
    temperedSideGeneratorsPart, (* g *)
    temperedSideMappingPart, (* M *)
    justSideGeneratorsPart, (* p *)
    justSideMappingPart, (* I *)
    eitherSideIntervalsPart, (* T *)
    eitherSideMultiplierPart, (* W *)
    powerPart,
    periodsPerOctavePart
  }
];
