getDualPower[power_] := If[power == 1, \[Infinity], 1 / (1 - 1 / power)];

(* compare with getDamageWeights *)
getDualMultiplier[tuningOptions_] := Module[
  {
    t,
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    
    complexityMultiplierAndLogPrimeCoordinationA
  },
  
  t = tuningOption[tuningOptions, "t"];
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  (* when computing tunings (as opposed to complexities directly), complexity-make-odd is handled through constraints *)
  complexityMakeOdd = False; (* trait 4d *)
  
  complexityMultiplierAndLogPrimeCoordinationA = getComplexityMultiplierAndLogPrimeCoordinationA[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  (* always essentially simplicity weighted *)
  tuningInverse[complexityMultiplierAndLogPrimeCoordinationA]
];

(* compare with getParts *)
getTargetingAllParts[tuningOptions_] := Module[
  {
    t,
    complexityNormPower,
    complexitySizeFactor,
    debug,
    
    generatorsTuningMap,
    ma,
    logPrimeCoordinationAndSummationMap,
    
    dualMultiplier,
    primesErrorMagnitudeNormPower,
    
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
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  debug = tuningOption[tuningOptions, "debug"];
  
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap} = getTuningMappings[t];
  
  dualMultiplier = getDualMultiplier[tuningOptions];
  primesErrorMagnitudeNormPower = getDualPower[complexityNormPower];
  
  justSideMappingPart = getPrimesIdentityA[t];
  eitherSideIntervalsPart = Transpose[getPrimesIdentityA[t]];
  powerPart = primesErrorMagnitudeNormPower;
  periodsPerOctavePart = getPeriodsPerOctave[t];
  
  If[
    complexitySizeFactor != 0,
    
    AppendTo[generatorsTuningMap, Symbol["gW"]];
    
    ma = Map[Join[#, {0}]&, ma];
    AppendTo[ma, Join[Table[complexitySizeFactor, Last[Dimensions[ma]] - 1].getLogPrimeCoordinationA[t], {-1}]];
    
    AppendTo[logPrimeCoordinationAndSummationMap, 0];
    
    justSideMappingPart = basicComplexitySizeFactorAugmentation[justSideMappingPart];
    
    eitherSideIntervalsPart = basicComplexitySizeFactorAugmentation[eitherSideIntervalsPart];
    
    dualMultiplier = basicComplexitySizeFactorAugmentation[dualMultiplier];
  ];
  
  temperedSideGeneratorsPart = {generatorsTuningMap};
  temperedSideMappingPart = ma;
  justSideGeneratorsPart = {logPrimeCoordinationAndSummationMap};
  eitherSideMultiplierPart = dualMultiplier;
  
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
    eitherSideIntervalsPart, (* I *)
    eitherSideMultiplierPart, (* X⁻¹ *)
    powerPart,
    periodsPerOctavePart
  }
];

basicComplexitySizeFactorAugmentation[a_] := Module[
  {augmentedA},
  
  augmentedA = Map[Join[#, {0}]&, a];
  AppendTo[augmentedA, Join[Table[0, Last[Dimensions[a]]], {1}]];
  
  augmentedA
]
