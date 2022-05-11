(* DAMAGE *)

Options[getGeneratorsTuningMapDamage] = tuningOptions;
getGeneratorsTuningMapDamage[t_, generatorsTuningMap_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait 9 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage,
    tuningOptions,
    tuningMap
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  forDamage = True;
  
  tuningOptions = processTuningOptions[
    t,
    unchangedIntervals, (* trait 9 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage
  ];
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  
  tuningMap = generatorsTuningMap.getA[getM[t]] / 1200;
  
  1200 * If[
    optimizationPower == \[Infinity],
    getMaxDamage[tuningMap, tuningOptions],
    If[
      optimizationPower == 2,
      get2SumDamage[tuningMap, tuningOptions],
      If[
        optimizationPower == 1,
        getSumDamage[tuningMap, tuningOptions],
        getPowerSumDamage[tuningMap, tuningOptions, optimizationPower]
      ]
    ]
  ]
];

Options[getTuningMapDamage] = tuningOptions;
getTuningMapDamage[t_, tuningMap_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait 9 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    generatorsTuningMap
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  generatorsTuningMap = generatorsTuningMapFromTAndTuningMap[t, tuningMap];
  
  getGeneratorsTuningMapDamage[t, generatorsTuningMap, {
    "unchangedIntervals" -> unchangedIntervals, (* trait 9 *)
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> complexityNegateLogPrimeCoordination, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityMakeOdd" -> complexityMakeOdd, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis, (* trait 8 *)
    "pureOctaveStretch" -> pureOctaveStretch, (* trait 10 *)
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "systematicComplexityName" -> systematicComplexityName,
    "originalComplexityName" -> originalComplexityName,
    "debug" -> debug
  }]
];

getTargetedIntervalDamagesL[tuningMap_, tuningOptions_] := Module[
  {t, targetedIntervalsA, primesTuningMap, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  primesTuningMap = getPrimesTuningMap[t];
  damageWeights = getDamageWeights[tuningOptions];
  
  Abs[
    N[
      Map[If[PossibleZeroQ[#], 0, #]&, tuningMap - primesTuningMap],
      absoluteValuePrecision
    ].Transpose[targetedIntervalsA].damageWeights
  ]
];

Square[n_] := n^2;

getSumDamage[tuningMap_, tuningOptions_] := Total[getTargetedIntervalDamagesL[tuningMap, tuningOptions]];
get2SumDamage[tuningMap_, tuningOptions_] := Total[Square[getTargetedIntervalDamagesL[tuningMap, tuningOptions]]];
getPowerSumDamage[tuningMap_, tuningOptions_, power_] := Total[Power[getTargetedIntervalDamagesL[tuningMap, tuningOptions], power]];
getMaxDamage[tuningMap_, tuningOptions_] := Max[getTargetedIntervalDamagesL[tuningMap, tuningOptions]];


(* COMPLEXITY *)

getComplexity[
  pcv_,
  t_,
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[{complexityMultiplier},
  complexityMultiplier = getComplexityMultiplier[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  Norm[complexityMultiplier.pcv, complexityNormPower]
];

(* Note that we don't actually use any of these functions directly; they're just around to test understanding *)
getPcvCopfrComplexity[pcv_, t_] := Total[Map[If[Abs[# > 0], 1, 0]&, pcv]];
(* AKA "Benedetti height" *)
getPcvProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getIntervalBasis[t], pcv}];
(* AKA "Tenney height" *)
getPcvLogProductComplexity[pcv_, t_] := Log2[getPcvProductComplexity[pcv, t]];
(* AKA "Wilson height", can also be used to find minimax-PNS ("BOP") tuning *)
getPcvSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getIntervalBasis[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find minimax-S ("TOP") tuning *)
getPcvLogSopfrComplexity[pcv_, t_] := Log2[getPcvSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getPcvIntegerLimitComplexity[pcv_, t_] := Module[{quotient},
  quotient = pcvToQuotient[pcv];
  Max[Numerator[quotient], Denominator[quotient]]
];
(* AKA "logarithmic Weil height", used for minimax-ZS ("Weil") tuning *)
getPcvLogIntegerLimitComplexity[pcv_, t_] := Log2[getPcvIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
removePowersOfTwoFromPcv[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getPcvOddLimitComplexity[pcv_, t_] := getPcvIntegerLimitComplexity[removePowersOfTwoFromPcv[pcv], t];
(* AKA "Kees expressibility", used for minimax-QZS ("Kees") tuning *)
getPcvLogOddLimitComplexity[pcv_, t_] := Log2[getPcvOddLimitComplexity[pcv, t]];

(* This is different than the damageWeights, this is nested within it;
this is to weight the quantities of the PC-vectors before taking a norm and getting an interval complexity, 
which are then all taken for each interval and assembled as damageWeights *)
getComplexityMultiplier[
  t_,
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[{complexityMultiplier},
  (* When used by getPrimeAbsErrorCounterweights for optimizeGeneratorsTuningMapPrimesMaximumNorm, covers minimax-S ("TOP"); 
when used by getPrimeAbsErrorCounterweights for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-ES ("TE"); 
when used by getDamageWeights covers any targeting-list tuning using this as its damage's complexity *)
  complexityMultiplier = getLogPrimeCoordinationA[t];
  
  If[
    (* When used by getPrimeAbsErrorCounterweights for optimizeGeneratorsTuningMapPrimesMaximumNorm, covers minimax-NS (the L1 version of "Frobenius");
    when used by getPrimeAbsErrorCounterweights for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-NES ("Frobenius") *)
    complexityNegateLogPrimeCoordination == True,
    complexityMultiplier = complexityMultiplier.Inverse[getLogPrimeCoordinationA[t]]
  ];
  
  If[
    (* When used by getPrimeAbsErrorCounterweights for optimizeGeneratorsTuningMapPrimesMaximumNorm, covers minimax-PNS ("BOP");
    when used by getPrimeAbsErrorCounterweights for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-PNES ("BE") *)
    complexityPrimePower > 0,
    complexityMultiplier = complexityMultiplier.DiagonalMatrix[Power[getIntervalBasis[t], complexityPrimePower]]
  ];
  
  If[
    (* When minimax-ZS ("Weil") needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the max minus min thing); 
    when used by getPrimeAbsErrorCounterweights for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-ZES ("WE") or minimax-QZES ("KE")
    (surprisingly minimax-QZES does not use the below; it instead uses this and applies an unchanged octave constraint); 
    when used by getDamageWeights should cover any targeting-list tuning using this as its damage's complexity *)
    complexitySizeFactor > 0,
    complexityMultiplier = (Join[getPrimesIdentityA[t], {Table[complexitySizeFactor, getD[t]]}] / (1 + complexitySizeFactor)).complexityMultiplier
  ];
  
  If[
    (* When minimax-QZS ("Kees") needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the max minus min thing) with pure-octave constraint on the solver; 
    note again that this is is not used for minimax-QZES ("KE"); see note above;
    when used by getDamageWeights should cover any targeting-list tuning using this as its damage's complexity *)
    complexityMakeOdd == True,
    complexityMultiplier = complexityMultiplier.DiagonalMatrix[Join[{0}, Table[1, getD[t] - 1]]]
  ];
  
  complexityMultiplier
];


(* PLOTTING *)

Options[plotDamage] = tuningOptions;
plotDamage[t_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait 9 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage,
    
    tuningOptions,
    
    tWithPossiblyChangedIntervalBasis,
    targetedIntervalsA,
    
    tuningMappings,
    generatorsTuningMap,
    ma,
    primesTuningMap,
    
    normPower,
    plotArgs,
    targetedIntervalGraphs,
    r
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMap[t, {
    "unchangedIntervals" -> unchangedIntervals, (* trait 9 *)
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> complexityNegateLogPrimeCoordination, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityMakeOdd" -> complexityMakeOdd, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis, (* trait 8 *)
    "pureOctaveStretch" -> pureOctaveStretch, (* trait 10 *)
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "systematicComplexityName" -> systematicComplexityName,
    "originalComplexityName" -> originalComplexityName,
    "debug" -> debug
  }];
  
  forDamage = True;
  
  tuningOptions = processTuningOptions[
    t,
    unchangedIntervals, (* trait 9 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage
  ];
  
  tWithPossiblyChangedIntervalBasis = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = tuningOption[tuningOptions, "damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"]; (* trait 4d *)
  
  tuningMappings = getTuningMappings[tWithPossiblyChangedIntervalBasis];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  primesTuningMap = Part[tuningMappings, 4];
  
  plotArgs = {};
  
  (* data *)
  targetedIntervalGraphs = Map[
    Function[
      {targetedIntervalPcv},
      
      Abs[generatorsTuningMap.ma.targetedIntervalPcv - 1200 * primesTuningMap.targetedIntervalPcv] / getComplexity[
        targetedIntervalPcv,
        tWithPossiblyChangedIntervalBasis,
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ],
    targetedIntervalsA
  ];
  normPower = If[
    optimizationPower == \[Infinity] && damageWeightingSlope == "simplicityWeighted" && Length[targetedIntervals] == 0,
    dualPower[complexityNormPower],
    optimizationPower
  ];
  AppendTo[plotArgs, Norm[targetedIntervalGraphs, normPower]];
  
  (* range *)
  MapIndexed[AppendTo[plotArgs, {Part[generatorsTuningMap, First[#2]], #1 - 10, #1 + 10}]&, optimumGeneratorsTuningMap];
  
  (* settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  
  (* plot type *)
  r = getR[tWithPossiblyChangedIntervalBasis];
  If[
    r == 1,
    Apply[Plot, plotArgs],
    If[
      r == 2,
      Apply[Plot3D, plotArgs],
      Throw["4D and higher visualizations not supported"]
    ]
  ]
];
