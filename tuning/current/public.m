(*
  
  GENERATORS PREIMAGE TRANSVERSAL
  
  
  getGeneratorsPreimageTransversal[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns a generators preimage transversal 
  (for each generator, one JI interval that maps to it).
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        getGeneratorsPreimageTransversal[meantoneM]
    
  Out   {{{1, 0, 0}, {-1, 1, 0}}, "contra"}
  
*)
getGeneratorsPreimageTransversal[t_] := Module[{ma, decomp, left, snf, right, generatorsPreimageTransversal},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  generatorsPreimageTransversal = right.Transpose[snf].left;
  
  {Transpose[generatorsPreimageTransversal], "contra"}
];


(*
  
  TUNING
  
  
  optimizeGeneratorsTuningMap[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimum generator tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "systematicTuningName" -> "minisos-NEC"]
    
  Out   {1198.24, 695.294}
*)
Options[optimizeGeneratorsTuningMap] = tuningOptions;
optimizeGeneratorsTuningMap[t_, OptionsPattern[]] := Module[
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
    optimizedGeneratorsTuningMap,
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA
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
  
  forDamage = False;
  
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
  
  tPossiblyWithChangedIntervalBasis = Part[tuningOptions, 1];
  targetedIntervalsA = Part[tuningOptions, 3];
  pureOctaveStretch = Part[tuningOptions, 11];
  
  optimizedGeneratorsTuningMap = 1200 * If[
    Length[targetedIntervalsA] == 0,
    
    (* covers targeting-all (includes 
    minimax-S "TOP", minimax-ES "TE", minimax-NES "Frobenius", 
    minimax-PNS "BOP", minimax-PNES "BE", 
    minimax-ZS "Weil", minimax-ZES "WE", minimax-QZS "Kees", minimax-QZES "KE", 
    unchanged-octave minimax-ES "CTE", 
    pure-octave-stretched minimax-S "POTOP", pure-octave-stretched minimax-ES "POTE") *)
    optimizeGeneratorsTuningMapTargetingAll[tuningOptions],
    
    (* covers targeting-list (includes 
    unchanged-octave diamond minimax-U "minimax", unchanged-octave diamond minisos-U "least squares") *)
    optimizeGeneratorsTuningMapTargetingList[tuningOptions]
  ];
  
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningIntervalBasis == "primes",
    optimizedGeneratorsTuningMap = retrievePrimesIntervalBasisGeneratorsTuningMap[optimizedGeneratorsTuningMap, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    pureOctaveStretch,
    optimizedGeneratorsTuningMap = getPureOctaveStretchedGeneratorsTuningMap[optimizedGeneratorsTuningMap, t]
  ];
  
  SetAccuracy[N[optimizedGeneratorsTuningMap], outputPrecision]
];


(*
  optimizeTuningMap[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimum tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 1899.26, 2790.25}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 1899.26, 2790.25} 
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "systematicTuningName" -> "minisos-NEC"]
    
  Out   {1198.24, 1893.54, 2781.18} 
*)
Options[optimizeTuningMap] = tuningOptions;
optimizeTuningMap[t_, OptionsPattern[]] := Module[
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
    debug
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
  
  optimizeGeneratorsTuningMap[t, {
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
  }].getA[getM[t]]
];

(*
  getGeneratorsTuningMapDamage[t, generatorsTuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning method, 
  returns how much damage this tuning map causes this temperament using this tuning method.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        quarterCommaGeneratorsTuningMap = {1200, 696.578};
        getGeneratorsTuningMapDamage[meantoneM, quarterCommaGeneratorsTuningMap, "systematicTuningName" -> "minimax-S"]
    
  Out   3.39251 
*)
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

(*
  getTuningMapDamage[t, tuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning method, 
  returns how much damage this tuning map causes this temperament using this tuning method.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        quarterCommaTuningMap = {1200, 1896.578, 2786.314};
        getTuningMapDamage[meantoneM, quarterCommaTuningMap, "systematicTuningName" -> "minimax-S"]
    
  Out   3.39236
*)
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

(*
  plotDamage[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  plots the damage to the targeted intervals within a close range around the optimum tuning.
  Plots as a 2D graph for a rank-1 temperament, a 3D graph for a rank-2 temperament, and errors otherwise.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        plotDamage[meantoneM, "systematicTuningName" -> "minisos-NEC"]
    
  Out   (3D graph)
  
  In    12etM = {{{12, 19, 28}, "co"};
        plotDamage[meantoneM, "systematicTuningName" -> "minisos-NEC"]
        
  Out   (2D graph)
*)
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
  
  {generatorsTuningMap, ma, primesTuningMap} = getTuningMappings[t];
  
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

(*
  generatorsTuningMapFromTAndTuningMap[t, tuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map, returns the generators tuning map.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        quarterCommaTuningMap = {1200, 1896.578, 2786.314};
        generatorsTuningMapFromTAndTuningMap[meantoneM, quarterCommaTuningMap]
    
  Out   {1200, 696.578};
*)
generatorsTuningMapFromTAndTuningMap[t_, tuningMap_] := Module[
  {generatorsTuningMap, ma, primesTuningMap, solution},
  
  {generatorsTuningMap, ma, primesTuningMap} = getTuningMappings[t];
  
  solution = NMinimize[Norm[generatorsTuningMap.ma - tuningMap], generatorsTuningMap];
  
  generatorsTuningMap /. Last[solution]
];
