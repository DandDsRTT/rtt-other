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
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage,
    tuningOptions,
    optimizedGeneratorsTuningMap,
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA,
    parts,
    powerPart,
    solution
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  forDamage = False;
  
  tuningOptions = processTuningOptions[
    t,
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage
  ];
  
  tPossiblyWithChangedIntervalBasis = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"]; (* trait 4c *)
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = tuningOption[tuningOptions, "pureOctaveStretch"]; (* trait 10 *)
  
  parts = If[
    Length[targetedIntervalsA] == 0,
    getTargetingAllParts[tuningOptions],
    getParts[tuningOptions]
  ];
  
  powerPart = part[parts, "powerPart"];
  
  solution = If[
    Length[unchangedIntervals] > 0,
    
    (* covers minimax-QZES "KE", unchanged-octave minimax-ES "CTE" *)
    If[debug, Print["power solver"]];
    powerSumSolution[parts, unchangedIntervals],
    
    If[
      powerPart == 2,
      
      (* covers unchanged-octave diamond minisos-U "least squares", 
      minimax-ES "TE", minimax-NES "Frobenius", pure-octave-stretched minimax-ES "POTE", 
      minimax-ZES "WE", minimax-PNES "BE" *)
      If[debug, Print["pseudoinverse"]];
      pseudoinverseSolution[parts, unchangedIntervals],
      
      If[
        powerPart == \[Infinity],
        
        (* covers unchanged-octave diamond minimax-U "minimax", 
        minimax-S "TOP", pure-octave-stretched minimax-S "POTOP", 
        minimax-PNS "BOP", minimax-ZS "Weil", minimax-QZS "Kees" *)
        If[debug, Print["max polytope"]];
        maxPolytopeSolution[parts, unchangedIntervals],
        
        If[
          powerPart == 1,
          
          (* no historically described tunings use this *)
          If[debug, Print["sum polytope"]];
          sumPolytopeSolution[parts, unchangedIntervals],
          
          (* no historically described tunings go here *)
          powerSumSolution[parts, unchangedIntervals]
        ]
      ]
    ]
  ];
  
  If[
    solution == Null,
    If[debug, Print["power limit solver"]];
    solution = powerSumLimitSolution[parts, unchangedIntervals]
  ];
  
  optimizedGeneratorsTuningMap = 1200 * solution;
  
  If[
    Length[targetedIntervalsA] == 0 && complexitySizeFactor != 0,
    optimizedGeneratorsTuningMap = Drop[optimizedGeneratorsTuningMap, -1]
  ];
  
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningIntervalBasis == "primes",
    optimizedGeneratorsTuningMap = retrievePrimesIntervalBasisGeneratorsTuningMap[optimizedGeneratorsTuningMap, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    pureOctaveStretch,
    optimizedGeneratorsTuningMap = getPureOctaveStretchedGeneratorsTuningMap[optimizedGeneratorsTuningMap, parts]
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
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  optimizeGeneratorsTuningMap[t, {
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> complexityNegateLogPrimeCoordination, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityMakeOdd" -> complexityMakeOdd, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis, (* trait 8 *)
    "unchangedIntervals" -> unchangedIntervals, (* trait 9 *)
    "pureOctaveStretch" -> pureOctaveStretch, (* trait 10 *)
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "systematicDamageName" -> systematicDamageName,
    "originalDamageName" -> originalDamageName,
    "systematicComplexityName" -> systematicComplexityName,
    "originalComplexityName" -> originalComplexityName,
    "debug" -> debug
  }].getA[getM[t]]
];

(*
  getGeneratorsTuningMapDamageMean[t, generatorsTuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning method, 
  returns how much damage this tuning map causes this temperament using this tuning method.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        quarterCommaGeneratorsTuningMap = {1200, 696.578};
        getGeneratorsTuningMapDamageMean[meantoneM, quarterCommaGeneratorsTuningMap, "systematicTuningName" -> "minimax-S"]
    
  Out   3.39251 
*)
Options[getGeneratorsTuningMapDamageMean] = tuningOptions;
getGeneratorsTuningMapDamageMean[t_, generatorsTuningMap_, OptionsPattern[]] := Module[
  {
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    tuningMap
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  tuningMap = generatorsTuningMap.getA[getM[t]];
  
  getTuningMapDamageMean[t, tuningMap, {
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> complexityNegateLogPrimeCoordination, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityMakeOdd" -> complexityMakeOdd, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis, (* trait 8 *)
    "unchangedIntervals" -> unchangedIntervals, (* trait 9 *)
    "pureOctaveStretch" -> pureOctaveStretch, (* trait 10 *)
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "systematicDamageName" -> systematicDamageName,
    "originalDamageName" -> originalDamageName,
    "systematicComplexityName" -> systematicComplexityName,
    "originalComplexityName" -> originalComplexityName,
    "debug" -> debug
  }]
];

(*
  getTuningMapDamageMean[t, tuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning method, 
  returns how much damage this tuning map causes this temperament using this tuning method.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        quarterCommaTuningMap = {1200, 1896.578, 2786.314};
        getTuningMapDamageMean[meantoneM, quarterCommaTuningMap, "systematicTuningName" -> "minimax-S"]
    
  Out   3.39236
*)
Options[getTuningMapDamageMean] = tuningOptions;
getTuningMapDamageMean[t_, tuningMap_, OptionsPattern[]] := Module[
  {
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage,
    tuningOptions,
    targetedIntervalsA,
    parts
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  forDamage = True;
  
  tuningOptions = processTuningOptions[
    t,
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage
  ];
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  
  parts = If[
    Length[targetedIntervalsA] == 0,
    getTargetingAllParts[tuningOptions],
    getParts[tuningOptions]
  ];
  (* set the temperedSideGeneratorsPart to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  parts[[1]] = {tuningMap / 1200};
  (* override the other half of the temperedSideMappingPart too, since we have the whole tuning map already *)
  parts[[2]] = IdentityMatrix[getD[t]];
  
  SetAccuracy[N[1200 * getPowerMeanAbsError[parts]], outputPrecision]
];

(* TODO: it would be cool if this did a nicer job of printing them out, like, telling you which targeted interval has which damages you know? *)
(*
  getGeneratorsTuningMapDamages[t, generatorsTuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning method, 
  returns the damages to each of the targeted intervals.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        quarterCommaGeneratorsTuningMap = {1200, 696.578};
        getGeneratorsTuningMapDamages[meantoneM, quarterCommaGeneratorsTuningMap, "systematicTuningName" -> "minimax-S"]
    
  Out   {} TODO: get this answer 
*)
Options[getGeneratorsTuningMapDamages] = tuningOptions;
getGeneratorsTuningMapDamages[t_, generatorsTuningMap_, OptionsPattern[]] := Module[
  {
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    tuningMap
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  tuningMap = generatorsTuningMap.getA[getM[t]];
  
  getTuningMapDamages[t, tuningMap, {
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> complexityNegateLogPrimeCoordination, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityMakeOdd" -> complexityMakeOdd, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis, (* trait 8 *)
    "unchangedIntervals" -> unchangedIntervals, (* trait 9 *)
    "pureOctaveStretch" -> pureOctaveStretch, (* trait 10 *)
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "systematicDamageName" -> systematicDamageName,
    "originalDamageName" -> originalDamageName,
    "systematicComplexityName" -> systematicComplexityName,
    "originalComplexityName" -> originalComplexityName,
    "debug" -> debug
  }]
];

(*
  getTuningMapDamages[t, tuningMap]
  
  Given a representation of a temperament as a mapping or comma basis,
  plus a tuning map for that temperament, and a tuning method, 
  returns the damages to each of the targeted intervals.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        quarterCommaTuningMap = {1200, 1896.578, 2786.314};
        getTuningMapDamages[meantoneM, quarterCommaTuningMap, "systematicTuningName" -> "minimax-S"]
    
  Out   {} TODO: get this answer
*)
Options[getTuningMapDamages] = tuningOptions;
getTuningMapDamages[t_, tuningMap_, OptionsPattern[]] := Module[
  {
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage,
    tuningOptions,
    targetedIntervalsA,
    parts
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  forDamage = True;
  
  tuningOptions = processTuningOptions[
    t,
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage
  ];
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"]; (* trait 0 *)
  
  parts = If[
    Length[targetedIntervalsA] == 0,
    getTargetingAllParts[tuningOptions],
    getParts[tuningOptions]
  ];
  (* set the temperedSideGeneratorsPart to the input tuningMap, in octaves, in the structure getAbsErrors needs it, 
  since getPowerMeanAbsError shares it with other methods *)
  parts[[1]] = {tuningMap / 1200};
  (* override the other half of the temperedSideMappingPart too, since we have the whole tuning map already *)
  parts[[2]] = IdentityMatrix[getD[t]];
  
  SetAccuracy[N[1200 * getAbsErrors[parts]], outputPrecision]
];
(* TODO: obviously this is super wet... 
there should be some way to get the damage mean ones to just do these but then do the total outside...
and also it really hammers home how obnoxious these argument lists are... maybe wolfram language has a better way
*)

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
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    forDamage,
    
    tuningOptions,
    
    tWithPossiblyChangedIntervalBasis,
    targetedIntervalsA,
    
    generatorsTuningMap,
    ma,
    logPrimeCoordinationAndSummationMap,
    
    normPower,
    plotArgs,
    targetedIntervalGraphs,
    r,
    plotStyle,
    image
  },
  
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"]; (* trait 8 *)
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait 9 *)
  pureOctaveStretch = OptionValue["pureOctaveStretch"]; (* trait 10 *)
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicDamageName = OptionValue["systematicDamageName"];
  originalDamageName = OptionValue["originalDamageName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  debug = OptionValue["debug"];
  
  optimumGeneratorsTuningMap = optimizeGeneratorsTuningMap[t, {
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> complexityNegateLogPrimeCoordination, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityMakeOdd" -> complexityMakeOdd, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis, (* trait 8 *)
    "unchangedIntervals" -> unchangedIntervals, (* trait 9 *)
    "pureOctaveStretch" -> pureOctaveStretch, (* trait 10 *)
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "systematicDamageName" -> systematicDamageName,
    "originalDamageName" -> originalDamageName,
    "systematicComplexityName" -> systematicComplexityName,
    "originalComplexityName" -> originalComplexityName,
    "debug" -> debug
  }];
  
  forDamage = True;
  
  tuningOptions = processTuningOptions[
    t,
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
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
  
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap} = getTuningMappings[t];
  
  plotArgs = {};
  
  (* data *)
  targetedIntervalGraphs = Map[
    Function[
      {targetedIntervalPcv},
      
      Abs[generatorsTuningMap.ma.targetedIntervalPcv - 1200 * logPrimeCoordinationAndSummationMap.targetedIntervalPcv] / getComplexity[
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
  (* Print["targetedIntervalGraphs", targetedIntervalGraphs]; *)
  normPower = If[
    optimizationPower == \[Infinity] && damageWeightingSlope == "simplicityWeighted" && Length[targetedIntervals] == 0,
    getDualPower[complexityNormPower],
    optimizationPower
  ];
  (* AppendTo[plotArgs, {targetedIntervalGraphs, Norm[targetedIntervalGraphs, normPower]}];*)
  
  image = Image[
    Map[
      Map[
        If[
          # == 1,
          {0, 0, 0, 1},
          {0, 0, 0, 0}
        ]&,
        #
      ]&,
      Array[(-1)^+ ## &, {32, 32}]
    ],
    ColorSpace -> "RGB"
  ];
  image = ImageResize[image, 256, Resampling -> "Constant"];
  
  (* Image@Array[(-1)^+## &, {64, 64}];*)
  (*image = Graphics@{PatternFilling["Checkerboard", ImageScaled[1/10]](*,   Rectangle[]*)}*)
  (* SetAlphaChannel[White];*)
  AppendTo[plotArgs, {targetedIntervalGraphs, (* Norm[targetedIntervalGraphs, Infinity],*) Norm[targetedIntervalGraphs, \[Infinity]] + 0.001(*, Norm[targetedIntervalGraphs, 2]^2*)(* Norm[targetedIntervalGraphs, 1]*)(*, sumLines*)}];
  plotStyle = Join[Table[Auto, Length[targetedIntervalGraphs]], {{Texture[image](*,PatternFilling["Checkerboard"]*)}(*{Black, Dotted},  {Black, Dashed},  {Black}*)}];
  (* Print[plotStyle];*)
  
  (* range *)
  MapIndexed[AppendTo[plotArgs, {Part[generatorsTuningMap, First[#2]], #1 - 2, #1 + 2}]&, optimumGeneratorsTuningMap];
  
  (* settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  AppendTo[plotArgs, PlotStyle -> plotStyle];
  AppendTo[plotArgs, MaxRecursion -> 6];
  
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
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap, solution},
  
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap} = getTuningMappings[t];
  
  solution = NMinimize[Norm[generatorsTuningMap.ma - tuningMap], generatorsTuningMap];
  
  generatorsTuningMap /. Last[solution]
];




(* ___ PRIVATE ___ *)



(* TUNING OPTIONS *)

outputPrecision = 4;
linearSolvePrecision = 8;
nMinimizePrecision = 128;
absoluteValuePrecision = nMinimizePrecision * 2;

tuningOptions = {
  "targetedIntervals" -> Null, (* trait 0 *)
  "optimizationPower" -> Null, (* trait 1: \[Infinity] = minimax, 2 = minisos, 1 = minisum *)
  "damageWeightingSlope" -> "", (* trait 2: unweighted, complexityWeighted, or simplicityWeighted *)
  "complexityNormPower" -> 1, (* trait 3: what Mike Battaglia refers to as `p` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space *)
  "complexityNegateLogPrimeCoordination" -> False, (* trait 4a: False = do nothing, True = negate the multiplication by logs of primes *)
  "complexityPrimePower" -> 0, (* trait 4b: what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when log prime coordination is negated and otherwise defaults; 1 = product complexity, equiv to sopfr when log prime coordination is negated and otherwise defaults; >1 = pth power of those *)
  "complexitySizeFactor" -> 0, (* trait 4c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = could be integer limit, etc. *)
  "complexityMakeOdd" -> False, (* trait 4d: False = do nothing, True = achieve odd limit from integer limit, etc. *)
  "tuningIntervalBasis" -> "primes", (* trait 8: Graham Breed calls this "inharmonic" vs "subgroup" notion in the context of minimax-ES ("TE") tuning, but it can be used for any tuning *)
  "unchangedIntervals" -> {}, (* trait 9 *)
  "pureOctaveStretch" -> False, (* trait 10 *)
  "systematicTuningName" -> "",
  "originalTuningName" -> "",
  "systematicDamageName" -> "",
  "originalDamageName" -> "",
  "systematicComplexityName" -> "",
  "originalComplexityName" -> "",
  "debug" -> False
};

processTuningOptions[
  t_,
  inputTargetedIntervals_, (* trait 0 *)
  inputOptimizationPower_, (* trait 1 *)
  inputDamageWeightingSlope_, (* trait 2 *)
  inputComplexityNormPower_, (* trait 3 *)
  inputComplexityNegateLogPrimeCoordination_, (* trait 4a *)
  inputComplexityPrimePower_, (* trait 4b *)
  inputComplexitySizeFactor_, (* trait 4c *)
  inputComplexityMakeOdd_, (* trait 4d *)
  inputTuningIntervalBasis_, (* trait 8 *)
  inputUnchangedIntervals_, (* trait 9 *)
  inputPureOctaveStretch_, (* trait 10 *)
  inputSystematicTuningName_,
  inputOriginalTuningName_,
  inputSystematicDamageName_,
  inputOriginalDamageName_,
  inputSystematicComplexityName_,
  inputOriginalComplexityName_,
  inputDebug_,
  forDamage_ : False
] := Module[
  {
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis, (* trait 8 *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    systematicTuningName,
    originalTuningName,
    systematicDamageName,
    originalDamageName,
    systematicComplexityName,
    originalComplexityName,
    debug,
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    intervalBasis,
    intervalRebase
  },
  
  targetedIntervals = inputTargetedIntervals; (* trait 0 *)
  optimizationPower = inputOptimizationPower; (* trait 1 *)
  damageWeightingSlope = inputDamageWeightingSlope; (* trait 2 *)
  complexityNormPower = inputComplexityNormPower; (* trait 3 *)
  complexityNegateLogPrimeCoordination = inputComplexityNegateLogPrimeCoordination; (* trait 4a *)
  complexityPrimePower = inputComplexityPrimePower; (* trait 4b *)
  complexitySizeFactor = inputComplexitySizeFactor; (* trait 4c *)
  complexityMakeOdd = inputComplexityMakeOdd; (* trait 4d *)
  tuningIntervalBasis = inputTuningIntervalBasis; (* trait 8 *)
  unchangedIntervals = inputUnchangedIntervals; (* trait 9 *)
  pureOctaveStretch = inputPureOctaveStretch; (* trait 10 *)
  systematicTuningName = inputSystematicTuningName;
  originalTuningName = inputOriginalTuningName;
  systematicDamageName = inputSystematicDamageName;
  originalDamageName = inputOriginalDamageName;
  systematicComplexityName = inputSystematicComplexityName;
  originalComplexityName = inputOriginalComplexityName;
  debug = inputDebug;
  
  If[
    originalTuningName === "minimax",
    optimizationPower = \[Infinity]; damageWeightingSlope = "unweighted"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
  ];
  If[
    originalTuningName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
  ];
  If[
    originalTuningName === "TOP" || originalTuningName === "TIPTOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    originalTuningName === "TE" || originalTuningName === "Tenney-Euclidean" || originalTuningName === "TOP-RMS",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E";
  ];
  If[
    originalTuningName === "Frobenius",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "NE";
  ];
  If[
    originalTuningName === "BOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "PN";
  ];
  If[
    originalTuningName === "BE" || originalTuningName === "Benedetti-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "PNE";
  ];
  If[
    originalTuningName === "Weil",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";systematicComplexityName = "Z";
  ];
  If[
    originalTuningName === "WE" || originalTuningName === "Weil-Euclidean",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "ZE";
  ];
  If[
    originalTuningName === "Kees",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "QZ";
  ];
  If[
    originalTuningName === "KE" || originalTuningName === "Kees-Euclidean",
    (* Note how this tuning works by enforcing an unchanged octave via a solver constraint, rather than through the complexity units multiplier *)
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "ZE"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
  ];
  If[
    originalTuningName === "POTOP" || originalTuningName === "POTT",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "POTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "CTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "E"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
  ];
  
  If[
    originalDamageName === "topDamage",
    damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  
  (* Note: we can't implement product complexity with the current design, and don't intend to revise.
   This is because product complexity is realized from a PC-vector as a product of terms,
    raised to the powers of the absolute values of the entries. But this design only multiplies entries and sums them. 
    Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    originalComplexityName === "copfr" || originalComplexityName === "l1Norm",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "sopfr" || originalComplexityName === "wilsonHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 1; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "integerLimit" || originalComplexityName === "weilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "oddLimit" || originalComplexityName === "keesHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = True; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "logProduct" || originalComplexityName === "tenneyHeight" || originalComplexityName === "harmonicDistance",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = False; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logIntegerLimit" || originalComplexityName === "logarithmicWeilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logOddLimit" || originalComplexityName === "keesExpressibility",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "rososcopfr" || originalComplexityName === "l2Norm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 0; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "rosossopfr",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 0; complexityPrimePower = 1; complexityMakeOdd = False;
  ];
  (* (following the pattern here, this one might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ]; *)
  (* (following the pattern here, this one might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ]; *)
  If[
    originalComplexityName === "tenneyEuclideanHeight",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "weilEuclideanNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "keesEuclideanSeminorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = False; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    originalComplexityName === "carlsNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = True; complexitySizeFactor = 0; complexityPrimePower = 2; complexityMakeOdd = False;
  ];
  
  (* trait 0 *)
  If[
    StringMatchQ[systematicTuningName, "*targeting-all*"] || (StringMatchQ[systematicTuningName, "*minimax*"] && StringMatchQ[systematicTuningName, "*S*"]),
    targetedIntervals = {};
  ];
  If[
    StringMatchQ[systematicTuningName, "*diamond*"],
    targetedIntervals = "diamond";
  ];
  
  (* trait 1 *)
  If[
    StringMatchQ[systematicTuningName, "*minimax*"],
    optimizationPower = \[Infinity];
  ];
  If[
    StringMatchQ[systematicTuningName, "*minisos*"],
    optimizationPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningName, "*minisum*"],
    optimizationPower = 1;
  ];
  
  (* trait 2 *)
  If[
    StringMatchQ[systematicTuningName, "*S*"] || StringMatchQ[systematicDamageName, "*S*"],
    damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningName, "*C*"] || StringMatchQ[systematicDamageName, "*C*"],
    damageWeightingSlope = "complexityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningName, "*U*"] || StringMatchQ[systematicDamageName, "*U*"],
    damageWeightingSlope = "unweighted";
  ];
  
  (* trait 3 - same as complexity systematic name parts *)
  If[
    StringMatchQ[systematicTuningName, "*E*"] || StringMatchQ[systematicDamageName, "*E*"] || StringMatchQ[systematicComplexityName, "*E*"],
    complexityNormPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningName, "*T*"] || StringMatchQ[systematicDamageName, "*T*"] || StringMatchQ[systematicComplexityName, "*T*"],
    complexityNormPower = 1;
  ];
  
  (* trait 4 - same as complexity systematic name parts  *)
  If[
    StringMatchQ[systematicTuningName, "*N*"] || StringMatchQ[systematicDamageName, "*N*"] || StringMatchQ[systematicComplexityName, "*N*"],
    complexityNegateLogPrimeCoordination = True;
  ];
  If[
    StringMatchQ[systematicTuningName, "*P*"] || StringMatchQ[systematicDamageName, "*P*"] || StringMatchQ[systematicComplexityName, "*P*"],
    complexityPrimePower = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Z*"] || StringMatchQ[systematicDamageName, "*Z*"] || StringMatchQ[systematicComplexityName, "*Z*"],
    complexitySizeFactor = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Q*"] || StringMatchQ[systematicDamageName, "*Q*"] || StringMatchQ[systematicComplexityName, "*Q*"],
    complexityMakeOdd = True;
  ];
  
  (* trait 8 - interval basis *)
  If[
    StringMatchQ[systematicTuningName, "*formal-primes-basis*"],
    tuningIntervalBasis = "primes";
  ];
  
  (* trait 9 - unchanged intervals *)
  If[
    StringMatchQ[systematicTuningName, "*unchanged-octave*"],
    unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
  ];
  
  (* trait 10 - pure-octave stretch *)
  If[
    StringMatchQ[systematicTuningName, "*pure-octave-stretched*"],
    pureOctaveStretch = True;
  ];
  
  (* complexityMakeOdd is enough to get odd limit complexity from integer limit complexity, 
  but when actually solving for tunings, it's necessary to lock down prime 2 (the octave) as an unchanged interval. *)
  If[complexityMakeOdd == True, unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]}];
  
  (* This has to go below the systematic tuning name gating, so that targetedIntervals has a change to be set to {} *)
  intervalBasis = getIntervalBasis[t];
  If[
    !isStandardPrimeLimitIntervalBasis[intervalBasis] && tuningIntervalBasis == "primes",
    
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getIntervalBasisDimension[intervalBasis]];
    commaBasisInPrimeLimitIntervalBasis = changeIntervalBasis[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    intervalRebase = getIntervalRebaseForC[intervalBasis, primeLimitIntervalBasis];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    targetedIntervalsA = If[
      targetedIntervals === Null,
      Throw["no targeted intervals"],
      If[
        ToString[targetedIntervals] == "{}",
        If[
          forDamage,
          getFormalPrimesA[tPossiblyWithChangedIntervalBasis],
          {}
        ],
        If[
          ToString[targetedIntervals] == "diamond",
          getDiamond[getD[tPossiblyWithChangedIntervalBasis]],
          intervalRebase.getA[targetedIntervals]
        ]
      ]
    ],
    
    tPossiblyWithChangedIntervalBasis = t;
    targetedIntervalsA = If[
      targetedIntervals === Null,
      Throw["no targeted intervals"],
      If[
        ToString[targetedIntervals] == "{}",
        If[
          forDamage,
          getFormalPrimesA[tPossiblyWithChangedIntervalBasis],
          {}
        ],
        If[
          ToString[targetedIntervals] == "diamond",
          getDiamond[getD[tPossiblyWithChangedIntervalBasis]],
          getA[targetedIntervals]
        ]
      ]
    ]
  ];
  
  If[
    !NumericQ[optimizationPower] && optimizationPower != \[Infinity],
    Throw["no optimization power"]
  ];
  If[
    damageWeightingSlope == "",
    Throw["no damage weighting slope"]
  ];
  If[
    Length[targetedIntervalsA] == 0 && optimizationPower != \[Infinity],
    Throw["It is not possible to optimize for minisum or minisos over all intervals, only minimax."]
  ];
  If[
    Length[targetedIntervalsA] == 0 && damageWeightingSlope != "simplicityWeighted",
    Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weighted."]
  ];
  
  If[
    debug == True,
    Print["tPossiblyWithChangedIntervalBasis: ", tPossiblyWithChangedIntervalBasis];
    Print["targetedIntervalsA: ", targetedIntervalsA]; (* trait 0 *)
    Print["optimizationPower: ", optimizationPower]; (* trait 1 *)
    Print["damageWeightingSlope: ", damageWeightingSlope]; (* trait 2 *)
    Print["complexityNormPower: ", complexityNormPower]; (* trait 3 *)
    Print["complexityNegateLogPrimeCoordination: ", complexityNegateLogPrimeCoordination]; (* trait 4a *)
    Print["complexityPrimePower: ", complexityPrimePower]; (* trait 4b *)
    Print["complexitySizeFactor: ", complexitySizeFactor]; (* trait 4c *)
    Print["complexityMakeOdd: ", complexityMakeOdd]; (* trait 4d *)
    Print["unchangedIntervals: ", unchangedIntervals]; (* trait 9 *)
    Print["pureOctaveStretch: ", pureOctaveStretch]; (* trait 10 *)
  ];
  
  {
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    unchangedIntervals, (* trait 9 *)
    pureOctaveStretch, (* trait 10 *)
    debug
  }
];

tuningOptionsPartsByOptionName = <|
  "t" -> 1,
  "targetedIntervalsA" -> 2, (* trait 0 *)
  "optimizationPower" -> 3, (* trait 1 *)
  "damageWeightingSlope" -> 4, (* trait 2 *)
  "complexityNormPower" -> 5, (* trait 3 *)
  "complexityNegateLogPrimeCoordination" -> 6, (* trait 4a *)
  "complexityPrimePower" -> 7, (* trait 4b *)
  "complexitySizeFactor" -> 8, (* trait 4c *)
  "complexityMakeOdd" -> 9, (* trait 4d *)
  "unchangedIntervals" -> 10, (* trait 9 *)
  "pureOctaveStretch" -> 11, (* trait 10 *)
  "debug" -> 12
|>;
tuningOption[tuningOptions_, optionName_] := Part[tuningOptions, tuningOptionsPartsByOptionName[optionName]];


(* PARTS *)

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
    Print["temperedSideGeneratorsPart: ", temperedSideGeneratorsPart // MatrixForm, N[temperedSideGeneratorsPart] // MatrixForm]; (* g *)
    Print["temperedSideMappingPart: ", temperedSideMappingPart // MatrixForm, N[temperedSideMappingPart] // MatrixForm]; (* M *)
    Print["justSideGeneratorsPart: ", justSideGeneratorsPart // MatrixForm, N[justSideGeneratorsPart] // MatrixForm]; (* p *)
    Print["justSideMappingPart: ", justSideMappingPart // MatrixForm, N[justSideMappingPart] // MatrixForm]; (* I *)
    Print["eitherSideIntervalsPart: ", eitherSideIntervalsPart // MatrixForm, N[eitherSideIntervalsPart] // MatrixForm]; (* T *)
    Print["eitherSideMultiplierPart: ", eitherSideMultiplierPart // MatrixForm, N[eitherSideMultiplierPart] // MatrixForm]; (* W *)
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

partsPartsByPartName = <|
  "temperedSideGeneratorsPart" -> 1,
  "temperedSideMappingPart" -> 2,
  "justSideGeneratorsPart" -> 3,
  "justSideMappingPart" -> 4,
  "eitherSideIntervalsPart" -> 5,
  "eitherSideMultiplierPart" -> 6,
  "powerPart" -> 7,
  "periodsPerOctavePart" -> 8
|>;
part[parts_, partName_] := Part[parts, partsPartsByPartName[partName]];


(* SHARED *)

getSummationMap[t_] := Table[1, getD[t]];

getLogPrimeCoordinationA[t_] := DiagonalMatrix[Log2[getIntervalBasis[t]]];

getLogPrimeCoordinationAndSummationMap[t_] := getSummationMap[t].getLogPrimeCoordinationA[t];

getPeriodsPerOctave[t_] := First[First[getA[getM[t]]]];

getPrimesIdentityA[t_] := IdentityMatrix[getD[t]];

getTuningMappings[t_] := Module[
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap},
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  logPrimeCoordinationAndSummationMap = getLogPrimeCoordinationAndSummationMap[t];
  
  {generatorsTuningMap, ma, logPrimeCoordinationAndSummationMap}
];

tuningInverse[damageWeighterOrComplexityMultiplier_] := MapThread[
  Function[
    {dataRow, zerosRow},
    MapIndexed[
      Function[
        {zerosEl, index},
        zerosEl + If[
          First[index] > Length[dataRow],
          0,
          Part[dataRow, First[index]]
        ]
      ],
      zerosRow
    ]
  ],
  {
    (* note: this is pseudo not because of non-square, due to complexity size factor,
    but because of when complexity is odd and the top-left entry is a 0 so det is 0 so it's singular *)
    PseudoInverse[
      damageWeighterOrComplexityMultiplier[[1 ;; Last[Dimensions[damageWeighterOrComplexityMultiplier]]]]
    ],
    Table[
      Table[
        0,
        First[Dimensions[damageWeighterOrComplexityMultiplier]]
      ],
      Last[Dimensions[damageWeighterOrComplexityMultiplier]]
    ]
  }
];


(* DAMAGE *)

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

(* ERROR *)

getPowerSumAbsError[parts_] := If[
  part[parts, "powerPart"] == \[Infinity],
  
  (* I thought it would be fine, but apparently Wolfram Language thinks the infinity-power-sum is "indeterminate" *)
  Max[getAbsErrors[parts]],
  
  Total[Power[getAbsErrors[parts], part[parts, "powerPart"]]]
];
getPowerNormAbsError[parts_] := Norm[getAbsErrors[parts], part[parts, "powerPart"]];
getPowerMeanAbsError[parts_] := Module[
  {absErrors, powerPart, targetedIntervalCount, result},
  
  absErrors = getAbsErrors[parts];
  powerPart = part[parts, "powerPart"];
  targetedIntervalCount = Last[Dimensions[part[parts, "eitherSideIntervalsPart"]]]; (* k *)
  
  (* Print["absErrors: ", SetAccuracy[N[absErrors * 1200], outputPrecision]]; *)
  (* Print["absErrors: ", absErrors * 1200]; *) (* TODO: the above with the accuracy setting was causing us one of those malformed real problems *)
  (* Print[SetAccuracy[N[part[parts, "justSideGeneratorsPart"]* 1200], outputPrecision]]; *)
  (* Print[SetAccuracy[N[part[parts, "temperedSideGeneratorsPart"]* 1200], outputPrecision]]; *)
  
  result = If[
    powerPart == \[Infinity],
    
    (* again, I thought it'd be fine, but Wolfram Language thinks the infinity-power-sum is "indeterminate" *)
    Max[absErrors],
    
    Power[
      Total[Power[
        absErrors,
        powerPart
      ]] / targetedIntervalCount,
      1 / powerPart
    ]
  ];
  
  result
];

(* returns errors in octaves *)
getAbsErrors[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}] := Module[
  {temperedSide, justSide},
  
  temperedSide = First[getSide[temperedSideGeneratorsPart, temperedSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart]];
  justSide = First[getSide[justSideGeneratorsPart, justSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart]];
  
  (* Print[SetAccuracy[N[1200*temperedSide], outputPrecision]]; *)
  (* Print[SetAccuracy[N[1200*justSide], outputPrecision]]; *)
  
  Abs[N[
    Map[
      If[Quiet[PossibleZeroQ[#]], 0, #]&,
      temperedSide - justSide
    ],
    absoluteValuePrecision
  ]]
];

(* COMPLEXITY *)

(* returns complexities in weighted octaves *)
getComplexity[
  pcv_,
  t_,
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {complexityMultiplierAndLogPrimeCoordinationA},
  
  complexityMultiplierAndLogPrimeCoordinationA = getComplexityMultiplierAndLogPrimeCoordinationA[
    t,
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  Norm[complexityMultiplierAndLogPrimeCoordinationA.pcv, complexityNormPower] / (1 + complexitySizeFactor)
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
  (* When used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesMaximumNorm, covers minimax-S ("TOP"); 
when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-ES ("TE"); 
when used by getDamageWeights covers any targeting-list tuning using this as its damage's complexity *)
  complexityMultiplier = IdentityMatrix[getD[t]];
  
  If[
    (* When used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesMaximumNorm, covers minimax-NS (the L1 version of "Frobenius");
    when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-NES ("Frobenius") *)
    complexityNegateLogPrimeCoordination == True,
    complexityMultiplier = complexityMultiplier.Inverse[getLogPrimeCoordinationA[t]]
  ];
  
  If[
    (* When used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesMaximumNorm, covers minimax-PNS ("BOP");
    when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-PNES ("BE") *)
    complexityPrimePower > 0,
    complexityMultiplier = complexityMultiplier.DiagonalMatrix[Power[getIntervalBasis[t], complexityPrimePower]]
  ];
  
  If[
    (* When minimax-ZS ("Weil") needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the max minus min thing); 
    when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers minimax-ZES ("WE") or minimax-QZES ("KE")
    (surprisingly minimax-QZES does not use the below; it instead uses this and applies an unchanged octave constraint); 
    when used by getDamageWeights should cover any targeting-list tuning using this as its damage's complexity *)
    complexitySizeFactor > 0,
    complexityMultiplier = Join[getPrimesIdentityA[t], {Table[complexitySizeFactor, getD[t]]}].complexityMultiplier
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

getComplexityMultiplierAndLogPrimeCoordinationA[
  t_,
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := getComplexityMultiplier[
  t,
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd (* trait 4d *)
].getLogPrimeCoordinationA[t];


(* TARGETING-ALL *)

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
    
    AppendTo[generatorsTuningMap, Symbol["gAugmented"]];
    
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
    Print["temperedSideGeneratorsPart: ", temperedSideGeneratorsPart // MatrixForm, N[temperedSideGeneratorsPart] // MatrixForm]; (* g *)
    Print["temperedSideMappingPart: ", temperedSideMappingPart // MatrixForm, N[temperedSideMappingPart] // MatrixForm]; (* M *)
    Print["justSideGeneratorsPart: ", justSideGeneratorsPart // MatrixForm, N[justSideGeneratorsPart] // MatrixForm]; (* p *)
    Print["justSideMappingPart: ", justSideMappingPart // MatrixForm, N[justSideMappingPart] // MatrixForm]; (* I *)
    Print["eitherSideIntervalsPart: ", eitherSideIntervalsPart // MatrixForm, N[eitherSideIntervalsPart] // MatrixForm]; (* I *)
    Print["eitherSideMultiplierPart: ", eitherSideMultiplierPart // MatrixForm, N[eitherSideMultiplierPart] // MatrixForm]; (* X⁻¹ *)
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
];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGeneratorsTuningMap[optimizedGeneratorsTuningMap_, originalT_, t_] := Module[
  {ma, optimizedTuningMap, generatorsPreimageTransversal, f},
  
  ma = getA[getM[t]];
  optimizedTuningMap = optimizedGeneratorsTuningMap.ma;
  generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[originalT]]];
  f = Transpose[getFormalPrimesA[originalT]];
  
  optimizedTuningMap.f.generatorsPreimageTransversal
];


(* PURE-OCTAVE STRETCH *)

getPureOctaveStretchedGeneratorsTuningMap[optimizedGeneratorsTuningMap_, parts_] := Module[
  {periodsPerOctavePart},
  
  periodsPerOctavePart = part[parts, "periodsPerOctavePart"];
  
  (1200 / periodsPerOctavePart) * (optimizedGeneratorsTuningMap / First[optimizedGeneratorsTuningMap])
];


(* TARGETED INTERVAL SETS *)

getDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  (* for when you want the tonality diamond to be in the natural order for a 5-limit diamond, 
  as when developing pedagogical materials and using this library, 
  because it normally doesn't end up getting them in the natural order
  {{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}} *)
  
  padVectorsWithZerosUpToD[Map[quotientToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;


(* SOLUTIONS: OPTIMIZATION POWER = \[Infinity] (MINIMAX) OR COMPLEXITY NORM POWER = 1 LEADING TO DUAL NORM POWER \[Infinity] ON PRIMES (MAX NORM) *)

(* covers unchanged-octave diamond minimax-U "minimax", minimax-S "TOP", pure-octave-stretched minimax-S "POTOP", 
minimax-PNS "BOP", minimax-ZS "Weil", minimax-QZS "Kees" *)
(* a semi-analytical solution *)
(* based on https://github.com/keenanpepper/tiptop/blob/main/tiptop.py *)
maxPolytopeSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervals_] := Module[
  {
    temperedSideMinusGeneratorsPart,
    justSide,
    
    generatorCount,
    maxCountOfNestedMinimaxibleDamages,
    minimaxTunings,
    minimaxLockForTemperedSide,
    minimaxLockForJustSide,
    undoMinimaxLocksForTemperedSide,
    undoMinimaxLocksForJustSide,
    uniqueOptimumTuning
  },
  
  (* the mapped and weighted targeted intervals on one side, and the just and weighted targeted intervals on the other;
  note that just side goes all the way down to tuning map level (logs of primes), including the generators
  while the tempered side isn't tuned, but merely mapped. that's so we can solve for the rest of it, 
  i.e. the generators AKA its tunings *)
  temperedSideMinusGeneratorsPart = Transpose[temperedSideMappingPart.eitherSideIntervalsPart.eitherSideMultiplierPart];
  justSide = Transpose[getSide[justSideGeneratorsPart, justSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart]];
  
  (* our goal is to find the generator tuning map not merely with minimaxed damage, 
  but where the next-highest damage is minimaxed as well, and in fact every next-highest damage is minimaxed, all the way down.
  the tuning which has all damages minimaxed within minimaxed all the way down like this we can call a "nested-minimax".
  it's the only sensible optimum given a desire for minimax damage, so in general we can simply still call it "minimax".
  though people have sometimes distinguished this tuning from the range of minimax tunings with a prefix, 
  such as "TIPTOP tuning" versus "TOP tunings", although there is no value in "TOP tunings" given the existence of "TIPTOP",
  so you may as well just keep calling it "TOP" and refine its definition. anyway...
  
  the `findAllNestedMinimaxTuningsFromPolytopeVertices` function this function calls may come back with more than one result. 
  (sometimes it pulls off some nested-minimaxing on its own, but that's a really subtle point, and we won't worry about it here.)
  the clever way we compute a nested-minimax uses the same polytope vertex searching method used for that first pass, but now with a twist.
  so in the basic case, this method finds the vertices of a max polytope for a temperament.
  so now, instead of running it on the case of the original temperament versus JI, we run it on a distorted version of this case.
  specifically, we run it on a case distorted so that the previous minimaxes are locked down.
  
  we achieve this by picking one of these minimax tunings and offset the just side by it. 
  it doesn't matter which minimax tuning we choose, by the way; they're not sorted, and we simply take the first one.
  the corresponding distortion to the tempered side is trickier, 
  involving the differences between this arbitrarily-chosen minimax tuning and each of the other minimax tunings.
  note that after this distortion, the original rank and dimensionality of the temperament will no longer be recognizable.
  
  we then search for polytope vertices of this minimax-locked distorted situation.
  and we repeatedly do this until we eventually find a unique, nested-minimax optimum. 
  once we've done that, though, our result isn't in the form of a generators tuning map yet. it's still distorted.
  well, with each iteration, we've been keeping track of the distortion applied, so that in the end we could undo them all.
  after undoing those, voilà, we're done! *)
  
  (* the same as rank here, but named this for correlation with elsewhere in this code *)
  generatorCount = Last[Dimensions[temperedSideMinusGeneratorsPart]];
  
  (* this is too complicated to be explained here and will be explained later *)
  maxCountOfNestedMinimaxibleDamages = 0;
  
  (* the candidate generator tuning maps which minimax damage to the targets*)
  minimaxTunings = findAllNestedMinimaxTuningsFromPolytopeVertices[
    temperedSideMinusGeneratorsPart,
    justSide,
    maxCountOfNestedMinimaxibleDamages
  ];
  maxCountOfNestedMinimaxibleDamages = generatorCount + 1;
  
  (* no minimax-damage-locking transformations yet, so the transformation trackers are identities 
  per their respective operations of matrix multiplication and addition *)
  undoMinimaxLocksForTemperedSide = IdentityMatrix[generatorCount];
  undoMinimaxLocksForJustSide = Table[{0}, generatorCount];
  
  While[
    (* a unique optimum has not yet been found *)
    Length[minimaxTunings] > 1,
    
    (* arbitrarily pick one of the minimax damage generator tuning maps; the first one from this unsorted list *)
    minimaxLockForJustSide = First[minimaxTunings];
    (* list of differences between each other minimax generator tuning map and the first one; 
    note how the range starts on index 2 in order to skip the first one *)
    minimaxLockForTemperedSide = Map[Flatten, Transpose[Map[
      Part[minimaxTunings, #] - minimaxLockForJustSide&,
      Range[2, Length[minimaxTunings]]
    ]]];
    
    (* apply the minimax-damage-locking transformation to the just side, and track it to undo later *)
    justSide -= temperedSideMinusGeneratorsPart.minimaxLockForJustSide;
    undoMinimaxLocksForJustSide += undoMinimaxLocksForTemperedSide.minimaxLockForJustSide;
    
    (* apply the minimax-damage-locking transformation to the tempered side, and track it to undo later *)
    (* this would be a .= if Wolfram supported an analog to += and -= *)
    (* unlike how it is with the justSide, the undo operation is not inverted here; 
    that's because we essentially invert it in the end by left-multiplying rather than right-multiplying *)
    temperedSideMinusGeneratorsPart = temperedSideMinusGeneratorsPart.minimaxLockForTemperedSide;
    undoMinimaxLocksForTemperedSide = undoMinimaxLocksForTemperedSide.minimaxLockForTemperedSide;
    
    (* search again, now in this transformed state *)
    minimaxTunings = findAllNestedMinimaxTuningsFromPolytopeVertices[temperedSideMinusGeneratorsPart, justSide, maxCountOfNestedMinimaxibleDamages];
    maxCountOfNestedMinimaxibleDamages += generatorCount + 1;
  ];
  
  uniqueOptimumTuning = First[minimaxTunings];
  
  SetAccuracy[Flatten[
    (* here's that left-multiplication mentioned earlier *)
    undoMinimaxLocksForTemperedSide.uniqueOptimumTuning + undoMinimaxLocksForJustSide
  ], 10]
];

findAllNestedMinimaxTuningsFromPolytopeVertices[temperedSideMinusGeneratorsPart_, justSide_, maxCountOfNestedMinimaxibleDamages_] := Module[
  {
    targetCount,
    generatorCount,
    nthmostMinDamage,
    vertexConstraintAs,
    targetIndices,
    candidateTunings,
    sortedDamagesByCandidateTuning,
    candidateTuning,
    sortedDamagesForThisCandidateTuning,
    newCandidateTunings,
    newSortedDamagesByCandidateTuning
  },
  
  (* in the basic case where no minimax-damage-locking transformations have been applied, 
  these will be the same as the count of original targeted intervals and the rank of the temperament, respectively *)
  targetCount = First[Dimensions[temperedSideMinusGeneratorsPart]];
  generatorCount = Last[Dimensions[temperedSideMinusGeneratorsPart]];
  
  (* here's the meat of it: solving a linear problem for each vertex of the of tuning polytope;
  more details on this in the constraint matrix gathering function's comments below *)
  candidateTunings = {};
  vertexConstraintAs = getTuningPolytopeVertexConstraintAs[generatorCount, targetCount];
  Do[
    AppendTo[
      candidateTunings,
      Quiet[Check[
        LinearSolve[
          N[vertexConstraintA.temperedSideMinusGeneratorsPart, linearSolvePrecision],
          N[vertexConstraintA.justSide, linearSolvePrecision]
        ],
        "err"
      ]
      ]],
    {vertexConstraintA, vertexConstraintAs}
  ];
  
  (* each damages list is sorted in descending order; 
  the list of lists itself is sorted corresponding to the candidate tunings*)
  sortedDamagesByCandidateTuning = Quiet[Map[
    Function[
      {candidateTuning},
      If[
        ToString[candidateTuning] == "err",
        "err",
        ReverseSort[Abs[Flatten[fixUpZeros[temperedSideMinusGeneratorsPart.candidateTuning - justSide]]]]
      ]
    ],
    candidateTunings
  ]];
  (* MapThread[Print["constraint matrix: ", #1 // MatrixForm, " tuning: ", 1200 * #2 , " damages: ", 1200 * #3]&, {vertexConstraintAs, candidateTunings, sortedDamagesByCandidateTuning}]; *)
  (* ignore the problems that are singular and therefore have no solution *)
  candidateTunings = Select[candidateTunings, !TrueQ[# == "err"]&];
  sortedDamagesByCandidateTuning = Select[sortedDamagesByCandidateTuning, !TrueQ[# == "err"]&];
  
  (*     
  here we're iterating by index of the targeted intervals, 
  repeatedly updating the lists candidate tunings and their damages,
  (each pass the list gets shorter, hopefully eventually hitting length 1, at which point a unique tuning has been found,
  but this doesn't necessarily happen, and if it does, it's handled by the function that calls this function)
  until by the final pass they are what we want to return.
  
  there's an inner loop by candidate tuning, and since that list is shrinking each time, the size of the inner loop changes.
  in other words, we're not covering an m \[Times] n rectangular grid's worth of possibilities; more like a jagged triangle.
  
  note that because the damages have all been sorted in descending order,
  these target "indices" do not actually correspond to an individual targeted interval.
  that's okay though because here it's not important which target each of these damages is for.
  all that matters is the size of the damages.
  once we find the tuning we want, we can easily compute its damages list sorted by target when we need it later; that info is not lost.
  
  and note that we don't iterate over *every* target "index".
  we only check as many targets as we could possibly nested-minimax by this point.
  that's why this method doesn't simply always return a unique nested-minimax tuning each time.
  this is also why the damages have been sorted in this way
  so first we compare each tuning's actual minimum damage,
  then we compare each tuning's second-closest-to-minimum damage,
  then compare each third-closest-to-minimum, etc.
  the count of target indices we iterate over is a running total; 
  each time it is increased, it goes up by the present generator count plus 1.
  why it increases by that amount is a bit of a mystery to me, but perhaps someone can figure it out and let me know.
  *)
  targetIndices = Range[Min[maxCountOfNestedMinimaxibleDamages + generatorCount + 1, targetCount]];
  Do[
    newCandidateTunings = {};
    newSortedDamagesByCandidateTuning = {};
    
    (* this is the nth-most minimum damage across all candidate tunings,
    where the actual minimum is found in the 1st index, the 2nd-most minimum in the 2nd index,
    and we index it by target index *)
    nthmostMinDamage = Min[Map[Part[#, targetIndex]&, sortedDamagesByCandidateTuning]];
    
    Do[
      (* having found the minimum damage for this target index, we now iterate by candidate tuning index *)
      candidateTuning = Part[candidateTunings, minimaxTuningIndex];
      sortedDamagesForThisCandidateTuning = Part[sortedDamagesByCandidateTuning, minimaxTuningIndex];
      
      If[
        (* and if this is one of the tunings which is tied for this nth-most minimum damage,
        add it to the list of those that we'll check on the next iteration of the outer loop 
        (and add its damages to the corresponding list) 
        note the tiny tolerance factor added to accommodate computer arithmetic error problems *)
        Part[sortedDamagesForThisCandidateTuning, targetIndex] <= nthmostMinDamage + 0.000000001,
        
        AppendTo[newCandidateTunings, candidateTuning];
        AppendTo[newSortedDamagesByCandidateTuning, sortedDamagesForThisCandidateTuning]
      ],
      
      {minimaxTuningIndex, Range[Length[candidateTunings]]}
    ];
    
    candidateTunings = newCandidateTunings;
    sortedDamagesByCandidateTuning = newSortedDamagesByCandidateTuning,
    
    {targetIndex, targetIndices}
  ];
  
  (* if duplicates are not deleted, then when differences are checked between tunings,
  some will come out to all zeroes, and this causes a crash *)
  DeleteDuplicates[
    candidateTunings,
    Function[{tuningA, tuningB}, AllTrue[MapThread[#1 == #2&, {tuningA, tuningB}], TrueQ]]
  ]
];
fixUpZeros[l_] := Map[
  Function[
    {nestedList},
    Map[
      If[Quiet[PossibleZeroQ[#]], 0, SetAccuracy[#, linearSolvePrecision]]&, (* TODO: reconcile this with other PossibleZeroQ part *)
      nestedList
    ]
  ],
  l
];

getTuningPolytopeVertexConstraintAs[generatorCount_, targetCount_] := Module[
  {vertexConstraintA, vertexConstraintAs, targetCombinations, directionPermutations},
  
  vertexConstraintAs = {};
  
  (* here we iterate over every combination of r + 1 (rank = generator count, in the basic case) targets 
  and for each of those combinations, looks at all permutations of their directions. 
  these are the vertices of the maximum damage tuning polytope. each is a generator tuning map. the minimum of these will be the minimax tuning.
  
  e.g. for target intervals 3/2, 5/4, and 5/3, with 1 generator, we'd look at three combinations (3/2, 5/4) (3/2, 5/3) (5/4, 5/3)
  and for the first combination, we'd look at both 3/2 \[Times] 5/4 = 15/8 and 3/2 \[Divide] 5/4 = 6/5.
  
  then what we do with each of those combo perm vertices is build a constraint matrix. 
  we'll apply this constraint matrix to a typical linear equation of the form Ax = b, 
  where A is a matrix, b is a vector, and x is another vector, the one we're solving for.
  in our case our matrix A is M, our mapping, b is our just tuning map j, and x is our generators tuning map g.
  
  e.g. when the targets are just the primes (and thus an identity matrix we can ignore),
  and the temperament we're tuning is 12-ET with M = [12 19 28] and standard interval basis so p = [log₂2 log₂3 log₂5],
  then we have [12 19 28][g₁] = [log₂2 log₂3 log₂5], or a system of three equations:
  
  12g₁ = log₂2
  19g₁ = log₂3
  28g₁ = log₂5
  
  Obviously not all of those can be true, but that's the whole point: we linear solve for the closest possible g₁ that satisfies all well.
  
  Now suppose we get the constraint matrix [1 1 0]. We multiply both sides of the setup by that:
  
  [1 1 0][12 19 28][g₁] = [1 1 0][log₂2 log₂3 log₂5]
  [31][g₁] = [log₂2 + log₂3]
  
  This leaves us with only a single equation:
  
  31g₁ = log₂6
  
  Or in other words, this tuning makes 6/1 pure, and divides it into 31 equal parts.
  If this temperament's mapping says it's 12 steps to 2/1 and 19 steps to 3/1, and it takes 31 steps to a pure 6/1,
  that implies that whatever damage there is on 2/1 is equal to whatever damage there is on 3/1, since they apparently cancel out.
  
  This constraint matrix [1 1 0] means that the target combo was 2/1 and 3/1, 
  because those are the targets corresponding to its nonzero elements.
  And both nonzero elements are +1 meaning that both targets are combined in the same direction.
  If the targeted intervals list had been [3/2, 4/3, 5/4, 8/5, 5/3, 6/5] instead, and the constraint matrix [1 0 0 0 -1 0],
  then that's 3/2 \[Divide] 5/3 = 5/2.
  
  The reason why we only need half of the permutations is because we only need relative direction permutations;
  they're anchored with the first targeted interval always in the super direction.
  *)
  targetCombinations = DeleteDuplicates[Map[Sort, Select[Tuples[Range[1, targetCount], generatorCount + 1], DuplicateFreeQ[#]&]]];
  (* Print["targetCombinations: ", targetCombinations // MatrixForm];*)
  
  Do[
    (* note that these are only generatorCount, not generatorCount + 1, because whichever is the first one will always be +1 *)
    (*Print["  targetCombination: ", targetCombination // MatrixForm];*)
    
    directionPermutations = Tuples[{1, -1}, generatorCount];
    (*Print["  directionPermutations: ", directionPermutations // MatrixForm];*)
    
    Do[
      (*  Print["    directionPermutation: ", directionPermutation // MatrixForm];*)
      
      vertexConstraintA = Table[Table[0, targetCount], generatorCount];
      
      Do[
        vertexConstraintA[[generatorIndex, Part[targetCombination, 1]]] = 1;
        vertexConstraintA[[generatorIndex, Part[targetCombination, generatorIndex + 1]]] = Part[directionPermutation, generatorIndex],
        
        {generatorIndex, Range[generatorCount]}
      ];
      
      (*Print["      vertexConstraintA: ", vertexConstraintA // MatrixForm];*)
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {directionPermutation, directionPermutations}
    ],
    
    {targetCombination, targetCombinations}
  ];
  
  (* if there's only one generator, we also need to consider each tuning where a target is pure 
  (rather than tied for damage with another target) *)
  If[
    generatorCount == 1,
    Do[
      vertexConstraintA = {Table[0, targetCount]};
      vertexConstraintA[[1, targetIndex]] = 1;
      
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {targetIndex, Range[targetCount]}
    ]
  ];
  
  (* count should be the product of the indices count and the signs count, plus the r == 1 ones *)
  vertexConstraintAs
];


(* SOLUTIONS: OPTIMIZATION POWER = 1 (MINIMSUM) OR COMPLEXITY NORM POWER = \[Infinity] LEADING TO DUAL NORM POWER 1 ON PRIMES (TAXICAB NORM) *)

(* no historically described tunings use this *)
(* an analytical solution *)
(* based on https://en.xen.wiki/w/Target_tunings#Minimax_tuning, 
where unchanged-octave diamond minimax-U "minimax" is described;
however, this computation method is in general actually a solution for minisum tunings, not minimax tunings. 
it only lucks out and works for minimax due to the pure-octave-constraint 
and nature of the tonality diamond targeted interval set,
namely that the places where damage to targets are equal is the same where other targets are pure.
*)
sumPolytopeSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervals_] := Module[
  {
    generatorCount,
    
    unchangedIntervalSetIndices,
    candidateUnchangedIntervalSets,
    normalizedCandidateUnchangedIntervalSets,
    filteredNormalizedCandidateUnchangedIntervalSets,
    candidateOptimumGeneratorAs,
    candidateOptimumGeneratorsTuningMaps,
    candidateOptimumGeneratorTuningMapAbsErrors,
    
    optimumGeneratorsTuningMapIndices,
    optimumGeneratorsTuningMapIndex
  },
  
  generatorCount = First[Dimensions[temperedSideMappingPart]]; (* First[], not Last[], because it's not transposed here. *)
  
  unchangedIntervalSetIndices = Subsets[Range[Length[Transpose[eitherSideIntervalsPart]]], {generatorCount}];
  candidateUnchangedIntervalSets = Map[Map[Transpose[eitherSideIntervalsPart][[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedCandidateUnchangedIntervalSets = Map[canonicalCa, candidateUnchangedIntervalSets];
  filteredNormalizedCandidateUnchangedIntervalSets = DeleteDuplicates[Select[normalizedCandidateUnchangedIntervalSets, MatrixRank[#] == generatorCount&]];
  candidateOptimumGeneratorAs = Select[Map[
    getGeneratorsAFromUnchangedIntervals[temperedSideMappingPart, #]&,
    filteredNormalizedCandidateUnchangedIntervalSets
  ], Not[# === Null]&];
  candidateOptimumGeneratorsTuningMaps = Map[justSideGeneratorsPart.#&, candidateOptimumGeneratorAs]; (* TODO: not really tuning maps because not in cents *)
  candidateOptimumGeneratorTuningMapAbsErrors = Map[
    Total[getAbsErrors[{
      #, (* note: this is an override; only reason these parts are unpacked *)
      temperedSideMappingPart,
      justSideGeneratorsPart,
      justSideMappingPart,
      eitherSideIntervalsPart,
      eitherSideMultiplierPart,
      powerPart,
      periodsPerOctavePart
    }]]&,
    candidateOptimumGeneratorsTuningMaps
  ];
  
  (* Print["candidateUnchangedIntervalSets: ", candidateUnchangedIntervalSets // MatrixForm];
  Print["normalizedCandidateUnchangedIntervalSets: ", normalizedCandidateUnchangedIntervalSets // MatrixForm];
  Print["filteredNormalizedCandidateUnchangedIntervalSets: ", filteredNormalizedCandidateUnchangedIntervalSets // MatrixForm];
  Print["candidateOptimumGeneratorAs: ", Map[Transpose, candidateOptimumGeneratorAs] // MatrixForm];
  Print["candidateOptimumGeneratorsTuningMaps: ", 1200 * N[candidateOptimumGeneratorsTuningMaps] // MatrixForm];
  Print["candidateOptimumGeneratorTuningMapAbsErrors: ", 1200 * N[candidateOptimumGeneratorTuningMapAbsErrors] // MatrixForm]; *)
  
  optimumGeneratorsTuningMapIndices = Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]];
  If[
    Length[optimumGeneratorsTuningMapIndices] == 1,
    
    (* result is unique; done *)
    optimumGeneratorsTuningMapIndex = First[First[Position[candidateOptimumGeneratorTuningMapAbsErrors, Min[candidateOptimumGeneratorTuningMapAbsErrors]]]];
    First[candidateOptimumGeneratorsTuningMaps[[optimumGeneratorsTuningMapIndex]]],
    
    (* result is non-unique, will need to handle otherwise *)
    Null
  ]
];

getGeneratorsAFromUnchangedIntervals[ma_, unchangedIntervalEigenvectors_] := Module[
  {mappedUnchangedIntervalEigenvectors},
  
  mappedUnchangedIntervalEigenvectors = ma.Transpose[unchangedIntervalEigenvectors];
  
  If[
    Det[mappedUnchangedIntervalEigenvectors] == 0,
    Null,
    Transpose[unchangedIntervalEigenvectors].Inverse[mappedUnchangedIntervalEigenvectors]
  ]
];


(* SOLUTIONS: OPTIMIZATION POWER = 2 (MINISOS) OR COMPLEXITY NORM POWER = 2 LEADING TO DUAL NORM POWER 2 ON PRIMES (EUCLIDEAN NORM) *)

(* an analytical solution *)
(* covers unchanged-octave diamond minisos-U "least squares", minimax-ES "TE", pure-octave-stretched minimax-ES "POTE",
minimax-NES "Frobenius", minimax-ZES "WE", minimax-PNES "BE" *)
pseudoinverseSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervals_] := Module[
  {temperedSideMinusGeneratorsPart, justSide},
  
  temperedSideMinusGeneratorsPart = temperedSideMappingPart.eitherSideIntervalsPart.eitherSideMultiplierPart;
  justSide = getSide[justSideGeneratorsPart, justSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart];
  
  (* Print["temperedSideMinusGeneratorsPart: ", N[temperedSideMinusGeneratorsPart] // MatrixForm];
  Print["temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]: ", N[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]] // MatrixForm];
  Print["Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]: ", N[Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]] // MatrixForm];
  Print["Transpose[temperedSideMinusGeneratorsPart].Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]: ", N[Transpose[temperedSideMinusGeneratorsPart].Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]] // MatrixForm];
  Print["justSide.Transpose[temperedSideMinusGeneratorsPart].Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]: ", N[justSide.Transpose[temperedSideMinusGeneratorsPart].Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]] // MatrixForm];
  Print["justSide: ", N[justSide] // MatrixForm]; *)
  
  First[justSide.Transpose[temperedSideMinusGeneratorsPart].Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]]
];


(* SOLUTIONS: GENERAL OPTIMIZATION POWER (MINISOP) OR GENERAL COMPLEXITY NORM POWER (P-NORM) *)

(* a numerical solution *)
(* covers minimax-QZES "KE", unchanged-octave minimax-ES "CTE" *)
powerSumSolution[parts_, unchangedIntervals_] := Module[
  {solution},
  
  solution = getPowerSumSolution[parts, unchangedIntervals];
  
  First[First[parts]] /. Last[solution]
];

(* no historically described tunings use this *)
(* a numerical solution *)
(* this is the fallback for when sumPolytopeSolution fails to find a unique solution *)
powerSumLimitSolution[{
  temperedSideGeneratorsPart_,
  temperedSideMappingPart_,
  justSideGeneratorsPart_,
  justSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_,
  powerPart_,
  periodsPerOctavePart_
}, unchangedIntervals_] := Module[
  {
    powerSumPowerLimit,
    powerSumPowerPower,
    powerSumPower,
    previousAbsErrorMagnitude,
    absErrorMagnitude,
    previousSolution,
    solution
  },
  
  powerSumPowerLimit = powerPart;
  powerSumPowerPower = 1;
  powerSumPower = Power[2, 1 / powerSumPowerPower];
  previousAbsErrorMagnitude = 1000001; (* this is just something really big, in order for initial conditions to work *)
  absErrorMagnitude = 1000000; (* this is just something really big, but not quite as big as previous *)
  
  While[
    powerSumPowerPower <= 6 && previousAbsErrorMagnitude - absErrorMagnitude > 0,
    previousAbsErrorMagnitude = absErrorMagnitude;
    previousSolution = solution;
    solution = getPowerSumSolution[{
      temperedSideGeneratorsPart,
      temperedSideMappingPart,
      justSideGeneratorsPart,
      justSideMappingPart,
      eitherSideIntervalsPart,
      eitherSideMultiplierPart,
      powerSumPower, (* note: this is different *)
      periodsPerOctavePart
    }, unchangedIntervals];
    absErrorMagnitude = First[solution];
    powerSumPowerPower = powerSumPowerPower += 1;
    powerSumPower = If[powerSumPowerLimit == 1, Power[2, 1 / powerSumPowerPower], Power[2, powerSumPowerPower]];
  ];
  
  First[temperedSideGeneratorsPart] /. Last[solution]
];

getPowerSumSolution[parts_, unchangedIntervals_] := Module[
  {temperedSideGeneratorsPart, periodsPerOctavePart, powerSum, minimizedPowerSum},
  
  temperedSideGeneratorsPart = part[parts, "temperedSideGeneratorsPart"];
  periodsPerOctavePart = part[parts, "periodsPerOctavePart"];
  
  powerSum = getPowerSumAbsError[parts];
  minimizedPowerSum = If[
    Length[unchangedIntervals] > 0,
    {powerSum, First[temperedSideGeneratorsPart][[1]] == 1 / periodsPerOctavePart},
    powerSum
  ];
  
  NMinimize[minimizedPowerSum, First[temperedSideGeneratorsPart], WorkingPrecision -> nMinimizePrecision]
];

getSide[
  temperedOrJustSideGeneratorsPart_,
  temperedOrJustSideMappingPart_,
  eitherSideIntervalsPart_,
  eitherSideMultiplierPart_
] := temperedOrJustSideGeneratorsPart.temperedOrJustSideMappingPart.eitherSideIntervalsPart.eitherSideMultiplierPart;
