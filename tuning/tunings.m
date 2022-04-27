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
  returns the optimal generator tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGeneratorsTuningMap[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 695.294}
*)
Options[optimizeGeneratorsTuningMap] = tuningOptions;
optimizeGeneratorsTuningMap[t_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    tuningOptions,
    optimizedGeneratorsTuningMap,
    tPossiblyWithChangedIntervalBasis
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait -1 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityLogPrimePower = OptionValue["complexityLogPrimePower"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityOctavePower = OptionValue["complexityOctavePower"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  
  tuningOptions = processTuningOptions[
    t,
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    False
  ];
  
  optimizationPower = Part[tuningOptions, 3];
  tPossiblyWithChangedIntervalBasis = Part[tuningOptions, 10];
  pureOctaveStretch = Part[tuningOptions, 13];
  
  optimizedGeneratorsTuningMap = 1200 * If[
    (* covers minimax, TOP, TE, L1-style Frobenius, Frobenius, BOP, BE, Weil, WE, Kees, KE, CTE, POTOP, POTE *)
    optimizationPower == \[Infinity],
    (*  Print["at least this"];*)
    optimizeGeneratorsTuningMapMinimax[tuningOptions],
    If[
      optimizationPower == 2,
      (* covers minisos *)
      optimizeGeneratorsTuningMapMinisos[tuningOptions],
      (* covers minisum *)
      optimizeGeneratorsTuningMapMinisum[tuningOptions]
    ]
  ];
  
  If[
    !isStandardPrimeLimitIntervalBasis[getIntervalBasis[t]] && tuningIntervalBasis == "primes",
    optimizedGeneratorsTuningMap = retrievePrimesIntervalBasisGeneratorsTuningMap[optimizedGeneratorsTuningMap, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    pureOctaveStretch,
    optimizedGeneratorsTuningMap = getPureOctaveStretchedGeneratorsTuningMap[optimizedGeneratorsTuningMap, t]
  ];
  
  SetAccuracy[optimizedGeneratorsTuningMap, 4]
];


(*
  optimizeTuningMap[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimal tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 1899.26, 2790.25}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 1899.26, 2790.25} 
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTuningMap[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 1893.54, 2781.18} 
*)
Options[optimizeTuningMap] = tuningOptions; (* TODO: oh what if we just moved this below the tuningOptions def instead ? *)
optimizeTuningMap[t_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait -1 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityLogPrimePower = OptionValue["complexityLogPrimePower"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityOctavePower = OptionValue["complexityOctavePower"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  
  SetAccuracy[optimizeGeneratorsTuningMap[t, {
    "unchangedIntervals" -> unchangedIntervals, (* trait -1 *)
    "targetedIntervals" -> targetedIntervals, (* trait 0 *)
    "optimizationPower" -> optimizationPower, (* trait 1 *)
    "damageWeightingSlope" -> damageWeightingSlope, (* trait 2 *)
    "complexityNormPower" -> complexityNormPower, (* trait 3 *)
    "complexityLogPrimePower" -> complexityLogPrimePower, (* trait 4a *)
    "complexityPrimePower" -> complexityPrimePower, (* trait 4b *)
    "complexitySizeFactor" -> complexitySizeFactor, (* trait 4c *)
    "complexityOctavePower" -> complexityOctavePower, (* trait 4d *)
    "tuningIntervalBasis" -> tuningIntervalBasis,
    "pureOctaveStretch" -> pureOctaveStretch,
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "originalComplexityName" -> originalComplexityName,
    "systematicComplexityName" -> systematicComplexityName
  }].getA[getM[t]], 4]
];


(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGeneratorsTuningMapMinimax[{
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  pureOctaveStretch_
}] := If[
  damageWeightingSlope == "simplicityWeighted" && Length[targetedIntervalsA] == 0,
  (*Print["doing this right"];*)
  optimizeGeneratorsTuningMapTargetingAll[
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ],
  
  If[
    damageWeightingSlope == "unweighted",
    (*    Print["and shoudl be this"];*)
    optimizeGeneratorsTuningMapMinimaxTargetingListAnalytical[
      unchangedIntervals, (* trait -1 *)
      targetedIntervalsA, (* trait 0 *)
      optimizationPower, (* trait 1 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityLogPrimePower, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityOctavePower, (* trait 4d *)
      t,
      d,
      primesTuningMap
    ],
    (*    Print["alright now we're weighted"];*)
    optimizeGeneratorsTuningMapTargetingListNumerical[
      unchangedIntervals, (* trait -1 *)
      targetedIntervalsA, (* trait 0 *)
      optimizationPower, (* trait 1 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityLogPrimePower, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityOctavePower, (* trait 4d *)
      t,
      d,
      primesTuningMap
    ]
  ]
];


(* TARGETING-ALL *)

optimizeGeneratorsTuningMapTargetingAll[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := If[
  complexityNormPower == 2 && Length[unchangedIntervals] == 0 && complexityOctavePower == 1,
  
  (* covers TE, Frobenius, WE, BE *)
  (*  Print["hmm... complexityNormPower: ", 2, " unchangedItnevals: ", unchangedIntervals, " and octavePower: ", complexityOctavePower];*)
  optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ],
  
  (* covers TOP, L1 version of Frobenius, BOP, Weil, Kees, KE, CTE *)
  (*  Print["now this is supposed to work but this is more than a simple rewiring"];*)
  optimizeGeneratorsTuningMapTargetingAllNumerical[
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[{unitsCounterMultiplierA, targetedIntervalsAsPrimesIdentityA},
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  
  unitsCounterMultiplierA = getUnitsCounterMultiplierA[
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ];
  
  Print["arlight WE do your worst! unitsCounterMultiplierA: ", unitsCounterMultiplierA];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[
    targetedIntervalsAsPrimesIdentityA,
    unitsCounterMultiplierA,
    t,
    primesTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumerical[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {generatorsTuningMap, ma, tuningMap},
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tuningMap = generatorsTuningMap.ma;
  
  If[
    complexitySizeFactor != 0 && complexityNormPower == 1,
    
    (*TODO: you can't have these originalComplexityName things floating around in the main body anymore since people might not provide those *)
    
    (* covers Weil and Kees *)
    (*    Print["succcesffulyly idenitficyudfying Weil"];*)
    optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
      unchangedIntervals, (* trait -1 *)
      complexityNormPower, (* trait 3 *)
      complexityLogPrimePower, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityOctavePower, (* trait 4d *)
      t,
      d,
      primesTuningMap,
      tuningMap,
      generatorsTuningMap
    ],
    
    (* covers TOP, BOP, and L1-version of Frobenius, and KE and CTE even though it's L2-ified? TODO: make sure this is so *)
    Print["so KE is doing here?"];
    optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
      unchangedIntervals, (* trait -1 *)
      complexityNormPower, (* trait 3 *)
      complexityLogPrimePower, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityOctavePower, (* trait 4d *)
      t,
      d,
      primesTuningMap,
      tuningMap,
      generatorsTuningMap
    ]
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := If[
  complexityNormPower != 2 && hasNonUniqueTuning[getM[t]],
  (*  Print["does not have unique result"];*)
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasNonUniqueResult[ (* TODO: rename to does not have unique result *)
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t,
    d,
    primesTuningMap,
    tuningMap,
    generatorsTuningMap
  ],
  (*  Print["has unique result"];*)
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t,
    d,
    primesTuningMap,
    tuningMap,
    generatorsTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := Module[
  {
    primesErrorMap,
    unitsCounterMultiplierA,
    targetedIntervalsAsPrimesIdentityA,
    solution,
    optimizationPower,
    unitsCounterMultipliedPrimesErrorMap,
    minimizationThing,
    periodsPerOctave
  },
  
  (*  Print["shiny happy people"];*)
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  
  
  
  unitsCounterMultiplierA = getUnitsCounterMultiplierA[
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    1, (* trait 4d *) (* complexityOctavePower = 1;*) (* TODO: okay I think the problem with my KE right now is that I'm doubling up, with the pure octave constraint to the minimizer, plus it's factored into this weighting matrix... so it should be not in the W! that's what this is about... and I think it speaks to the fact that this doesn't need to be handled as part of complexity but shoudl just be a cosntraint. well I mean, you could still ask of an interval what is its KE-seminorm, but if you want to calc the tuning, you don't describe it in terms of the KE-seminorm, you just do a WE-norm and then octave constraint it I guess? *)
    t
  ];
  periodsPerOctave = First[First[getA[getM[t]]]]; (* TODO extract this somewhere shared *)
  
  (* TODO: I don't like this name and don't want to propagate it *)
  unitsCounterMultipliedPrimesErrorMap = primesErrorMap.unitsCounterMultiplierA;
  Print["am i misunderstanding this, then? ", unitsCounterMultipliedPrimesErrorMap];
  optimizationPower = dualPower[complexityNormPower];
  (* TODO: so this is still me procrastinating on actually realizing unchanged intervals; still just dumbly 
  if there's anything then assume it's prime 2 and first el is prime 2 no nonstandard interval basis or anything *)
  Print["this condition", Length[unchangedIntervals] > 0 || complexityOctavePower == 0, unchangedIntervals, complexityOctavePower];
  minimizationThing = If[
    Length[unchangedIntervals] > 0 || complexityOctavePower == 0,
    {Norm[unitsCounterMultipliedPrimesErrorMap, optimizationPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave}, (* TODO: this is a way better way to apparoach this problem I think *)
    Norm[unitsCounterMultipliedPrimesErrorMap, optimizationPower]
  ];(* TODO: you have something else like this somewhere here *)
  
  (*  Print["minimizationThing: ", minimizationThing];*)
  (*  Print["generatorsTuningMap: ", generatorsTuningMap];*)
  (*  Print["unitsCounterMultiplierA: ", unitsCounterMultiplierA];*)
  (*  Print["unchangedIntervals: ", unchangedIntervals];*)
  (*  Print["optimizationPower: ", optimizationPower];*)
  (*  Print["primesErrorMap: ", primesErrorMap];*)
  (*  Print["unitsCounterMultipliedPrimesErrorMap: ", unitsCounterMultipliedPrimesErrorMap];*)
  
  solution = NMinimize[
    minimizationThing,
    generatorsTuningMap,
    WorkingPrecision -> 128
  ];
  
  generatorsTuningMap /. Last[solution] // N
];

(* TODO: might be able to DRY it up with optimizeGeneratorsTuningMapTargetingListNumerical, 
or at least correlate their implementations as much as possible to illuminate the patterns, 
as you did with getDamagesWeightingA and getUnitsCounterMultiplierA *)
optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasNonUniqueResult[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := Module[
  {
    primesErrorMap,
    unitsCounterMultiplierA,
    targetedIntervalsAsPrimesIdentityA,
    solution,
    previousSolution,
    optimizationPower,
    previousPrimesErrorMagnitude,
    primesErrorMagnitude,
    normPowerPower,
    normPower,
    unitsCounterMultipliedPrimesErrorMap,
    minimizationThing
  },
  
  primesErrorMap = tuningMap - primesTuningMap;
  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];
  unitsCounterMultiplierA = getUnitsCounterMultiplierA[
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ];
  unitsCounterMultipliedPrimesErrorMap = primesErrorMap.unitsCounterMultiplierA;
  optimizationPower = dualPower[complexityNormPower];
  previousPrimesErrorMagnitude = \[Infinity];
  primesErrorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  
  (*  Print["just making sure this is looks normal", unitsCounterMultipliedPrimesErrorMap];*)
  
  While[
    (*    Print["h"] ||*)
    normPowerPower <= 10 && previousPrimesErrorMagnitude != primesErrorMagnitude && previousPrimesErrorMagnitude - primesErrorMagnitude > 0,
    
    (*    Print["bring it around town"];*)
    previousPrimesErrorMagnitude = primesErrorMagnitude;
    (*    Print["a", previousPrimesErrorMagnitude];*)
    previousSolution = solution;
    (*    Print["b", previousSolution];*)
    Print["this condition 2", Length[unchangedIntervals] > 0 || complexityOctavePower == 0, unchangedIntervals, complexityOctavePower];
    minimizationThing = If[
      Length[unchangedIntervals] > 0 || complexityOctavePower == 0,
      {Norm[unitsCounterMultipliedPrimesErrorMap, optimizationPower], unitsCounterMultipliedPrimesErrorMap[[1]] == 0},
      Norm[unitsCounterMultipliedPrimesErrorMap, optimizationPower]
    ];
    (*    Print["c", minimizationThing];*)
    solution = NMinimize[minimizationThing, generatorsTuningMap, WorkingPrecision -> 128];
    (*    Print["d", solution];*)
    primesErrorMagnitude = First[solution];
    (*    Print["e", primesErrorMagnitude];*)
    normPowerPower = normPowerPower += 1;
    (*    Print["f", normPowerPower];*)
    (*    Print["was it shomething I said? ", previousPrimesErrorMagnitude ];*)
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
    (*    Print["g", normPower];*)
    (* Print["doing this loop. normPowerPower: ", normPowerPower," prev: ", N[previousPrimesErrorMagnitude,7],  " new: ", N[primesErrorMagnitude,7], " diff: ",N[previousPrimesErrorMagnitude - primesErrorMagnitude,7] ];*)
    (*  Print["doing this loop. normPowerPower: ", normPowerPower," prev: ", previousPrimesErrorMagnitude,  " new: ", primesErrorMagnitude, " diff: ",previousPrimesErrorMagnitude - primesErrorMagnitude ];*)
  ];
  
  generatorsTuningMap /. Last[previousSolution] // N
];

(* TODO: this doesn't actually handle different powers of the size factor yet ... 
would need to research on the Xenwiki to see how to do that; pretty sure it was explained *)
optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  tuningMap_,
  generatorsTuningMap_
] := Module[
  {augmentedThing, almostL1Norm, solution, middleMan, minimizeSetup},
  
  middleMan = tuningMap / primesTuningMap - Table[1, d];
  augmentedThing = AppendTo[middleMan, 0];
  almostL1Norm = Max[augmentedThing] - Min[augmentedThing];
  minimizeSetup = If[complexityOctavePower == 0, {almostL1Norm, augmentedThing[[1]] == 0}, almostL1Norm];
  solution = NMinimize[minimizeSetup, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];

(* getDamagesWeightingA may be simplicity-weighted or complexity-weighted, but this is always essentially "simplicity-weighted" *)
getUnitsCounterMultiplierA[
  targetedIntervalsAsPrimesIdentityA_, (* trait 0 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_
] := Module[{},
  Print["how can this be happening"];
  
  PseudoInverse[getUnitsMultiplierA[
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ]]
];


(* TARGETING-LIST *)

optimizeGeneratorsTuningMapMinimaxTargetingListAnalytical[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := optimizeGeneratorsTuningMapSimplex[
  unchangedIntervals, (* trait -1 *)
  targetedIntervalsA, (* trait 0 *)
  optimizationPower, (* trait 1 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityLogPrimePower, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityOctavePower, (* trait 4d *)
  t,
  d,
  primesTuningMap
];

optimizeGeneratorsTuningMapMinisos[{
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  pureOctaveStretch_
}] := Module[
  {damagesWeightingA},
  
  damagesWeightingA = getDamagesWeightingA[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[targetedIntervalsA, damagesWeightingA, t, primesTuningMap]
];

optimizeGeneratorsTuningMapMinisum[{
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_,
  pureOctaveStretch_
}] := optimizeGeneratorsTuningMapSimplex[
  unchangedIntervals, (* trait -1 *)
  targetedIntervalsA, (* trait 0 *)
  optimizationPower, (* trait 1 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityLogPrimePower, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityOctavePower, (* trait 4d *)
  t,
  d,
  primesTuningMap
];

(* TODO: note we're not yet ready to extract the NMinimizes from the targeting-all stuff 
cuz there's already too much going on in this refactor*)
(* NUMERICAL *) (* TODO: LIST WHAT USES THIS, LIKE YOU HAVE FOR ANALYTIC BELOW *)

optimizeGeneratorsTuningMapTargetingListNumerical[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := If[
  hasNonUniqueTuning[getM[t]],
  (*Print["unique her"];*)
  optimizeGeneratorsTuningMapTargetingListNumericalUnique[
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ],
  (*Print["non-unique her"];*)
  optimizeGeneratorsTuningMapTargetingListNumericalNonUnique[
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t,
    d,
    primesTuningMap
  ]
];

optimizeGeneratorsTuningMapTargetingListNumericalUnique[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {
    generatorsTuningMap,
    ma,
    mappedTargetedIntervalsA,
    pureTargetedIntervalsASizes,
    damagesWeightingA,
    solution,
    targetedIntervalErrorsL
  },
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTargetedIntervalsASizes = Map[primesTuningMap.#&, targetedIntervalsA];
  damagesWeightingA = getDamagesWeightingA[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ];
  mappedTargetedIntervalsA = Transpose[ma.Transpose[targetedIntervalsA]];
  targetedIntervalErrorsL = Flatten[MapIndexed[
    Function[
      {mappedTargetedInterval, targetedIntervalIndex},
      Abs[
        Total[
          MapThread[
            Function[
              {mappedTargetedIntervalEntry, gtmEntry},
              mappedTargetedIntervalEntry * gtmEntry
            ],
            {mappedTargetedInterval, generatorsTuningMap}
          ]
        ] - pureTargetedIntervalsASizes[[targetedIntervalIndex]]
      ] * Part[Part[damagesWeightingA, targetedIntervalIndex, targetedIntervalIndex]]
    ],
    mappedTargetedIntervalsA
  ]];
  
  solution = NMinimize[Norm[targetedIntervalErrorsL, optimizationPower], generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution] // N
];

optimizeGeneratorsTuningMapTargetingListNumericalNonUnique[
  unchangedIntervals_, (* trait -1 *) (* TODO: neither this nor the unique version use this yet *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {
    generatorsTuningMap,
    ma,
    mappedTargetedIntervalsA,
    pureTargetedIntervalsASizes,
    damagesWeightingA,
    solution,
    errorMagnitude,
    previousErrorMagnitude,
    normPower,
    normPowerPower,
    targetedIntervalErrorsL,
    previousSolution
  },
  (*Print["going here nonunique"];*)
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTargetedIntervalsASizes = Map[primesTuningMap.#&, targetedIntervalsA];
  damagesWeightingA = getDamagesWeightingA[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ];
  mappedTargetedIntervalsA = Transpose[ma.Transpose[targetedIntervalsA]];
  
  previousErrorMagnitude = \[Infinity];
  errorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  targetedIntervalErrorsL = Flatten[MapIndexed[
    Function[
      {mappedTargetedInterval, targetedIntervalIndex},
      Abs[
        Total[
          MapThread[
            Function[
              {mappedTargetedIntervalEntry, gtmEntry},
              mappedTargetedIntervalEntry * gtmEntry
            ],
            {mappedTargetedInterval, generatorsTuningMap}
          ]
        ] - pureTargetedIntervalsASizes[[targetedIntervalIndex]]
      ] * Part[Part[damagesWeightingA, targetedIntervalIndex, targetedIntervalIndex]]
    ],
    mappedTargetedIntervalsA
  ]];
  
  While[
    normPowerPower <= 10 && previousErrorMagnitude - errorMagnitude > 0,
    
    previousErrorMagnitude = errorMagnitude;
    previousSolution = solution;
    solution = NMinimize[Norm[targetedIntervalErrorsL, normPower], generatorsTuningMap, WorkingPrecision -> 128];
    errorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  generatorsTuningMap /. Last[previousSolution] // N
];


(* ANALYTICAL  *)

optimizeGeneratorsTuningMapSimplex[
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  d_,
  primesTuningMap_
] := Module[
  {
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialProjectionAs,
    potentialTuningMaps,
    powerSumOfDamages,
    minMeanIndices,
    minMeanIndex,
    minMeanP,
    generatorsPreimageTransversal,
    projectedGenerators,
    damagePowerSum
  },
  (*  Print["so this isn't matching up somehow? here"];*)
  
  damagePowerSum = If[optimizationPower == 1, getSumDamage, getMaxDamage];
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[targetedIntervalsA]], {r}];
  potentialUnchangedIntervalSets = Map[Map[targetedIntervalsA[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialProjectionAs = Select[Map[
    getProjectionAFromUnchangedIntervals[t, #]&,
    filteredNormalizedPotentialUnchangedIntervalSets
  ], Not[# === Null]&];
  potentialTuningMaps = Map[primesTuningMap.#&, potentialProjectionAs];
  powerSumOfDamages = Map[damagePowerSum[
    #,
    t,
    primesTuningMap,
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower (* trait 4d *)
  ]&, potentialTuningMaps];
  
  (* Print["alright, powerSumOfDamages: ", powerSumOfDamages]; (* TODO:isn't it really powerSumsOfDamages? or just damages, that come from power sums?*)
 Print["targetedIntervalsA: ", targetedIntervalsA];
  Print["unchangedIntervalSetIndices: ", unchangedIntervalSetIndices];
   Print["t: ", t];
  Print["d: ", d];*)
  
  minMeanIndices = Position[powerSumOfDamages, Min[powerSumOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    
    (*    Print["A"];*)
    minMeanIndex = First[First[Position[powerSumOfDamages, Min[powerSumOfDamages]]]];
    minMeanP = potentialProjectionAs[[minMeanIndex]];
    generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[t]]];
    projectedGenerators = minMeanP.generatorsPreimageTransversal;
    primesTuningMap.projectedGenerators // N,
    
    (*    Print["B"];*)
    optimizeGeneratorsTuningMapTargetingListNumerical[
      unchangedIntervals, (* trait -1 *)
      targetedIntervalsA, (* trait 0 *)
      optimizationPower, (* trait 1 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityLogPrimePower, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityOctavePower, (* trait 4d *)
      t,
      d,
      primesTuningMap
    ]
  ]
];

getProjectionAFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[{commaEigenvectors, eigenvectors, diagonalEigenvalueA},
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueA = getDiagonalEigenvalueA[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueA.Inverse[eigenvectors]]
];

getDiagonalEigenvalueA[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];

optimizeGeneratorsTuningMapWithPseudoInverse[timaOrPrimesIdentityA_, WorUnitsMultiplierA_, t_, primesTuningMap_] :=
    Module[{ma, weightedTargetedIntervalsAMapped, generatorsA, generatorsTuningMap},
      (*  Print["uh huh yeeah", WorUnitsMultiplierA];*) (* TODO: rename this W thing *)
      ma = getA[getM[t]];
      weightedTargetedIntervalsAMapped = ma.Transpose[timaOrPrimesIdentityA].WorUnitsMultiplierA;
      (* TODO: if we're constantly transposing targetedIntervalsA, maybe just do it once up front? or have getA respect the co/contra? *)
      generatorsA = Transpose[timaOrPrimesIdentityA].WorUnitsMultiplierA.Transpose[weightedTargetedIntervalsAMapped].Inverse[weightedTargetedIntervalsAMapped.Transpose[weightedTargetedIntervalsAMapped]];
      generatorsTuningMap = primesTuningMap.generatorsA;
      generatorsTuningMap // N
    ];


(* TARGET INTERVAL SETS *)

getDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  padVectorsWithZerosUpToD[Map[quotientToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;


(* DAMAGE *) (* TODO: so you would also imagine the plot function would follow a similar pattern to getDamage, in that it follows a similar pattern to optimizeGtm itself *)

Options[getDamage] = tuningOptions;
getDamage[t_, generatorsTuningMap_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    ma,
    tuningMap,
    primesTuningMap,
    tuningOptions,
    targetedIntervalsA
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait -1 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityLogPrimePower = OptionValue["complexityLogPrimePower"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityOctavePower = OptionValue["complexityOctavePower"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  
  tuningOptions = processTuningOptions[
    t, (* TODO: I think this is more the right way to do it: pass t around and don't worry about calculating ptm and d and stuff in other places *)
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    True (* TODO: make a named variable in both places so it's clear what's happening *)
  ];
  optimizationPower = Part[tuningOptions, 1];
  (*TODO: see another reason not to have a separate targetedIntervalsA and targetedIntervals! 
  causes weird doubling up in the local variables list *)
  targetedIntervalsA = Part[tuningOptions, 2];
  
  ma = getA[getM[t]];
  tuningMap = (generatorsTuningMap / 1200).ma;
  primesTuningMap = getPrimesTuningMap[t];
  
  If[
    optimizationPower == \[Infinity],
    getMaxDamage[
      tuningMap,
      t,
      primesTuningMap,
      targetedIntervalsA, (* trait 0 *)
      damageWeightingSlope, (* trait 2 *)
      complexityNormPower, (* trait 3 *)
      complexityLogPrimePower, (* trait 4a *)
      complexitySizeFactor, (* trait 4b *)
      complexityPrimePower, (* trait 4c *)
      complexityOctavePower (* trait 4d *)
    ],
    If[
      optimizationPower == 2,
      get2SumDamage[
        tuningMap,
        t,
        primesTuningMap,
        targetedIntervalsA, (* trait 0 *)
        damageWeightingSlope, (* trait 2 *)
        complexityNormPower, (* trait 3 *)
        complexityLogPrimePower, (* trait 4a *)
        complexitySizeFactor, (* trait 4b *)
        complexityPrimePower, (* trait 4c *)
        complexityOctavePower (* trait 4d *)
      ],
      getSumDamage[
        tuningMap,
        t,
        primesTuningMap,
        targetedIntervalsA, (* trait 0 *)
        damageWeightingSlope, (* trait 2 *)
        complexityNormPower, (* trait 3 *)
        complexityLogPrimePower, (* trait 4a *)
        complexitySizeFactor, (* trait 4b *)
        complexityPrimePower, (* trait 4c *)
        complexityOctavePower (* trait 4d *)
      ]
    ]
  ]
];

getTargetedIntervalDamages[
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_,
  tuningMap_,
  primesTuningMap_
] := Module[
  {targetedIntervalErrorsList, targetedIntervalDamagesList, damagesWeightingA},
  
  targetedIntervalErrorsList = N[tuningMap.Transpose[targetedIntervalsA]] - N[primesTuningMap.Transpose[targetedIntervalsA]];
  targetedIntervalDamagesList = Abs[targetedIntervalErrorsList];
  damagesWeightingA = getDamagesWeightingA[
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ];
  
  targetedIntervalDamagesList.damagesWeightingA
];

Square[n_] := n^2;

getSumDamage[
  tuningMap_,
  t_,
  primesTuningMap_,
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityOctavePower_ (* trait 4d *)
] := Total[getTargetedIntervalDamages[
  targetedIntervalsA, (* trait 0 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityLogPrimePower, (* trait 4a *)
  complexitySizeFactor, (* trait 4b *)
  complexityPrimePower, (* trait 4c *)
  complexityOctavePower, (* trait 4d *)
  t,
  tuningMap,
  primesTuningMap
]];

get2SumDamage[
  tuningMap_,
  t_,
  primesTuningMap_,
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityOctavePower_ (* trait 4d *)
] := Total[Square[getTargetedIntervalDamages[
  targetedIntervalsA, (* trait 0 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityLogPrimePower, (* trait 4a *)
  complexitySizeFactor, (* trait 4b *)
  complexityPrimePower, (* trait 4c *)
  complexityOctavePower, (* trait 4d *)
  t,
  tuningMap,
  primesTuningMap
]]];

getMaxDamage[
  tuningMap_,
  t_,
  primesTuningMap_,
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexitySizeFactor_, (* trait 4b *)
  complexityPrimePower_, (* trait 4c *)
  complexityOctavePower_ (* trait 4d *)
] := Max[getTargetedIntervalDamages[
  targetedIntervalsA, (* trait 0 *)
  damageWeightingSlope, (* trait 2 *)
  complexityNormPower, (* trait 3 *)
  complexityLogPrimePower, (* trait 4a *)
  complexitySizeFactor, (* trait 4b *)
  complexityPrimePower, (* trait 4c *)
  complexityOctavePower, (* trait 4d *)
  t,
  tuningMap,
  primesTuningMap
]];

getDamagesWeightingA[
  targetedIntervalsA_, (* trait 0 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_
] := Module[{damagesWeightingA},
  damagesWeightingA = If[
    damageWeightingSlope != "unweighted",
    DiagonalMatrix[Map[getComplexity[
      #,
      complexityNormPower, (* trait 3 *)
      complexityLogPrimePower, (* trait 4a *)
      complexityPrimePower, (* trait 4b *)
      complexitySizeFactor, (* trait 4c *)
      complexityOctavePower, (* trait 4d *)
      t
    ]&, targetedIntervalsA]],
    IdentityMatrix[Length[targetedIntervalsA]]
  ];
  
  If[damageWeightingSlope == "simplicityWeighted", PseudoInverse[damagesWeightingA], damagesWeightingA]
];


(* COMPLEXITY *)

getComplexity[
  pcv_,
  complexityNormPower_, (* trait 3 *)
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_
] := Module[{unitsMultiplierA},
  (* TODO: I'm afraid this might be computing over and over... might be a good way to save some computation *)
  unitsMultiplierA = getUnitsMultiplierA[
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    t
  ];
  
  Norm[unitsMultiplierA.pcv, complexityNormPower]
];

(* TODO: we don't actually use any of these directly *)
getPcvCopfrComplexity[pcv_, t_] := Total[Map[If[Abs[# > 0], 1, 0]&, pcv]];
(* AKA "Benedetti height" *)
getPcvProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getIntervalBasis[t], pcv}];
(* AKA "Tenney height" *)
getPcvLogProductComplexity[pcv_, t_] := Log[2, getPcvProductComplexity[pcv, t]];
(* AKA "Wilson height", can also be used to find BOP tuning *)
getPcvSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getIntervalBasis[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find TOP tuning *)
getPcvLogSopfrComplexity[pcv_, t_] := Log[2, getPcvSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getPcvIntegerLimitComplexity[pcv_, t_] := Module[{quotient},
  quotient = pcvToQuotient[pcv];
  Max[Numerator[quotient], Denominator[quotient]]
];
(* AKA "logarithmic Weil height", used for "Weil tuning" *)
getPcvLogIntegerLimitComplexity[pcv_, t_] := Log[2, getPcvIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
removePowersOfTwoFromPcv[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getPcvOddLimitComplexity[pcv_, t_] := getPcvIntegerLimitComplexity[removePowersOfTwoFromPcv[pcv], t];
(* AKA "Kees expressibility" , used for "Kees tuning" *)
getPcvLogOddLimitComplexity[pcv_, t_] := Log[2, getPcvOddLimitComplexity[pcv, t]];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGeneratorsTuningMap[optimizedGeneratorsTuningMap_, originalT_, t_] := Module[
  {ma, optimizedTm, generatorsPreimageTransversal, f},
  
  ma = getA[getM[t]];
  optimizedTm = optimizedGeneratorsTuningMap.ma;
  generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[originalT]]];
  f = Transpose[getFormalPrimesA[originalT]];
  
  optimizedTm.f.generatorsPreimageTransversal
];


(* PURE-OCTAVE STRETCH *)

getPureOctaveStretchedGeneratorsTuningMap[optimizedGeneratorsTuningMap_, t_] := Module[{periodsPerOctave},
  periodsPerOctave = First[First[getA[getM[t]]]];
  
  (1200 / periodsPerOctave) * (optimizedGeneratorsTuningMap / First[optimizedGeneratorsTuningMap])
];


(* TUNING UNIQUENESS *)

hasNonUniqueTuning[m_] := getR[m] > 1 && (hasIndependentGenerator[m] || primesInLockedRatio[m]);

countNonzeroElements[l_] := Count[l, element_ /; element != 0];
whichGeneratorIsTheSingleOneApproximatingThisPrime[generatorsApproximatingPrime_] := First[Position[generatorsApproximatingPrime, x_ /; x > 0, 1, 1]];

primesInLockedRatio[m_] := Module[
  {
    canonicalM,
    generatorsApproximatingEachPrime,
    countGeneratorsInvolvedInApproximatingEachPrime,
    whetherPrimesAreApproximatedBySingleGeneratorOrNot,
    indexesOfPrimesApproximatedBySingleGenerators,
    perGeneratorHowManyPrimesAreApproximatedOnlyByIt,
    index,
    hmmm, (* TODO: clean up *)
    whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator
  },
  canonicalM = canonicalForm[m];
  
  generatorsApproximatingEachPrime = Transpose[getA[canonicalM]];
  countGeneratorsInvolvedInApproximatingEachPrime = Map[countNonzeroElements, generatorsApproximatingEachPrime];
  whetherPrimesAreApproximatedBySingleGeneratorOrNot = Map[# == 1&, countGeneratorsInvolvedInApproximatingEachPrime];
  indexesOfPrimesApproximatedBySingleGenerators = {};
  MapIndexed[If[#1 == True, AppendTo[indexesOfPrimesApproximatedBySingleGenerators, #2] ]&, whetherPrimesAreApproximatedBySingleGeneratorOrNot];
  
  perGeneratorHowManyPrimesAreApproximatedOnlyByIt = Association[];
  Map[
    Function[{indexOfPrimeApproximatedBySingleGenerator},
      hmmm = First[Part[generatorsApproximatingEachPrime, indexOfPrimeApproximatedBySingleGenerator]];
      index = whichGeneratorIsTheSingleOneApproximatingThisPrime[hmmm];
      If[
        KeyExistsQ[perGeneratorHowManyPrimesAreApproximatedOnlyByIt, index],
        perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] = perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] + 1,
        perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] = 1
      ];
    ],
    indexesOfPrimesApproximatedBySingleGenerators
  ];
  whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator = Map[# > 1&, Values[perGeneratorHowManyPrimesAreApproximatedOnlyByIt]];
  AnyTrue[whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator, TrueQ]
];

hasIndependentGenerator[m_] := Module[{},
  canonicalM = canonicalForm[m];
  
  AnyTrue[getA[canonicalM], TrueQ[Total[Abs[#]] == 1]&]
];


(* SHARED *)

tuningOptions = {
  "unchangedIntervals" -> {}, (* trait -1 *)
  "targetedIntervals" -> Null, (* trait 0 *)
  "optimizationPower" -> Null, (* trait 1 *)
  "damageWeightingSlope" -> "", (* trait 2 *)
  "complexityNormPower" -> 1, (* trait 3 *)
  "complexityLogPrimePower" -> 1, (* trait 4a: multiply by logs of primes *)
  "complexityPrimePower" -> 0, (* trait 4b: 0 = nothing, equiv to copfr when span and log prime are 0 and octave is 1; 1 = product complexity, equiv to sopfr when span and log prime are 0 and octave is 1; >1 = pth power of those *)
  "complexitySizeFactor" -> 0, (* trait 4c: 0 = no augmentation to factor in span, 1 = Weil style, etc. *)
  "complexityOctavePower" -> 1, (* trait 4d: False = nothing, True = achieve Kees from Weil, KE from WE, etc. *)
  "tuningIntervalBasis" -> "primes",
  "pureOctaveStretch" -> False,
  "systematicTuningName" -> "",
  "originalTuningName" -> "",
  "systematicComplexityName" -> "",
  "originalComplexityName" -> ""
};

processTuningOptions[
  t_,
  inputUnchangedIntervals_, (* trait -1 *)
  inputTargetedIntervals_, (* trait 0 *)
  inputOptimizationPower_, (* trait 1 *)
  inputDamageWeightingSlope_, (* trait 2 *)
  inputComplexityNormPower_, (* trait 3 *)
  inputComplexityLogPrimePower_, (* trait 4a *)
  inputComplexityPrimePower_, (* trait 4b *)
  inputComplexitySizeFactor_, (* trait 4c *)
  inputComplexityOctavePower_, (* trait 4d *)
  inputTuningIntervalBasis_,
  inputPureOctaveStretch_,
  inputSystematicTuningName_,
  inputOriginalTuningName_,
  inputSystematicComplexityName_,
  inputOriginalComplexityName_,
  forDamage_ : False
] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    primesTuningMap,
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA,
    d,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    intervalBasis,
    intervalRebase
  },
  
  unchangedIntervals = inputUnchangedIntervals; (* trait -1 *)
  targetedIntervals = inputTargetedIntervals; (* trait 0 *)
  optimizationPower = inputOptimizationPower; (* trait 1 *)
  damageWeightingSlope = inputDamageWeightingSlope; (* trait 2 *)
  complexityNormPower = inputComplexityNormPower; (* trait 3 *)
  complexityLogPrimePower = inputComplexityLogPrimePower; (* trait 4a *)
  complexityPrimePower = inputComplexityPrimePower; (* trait 4b *)
  complexitySizeFactor = inputComplexitySizeFactor; (* trait 4c *)
  complexityOctavePower = inputComplexityOctavePower; (* trait 4d *)
  tuningIntervalBasis = inputTuningIntervalBasis;
  pureOctaveStretch = inputPureOctaveStretch;
  systematicTuningName = inputSystematicTuningName;
  originalTuningName = inputOriginalTuningName;
  systematicComplexityName = inputSystematicComplexityName;
  originalComplexityName = inputOriginalComplexityName;
  
  If[
    originalTuningName === "minimax",
    optimizationPower = \[Infinity]; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "TOP" || originalTuningName === "TIPTOP",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    originalTuningName === "TE" || originalTuningName === "Tenney-Euclidean",
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
    (* Print["yes"];*)
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
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "QZE";
  ];
  If[
    originalTuningName === "POTOP" || originalTuningName === "POTT",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "POTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; systematicComplexityName = "E"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "CTE",
    targetedIntervals = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted";  systematicComplexityName = "E"; unchangedIntervals = {Join[{1}, Table[0, getD[t] - 1]]};
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
    StringMatchQ[systematicTuningName, "*S*"],
    damageWeightingSlope = "simplicityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningName, "*C*"],
    damageWeightingSlope = "complexityWeighted";
  ];
  If[
    StringMatchQ[systematicTuningName, "*U*"],
    damageWeightingSlope = "unweighted";
  ];
  
  (* trait 3 - same as complexity systematic name parts *)
  If[
    StringMatchQ[systematicTuningName, "*E*"] || StringMatchQ[systematicComplexityName, "*E*"],
    complexityNormPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningName, "*T*"] || StringMatchQ[systematicComplexityName, "*T*"],
    complexityNormPower = 1;
  ];
  
  (* trait 4 - same as complexity systematic name parts  *)
  If[
    StringMatchQ[systematicTuningName, "*N*"] || StringMatchQ[systematicComplexityName, "*N*"],
    complexityLogPrimePower = 0; (* TODO: rename this now to something more like Boolean, negateLogPrimeCoordination ... and then there's no good reason for complexityOctavePower to be a power too since theyre' not all four going to be powers, how about only one of them is... thatst gcan go back to being a boolena because no one ever asked for that to tbe a power either*)
  ];
  If[
    StringMatchQ[systematicTuningName, "*P*"] || StringMatchQ[systematicComplexityName, "*P*"], (* TODO: support  P2, P3, etc *)
    complexityPrimePower = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Z*"] || StringMatchQ[systematicComplexityName, "*Z*"],
    complexitySizeFactor = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Q*"] || StringMatchQ[systematicComplexityName, "*Q*"],
    complexityOctavePower = 0;
  ];
  
  If[
    originalComplexityName === "copfr" || originalComplexityName === "l1Norm", (* TODO: seriously though, copfr is copfr???? also, mercifully, no one has attempted to eponymify this one *)
    complexityNormPower = 1; complexityLogPrimePower = 0; complexitySizeFactor = 0; complexityPrimePower = 0; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "sopfr" || originalComplexityName === "wilsonHeight", (* TODO: note we can't implement product complexity using the setup we use here, because it's a product of terms not a sum *)
    complexityNormPower = 1; complexityLogPrimePower = 0; complexitySizeFactor = 0; complexityPrimePower = 1; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "integerLimit" || originalComplexityName === "weilHeight",
    complexityNormPower = 1; complexityLogPrimePower = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "oddLimit" || originalComplexityName === "keesHeight",
    complexityNormPower = 1; complexityLogPrimePower = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 0;
  ];
  If[
    originalComplexityName === "logProduct" || originalComplexityName === "tenneyHeight", (* TODO: since we disagree with this name, should we distinguish? like one is our systematic name, one is original name? *)
    complexityNormPower = 1; complexityLogPrimePower = 1; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "logIntegerLimit" || originalComplexityName === "logarithmicWeilHeight",
    complexityNormPower = 1; complexityLogPrimePower = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "logOddLimit" || originalComplexityName === "keesExpressibility",
    complexityNormPower = 1; complexityLogPrimePower = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 0;
  ];
  If[
    originalComplexityName === "rososcopfr" || originalComplexityName === "l2Norm", (* TODO: seriously though, copfr is copfr???? also, mercifully, no one has attempted to eponymify this one *)
    complexityNormPower = 2; complexityLogPrimePower = 0; complexitySizeFactor = 0; complexityPrimePower = 0; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "rosossopfr", (* TODO: note we can't implement product complexity using the setup we use here, because it's a product of terms not a sum *)
    complexityNormPower = 2; complexityLogPrimePower = 0; complexitySizeFactor = 0; complexityPrimePower = 1; complexityOctavePower = 1;
  ];
  (* this one is not defined If[
    ,
    complexityNormPower = 2; complexityLogPrimePower = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 1;
  ]; *)
  (* this one is not defined If[
    ,
    complexityNormPower = 2; complexityLogPrimePower = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 0;
  ]; *)
  If[
    originalComplexityName === "tenneyEuclideanHeight", (* TODO: since we disagree with this name, should we distinguish? like one is our systematic name, one is original name? *)
    complexityNormPower = 2; complexityLogPrimePower = 1; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "weilEuclideanNorm",
    complexityNormPower = 2; complexityLogPrimePower = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 1;
  ];
  If[
    originalComplexityName === "keesEuclideanSeminorm",
    complexityNormPower = 2; complexityLogPrimePower = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityOctavePower = 0;
  ];
  (* then this one doesn't follow the above patterns as closely *)
  If[
    originalComplexityName === "carlsNorm", (* https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
    complexityNormPower = 2; complexityLogPrimePower = 0; complexitySizeFactor = 0; complexityPrimePower = 2; complexityOctavePower = 1;
  ];
  
  (* note: this has to go below the systematic tuning name gating, so that targetedIntervals has a change to be set to {} *)
  intervalBasis = getIntervalBasis[t];
  If[
    !isStandardPrimeLimitIntervalBasis[intervalBasis] && tuningIntervalBasis == "primes",
    
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getIntervalBasisDimension[intervalBasis]];
    commaBasisInPrimeLimitIntervalBasis = changeIntervalBasis[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    intervalRebase = getIntervalRebaseForC[intervalBasis, primeLimitIntervalBasis];
    targetedIntervalsA = If[targetedIntervals === Null, getDiamond[d], If[Length[targetedIntervals] == 0, If[forDamage, intervalRebase.getA[getC[t]], {}], intervalRebase.getA[targetedIntervals]]];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    d = getD[tPossiblyWithChangedIntervalBasis];
    primesTuningMap = getPrimesTuningMap[tPossiblyWithChangedIntervalBasis],
    
    tPossiblyWithChangedIntervalBasis = t;
    d = getD[tPossiblyWithChangedIntervalBasis];
    primesTuningMap = getPrimesTuningMap[tPossiblyWithChangedIntervalBasis];
    targetedIntervalsA = If[targetedIntervals === Null, getDiamond[d], If[Length[targetedIntervals] == 0, If[forDamage, getA[getC[t]], {}], getA[targetedIntervals]]];
  ];
  
  If[!NumericQ[optimizationPower] && optimizationPower != \[Infinity], Throw["no optimization power"]];
  If[damageWeightingSlope == "", Throw["no damage weighting slope"]];
  
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *) (* TODO: I should have a function called aMultiply[] which takes a list of co/contra things, gets A's and transposes the contra ones and then dots them, so I don't have to pass this business around separately from targetedIntervals *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityLogPrimePower, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityOctavePower, (* trait 4d *)
    tPossiblyWithChangedIntervalBasis,
    d,
    primesTuningMap,
    pureOctaveStretch
  }
];

getPrimesTuningMap[t_] := Log[2, getIntervalBasis[t]];

(* TODO: verify that these  matrices result in the same formulae as appear in the "vector i form" of my spreadsheet: 
https://docs.google.com/spreadsheets/d/1BBcUCoe6seCC1PM2qaByyiMNNLdxkEsx5X_-XJ9BdpE/edit#gid=694229653 *)
(* note this is different than the damagesWeightingA gotten by getDamagesWeightingA, this is nested within it;
this is to weight the quantities of the PC-vectors before taking a norm and getting an interval complexity, 
which are then all taken for each interval and assembled as damagesWeightingA *)
getUnitsMultiplierA[
  complexityLogPrimePower_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityOctavePower_, (* trait 4d *)
  t_
] := Module[{unitsMultiplierA},
  (* TODO: maybe this is actually a sort of F, formal primes matrix? 
  or maybe eventually this is just going to be an additional one of the transformation matrices per property *)
  unitsMultiplierA = IdentityMatrix[getD[t]];
  Print["well that was pretty crayz just dleteing me stuf"];
  
  If[
    (* when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm, covers TOP; 
    when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers TE; 
    when used by getDamagesWeightingA by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
    or by getTargetedIntervalDamages by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
    covers any targeting-list tuning using this as its damage's complexity *)
    complexityLogPrimePower > 0,
    unitsMultiplierA = unitsMultiplierA.DiagonalMatrix[Log2[getIntervalBasis[t]]]
  ];
  
  If[
    (* when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm, covers BOP;
    when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers BE *)
    complexityPrimePower > 0,
    (* Print["hey yeah doing this", complexityPrimePower, t, getIntervalBasis[t],Power[getIntervalBasis[t], complexityPrimePower], DiagonalMatrix[Power[getIntervalBasis[t], complexityPrimePower]] ];*)
    unitsMultiplierA = unitsMultiplierA.DiagonalMatrix[Power[getIntervalBasis[t], complexityPrimePower]]
  ];
  
  (* If[
    (* also covers TOP, TE, etc. (equivalent to "logProduct") *)
    originalComplexityName == "logSopfr",
    DiagonalMatrix[Map[getPcvLogSopfrComplexity[#, t]&, baseA]],
    
    If[
      (* also covers BOP, BE, etc. (equivalent to "product") *)
      originalComplexityName == "sopfr",
      DiagonalMatrix[Map[getPcvSopfrComplexity[#, t]&, baseA]],
      *)
  
  If[
    (* when Weil needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the min - max thing); 
    when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, covers WE; 
    when used by getDamagesWeightingA by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
    or by getTargetedIntervalDamages by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
    should cover any targeting-list tuning using this as its damage's complexity *)
    complexitySizeFactor > 0,
    (* Print["is this the problem here? (no it's not)", (Join[IdentityMatrix[getD[t]], {Table[complexitySizeFactor, getD[t]]}] / (1 + complexitySizeFactor)), " and ", unitsMultiplierA];*)
    unitsMultiplierA = (Join[IdentityMatrix[getD[t]], {Table[complexitySizeFactor, getD[t]]}] / (1 + complexitySizeFactor)).unitsMultiplierA
  ];
  
  If[
    (* when Kees needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the min - max thing) with pure-octave constraint on the solver; 
    ### still need to cover KE; when used by getDamagesWeightingA by optimizeGeneratorsTuningMapMinisos, optimizeGeneratorsTuningMapTargetingListNumerical, 
    or by getTargetedIntervalDamages by getSumDamage or getMaxDamage by optimizeGeneratorsTuningMapSimplex, 
    should cover any targeting-list tuning using this as its damage's complexity *)
    complexityOctavePower != 0, (* TODO: eventually this will just be the other property *)
    unitsMultiplierA = unitsMultiplierA.DiagonalMatrix[Join[{complexityOctavePower}, Table[1, getD[t] - 1]]]
  ];
  
  Print["final answer? ", unitsMultiplierA];
  
  (* if none of the above, you end up with
  when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm, 
     covers L1 version of Frobenius; 
     when used by getUnitsCounterMultiplierA for optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical, 
     covers Frobenius *)
  unitsMultiplierA
];



(* TODO eventually this should be able to just be a generic solver *)
(* TODO for now it just assumes unchangedIntervals is prime 2 and that's it, but eventually it can become an actual matrix like targetedIntervals *)
(*optimizeGeneratorsTuningMapCte[d_, t_, primesTuningMap_, complexityNormPower_, complexityLogPrimePower_, complexitySizeFactor_, complexityPrimePower_, complexityOctavePower_, unchangedIntervals_] := Module[*)
(*  {generatorsTuningMap, ma, tuningMap, primesErrorMap, solution, optimizationPower, targetedIntervalsAsPrimesIdentityA, unitsCounterMultiplierA},*)
(*  *)
(*  targetedIntervalsAsPrimesIdentityA = IdentityMatrix[d];*)
(*  unitsCounterMultiplierA = getUnitsCounterMultiplierA[t, targetedIntervalsAsPrimesIdentityA, originalComplexityName, complexityNormPower]; *)(* TODO: instead of counter-multiplier can it be like, units multiplier matrix for interval complexity, and units multiplier matrix for primes error magnitude? *)
(*  *)
(*  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];*)
(*  ma = getA[getM[t]];*)
(*  tuningMap = generatorsTuningMap.ma;*)
(*  *)
(*  primesErrorMap = tuningMap - primesTuningMap;*)
(*  primesErrorMap = unitsCounterMultiplierA.primesErrorMap;*)
(*  optimizationPower = 2;*)
(*  *)
(*  solution = NMinimize[{Norm[primesErrorMap, optimizationPower], primesErrorMap[[1]] == 0}, generatorsTuningMap, WorkingPrecision -> 128];*)
(*  *)
(*  generatorsTuningMap /. Last[solution] // N*)
(*];*)

(* TODO: wait this can't be right... it's not actually doing anything L2-ish, right? yeah I think you just do a normal weighting with the pseudoinverse of the interval complexity units multiplier, then take norm 2?? *)
(*optimizeGeneratorsTuningMapKe[d_, t_, primesTuningMap_, complexityNormPower_, complexityLogPrimePower_, complexitySizeFactor_, complexityPrimePower_, complexityOctavePower_, unchangedIntervals_] := Module[ *)
(*  {generatorsTuningMap, ma, tuningMap, solution, augmentedThing, almostL1Norm, middleMan, minimizeSetup},*)
(*  *)
(*  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];*)
(*  ma = getA[getM[t]];*)
(*  tuningMap = generatorsTuningMap.ma;*)
(*  *)
(*  middleMan = tuningMap / primesTuningMap - Table[1, d];*)
(*  augmentedThing = AppendTo[middleMan, 0]; *)(* maybe this 0 is the "junk" *)
(*  almostL1Norm = Max[augmentedThing] - Min[augmentedThing];*)
(*  minimizeSetup = {almostL1Norm, augmentedThing[[1]] == 0};*)
(*  solution = NMinimize[minimizeSetup, generatorsTuningMap, WorkingPrecision -> 128];*)
(*  *)
(*  generatorsTuningMap /. Last[solution] // N*)
(*];*)

