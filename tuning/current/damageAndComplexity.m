(* DAMAGE *)

Options[getDamage] = tuningOptions;
getDamage[t_, generatorsTuningMap_, OptionsPattern[]] := Module[
  {
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    tuningMap,
    tuningOptions,
    targetedIntervalsA,
    forDamage,
    tPossiblyWithChangedIntervalBasis
  },
  
  unchangedIntervals = OptionValue["unchangedIntervals"]; (* trait -1 *)
  targetedIntervals = OptionValue["targetedIntervals"]; (* trait 0 *)
  optimizationPower = OptionValue["optimizationPower"]; (* trait 1 *)
  damageWeightingSlope = OptionValue["damageWeightingSlope"]; (* trait 2 *)
  complexityNormPower = OptionValue["complexityNormPower"]; (* trait 3 *)
  complexityNegateLogPrimeCoordination = OptionValue["complexityNegateLogPrimeCoordination"]; (* trait 4a *)
  complexityPrimePower = OptionValue["complexityPrimePower"]; (* trait 4b *)
  complexitySizeFactor = OptionValue["complexitySizeFactor"]; (* trait 4c *)
  complexityMakeOdd = OptionValue["complexityMakeOdd"]; (* trait 4d *)
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  systematicComplexityName = OptionValue["systematicComplexityName"];
  originalComplexityName = OptionValue["originalComplexityName"];
  
  forDamage = True;
  
  tuningOptions = processTuningOptions[
    t,
    unchangedIntervals, (* trait -1 *)
    targetedIntervals, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    tuningIntervalBasis,
    pureOctaveStretch,
    systematicTuningName,
    originalTuningName,
    systematicComplexityName,
    originalComplexityName,
    forDamage
  ];
  
  optimizationPower = tuningOption[tuningOptions, "optimizationPower"];
  
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

(* compare with getDualMultipliedPrimesErrorL *)
getTargetedIntervalDamagesL[tuningMap_, tuningOptions_] := Module[
  {t, targetedIntervalsA, primesTuningMap, damageWeights},
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  primesTuningMap = getPrimesTuningMap[t];
  damageWeights = getDamageWeights[tuningOptions];
  
  (* TODO: yeah now you really need to clarify the relation between this and the WorkingPrecision *)
  Abs[N[tuningMap - primesTuningMap, 256].Transpose[targetedIntervalsA].damageWeights]
];

Square[n_] := n^2;

getSumDamage[tuningMap_, tuningOptions_] := Total[getTargetedIntervalDamagesL[tuningMap, tuningOptions]];
get2SumDamage[tuningMap_, tuningOptions_] := Total[Square[getTargetedIntervalDamagesL[tuningMap, tuningOptions]]];
getPowerSumDamage[tuningMap_, tuningOptions_, power_] := Total[Power[getTargetedIntervalDamagesL[tuningMap, tuningOptions], power]];
getMaxDamage[tuningMap_, tuningOptions_] := Max[getTargetedIntervalDamagesL[tuningMap, tuningOptions]];

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
  complexityNormPower = tuningOption[tuningOptions, "complexityNormPower"];
  complexityNegateLogPrimeCoordination = tuningOption[tuningOptions, "complexityNegateLogPrimeCoordination"];
  complexityPrimePower = tuningOption[tuningOptions, "complexityPrimePower"];
  complexitySizeFactor = tuningOption[tuningOptions, "complexitySizeFactor"];
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"];
  
  damageWeights = If[
    damageWeightingSlope == "unweighted",
    
    IdentityMatrix[Length[targetedIntervalsA]],
    
    DiagonalMatrix[Map[Function[
      {targetedInterval},
      getComplexity[
        targetedInterval,
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
    PseudoInverse[damageWeights],
    damageWeights
  ]
];


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
(* AKA "Wilson height", can also be used to find BOP tuning *)
getPcvSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getIntervalBasis[t], pcv}]];
(* This apparently doesn't have a name, but can also be used to find TOP tuning *)
getPcvLogSopfrComplexity[pcv_, t_] := Log2[getPcvSopfrComplexity[pcv, t]];
(* AKA "Weil height" *)
getPcvIntegerLimitComplexity[pcv_, t_] := Module[{quotient},
  quotient = pcvToQuotient[pcv];
  Max[Numerator[quotient], Denominator[quotient]]
];
(* AKA "logarithmic Weil height", used for "Weil tuning" *)
getPcvLogIntegerLimitComplexity[pcv_, t_] := Log2[getPcvIntegerLimitComplexity[pcv, t]];
(* AKA "Kees height" *)
removePowersOfTwoFromPcv[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getPcvOddLimitComplexity[pcv_, t_] := getPcvIntegerLimitComplexity[removePowersOfTwoFromPcv[pcv], t];
(* AKA "Kees expressibility", used for "Kees tuning" *)
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
  (* When used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm, covers TOP; 
when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers TE; 
when used by getDamageWeights covers any targeting-list tuning using this as its damage's complexity *)
  complexityMultiplier = getLogPrimeCoordinationA[t]; (* TODO: wait a sec.. I thought the idea was that we do this OUTSIDE OF THIS *)
  
  If[
    (* When used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm,
    covers L1 version of Frobenius;
    when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm,
    covers Frobenius *)
    complexityNegateLogPrimeCoordination == True,
    complexityMultiplier = complexityMultiplier.Inverse[getLogPrimeCoordinationA[t]]
  ];
  
  If[
    (* When used by getDualMultiplier for optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm, covers BOP;
    when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers BE *)
    complexityPrimePower > 0,
    complexityMultiplier = complexityMultiplier.DiagonalMatrix[Power[getIntervalBasis[t], complexityPrimePower]]
  ];
  
  If[
    (* When Weil needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the min - max thing); 
    when used by getDualMultiplier for optimizeGeneratorsTuningMapPrimesEuclideanNorm, covers WE or KE
    (surprisingly KE does not use the below; it instead uses this and applies an unchanged octave constraint); 
    when used by getDamageWeights should cover any targeting-list tuning using this as its damage's complexity *)
    complexitySizeFactor > 0,
    complexityMultiplier = (Join[getPrimesIdentityA[t], {Table[complexitySizeFactor, getD[t]]}] / (1 + complexitySizeFactor)).complexityMultiplier
  ];
  
  If[
    (* When Kees needs its dual norm, we actually go into optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm, 
    where it's implemented separately (the min - max thing) with pure-octave constraint on the solver; 
    note again that this is is not used for KE; see note above;
    when used by getDamageWeights should cover any targeting-list tuning using this as its damage's complexity *)
    complexityMakeOdd == True,
    complexityMultiplier = complexityMultiplier.DiagonalMatrix[Join[{0}, Table[1, getD[t] - 1]]]
  ];
  
  complexityMultiplier
];
