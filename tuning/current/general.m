(* SHARED *)

tuningOptions = {
  "unchangedIntervals" -> {}, (* trait -1 *)
  "targetedIntervals" -> Null, (* trait 0 *)
  "optimizationPower" -> Null, (* trait 1: \[Infinity] = minimax, 2 = minisos, 1 = minisum *)
  "damageWeightingSlope" -> "", (* trait 2: unweighted, complexity-weighted, or simplicity-weighted *)
  "complexityNormPower" -> 1, (* trait 3: what Mike Battaglia refers to as `p` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space *)
  "complexityNegateLogPrimeCoordination" -> False, (* trait 4a: False = do nothing, True = negate the multiplication by logs of primes *)
  "complexityPrimePower" -> 0, (* trait 4b: what Mike Battaglia refers to as `s` in https://en.xen.wiki/w/BOP_tuning; 0 = nothing, equiv to copfr when log prime coordination is negated and otherwise defaults; 1 = product complexity, equiv to sopfr when log prime coordination is negated and otherwise defaults; >1 = pth power of those *)
  "complexitySizeFactor" -> 0, (* trait 4c: what Mike Battaglia refers to as `k` in https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space; 0 = no augmentation to factor in span, 1 = Weil style, etc. *)
  "complexityMakeOdd" -> False, (* trait 4d: False = do nothing, True = achieve Kees from Weil, KE from WE, etc. *)
  "tuningIntervalBasis" -> "primes", (* Graham Breed calls this "inharmonic" vs "subgroup" notion in the context of TE tuning, but it can be used for any tuning *)
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
  inputComplexityNegateLogPrimeCoordination_, (* trait 4a *)
  inputComplexityPrimePower_, (* trait 4b *)
  inputComplexitySizeFactor_, (* trait 4c *)
  inputComplexityMakeOdd_, (* trait 4d *)
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
    tPossiblyWithChangedIntervalBasis,
    targetedIntervalsA,
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
  complexityNegateLogPrimeCoordination = inputComplexityNegateLogPrimeCoordination; (* trait 4a *)
  complexityPrimePower = inputComplexityPrimePower; (* trait 4b *)
  complexitySizeFactor = inputComplexitySizeFactor; (* trait 4c *)
  complexityMakeOdd = inputComplexityMakeOdd; (* trait 4d *)
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
  
  (* trait 0 *)
  If[
    StringMatchQ[systematicTuningName, "*targeting-all*"],
    targetedIntervals = {};
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
    complexityNegateLogPrimeCoordination = True;
  ];
  If[
    StringMatchQ[systematicTuningName, "*P*"] || StringMatchQ[systematicComplexityName, "*P*"],
    complexityPrimePower = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Z*"] || StringMatchQ[systematicComplexityName, "*Z*"],
    complexitySizeFactor = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*Q*"] || StringMatchQ[systematicComplexityName, "*Q*"],
    complexityMakeOdd = True;
  ];
  
  (* Note: we can't implement product complexity with the current design, and don't intend to revise.
   This is because product complexity is realized from a PC-vector as a product of terms,
    raised to the powers of the absolute values of the entries. But this design only multiplies entries and sums them. 
    Since sopfr achieves the same tuning, we simply treat that sopfr as the canonical approach for this effect. *)
  If[
    originalComplexityName === "copfr" || originalComplexityName === "l1Norm",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "sopfr" || originalComplexityName === "wilsonHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexityPrimePower = 1; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "integerLimit" || originalComplexityName === "weilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "oddLimit" || originalComplexityName === "keesHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 0; complexityPrimePower = 0; complexitySizeFactor = 1; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "logProduct" || originalComplexityName === "tenneyHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 1; complexityPrimePower = 0; complexitySizeFactor = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logIntegerLimit" || originalComplexityName === "logarithmicWeilHeight",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "logOddLimit" || originalComplexityName === "keesExpressibility",
    complexityNormPower = 1; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  If[
    originalComplexityName === "rososcopfr" || originalComplexityName === "l2Norm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "rosossopfr",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 1; complexityMakeOdd = False;
  ];
  (* (following the pattern here, this one might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ]; *)
  (* (following the pattern here, this one might exist, but it has not been described or named) If[
    ,
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ]; *)
  If[
    originalComplexityName === "tenneyEuclideanHeight",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 0;  complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "weilEuclideanNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = False;
  ];
  If[
    originalComplexityName === "keesEuclideanSeminorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 1; complexitySizeFactor = 1; complexityPrimePower = 0; complexityMakeOdd = True;
  ];
  (* This one doesn't follow the above patterns as closely.
   See: https://www.facebook.com/groups/xenharmonicmath/posts/1426449464161938/?comment_id=1426451087495109&reply_comment_id=1426470850826466 *)
  If[
    originalComplexityName === "carlsNorm",
    complexityNormPower = 2; complexityNegateLogPrimeCoordination = 0; complexitySizeFactor = 0; complexityPrimePower = 2; complexityMakeOdd = False;
  ];
  
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
    targetedIntervalsA = If[targetedIntervals === Null, getDiamond[getD[tPossiblyWithChangedIntervalBasis]], If[Length[targetedIntervals] == 0, If[forDamage, getFormalPrimesA[tPossiblyWithChangedIntervalBasis], {}], intervalRebase.getA[targetedIntervals]]],
    
    tPossiblyWithChangedIntervalBasis = t;
    targetedIntervalsA = If[targetedIntervals === Null, getDiamond[getD[tPossiblyWithChangedIntervalBasis]], If[Length[targetedIntervals] == 0, If[forDamage, getFormalPrimesA[tPossiblyWithChangedIntervalBasis], {}], getA[targetedIntervals]]];
  ];
  
  If[!NumericQ[optimizationPower] && optimizationPower != \[Infinity], Throw["no optimization power"]];
  If[damageWeightingSlope == "", Throw["no damage weighting slope"]];
  
  {
    tPossiblyWithChangedIntervalBasis,
    unchangedIntervals, (* trait -1 *)
    targetedIntervalsA, (* trait 0 *)
    optimizationPower, (* trait 1 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd, (* trait 4d *)
    pureOctaveStretch
  }
];

tuningOption[tuningOptions_, optionName_] := Module[
  {tuningOptionsPartsByOptionName},
  
  tuningOptionsPartsByOptionName = <|
    "t" -> 1,
    "unchangedIntervals" -> 2, (* trait -1 *)
    "targetedIntervalsA" -> 3, (* trait 0 *)
    "optimizationPower" -> 4, (* trait 1 *)
    "damageWeightingSlope" -> 5, (* trait 2 *)
    "complexityNormPower" -> 6, (* trait 3 *)
    "complexityNegateLogPrimeCoordination" -> 7, (* trait 4a *)
    "complexityPrimePower" -> 8, (* trait 4b *)
    "complexitySizeFactor" -> 9, (* trait 4c *)
    "complexityMakeOdd" -> 10, (* trait 4d *)
    "pureOctaveStretch" -> 11
  |>;
  
  Part[tuningOptions, tuningOptionsPartsByOptionName[optionName]]
];

getPrimesTuningMap[t_] := Log2[getIntervalBasis[t]];

getLogPrimeCoordinationA[t_] := DiagonalMatrix[Log2[getIntervalBasis[t]]];

getPeriodsPerOctave[t_] := First[First[getA[getM[t]]]];

getPrimesIdentityA[t_] := IdentityMatrix[getD[t]];

getTuningMappings[t_] := Module[
  {generatorsTuningMap, ma, tuningMap, primesTuningMap},
  
  generatorsTuningMap = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tuningMap = generatorsTuningMap.ma;
  primesTuningMap = getPrimesTuningMap[t];
  
  {generatorsTuningMap, ma, tuningMap, primesTuningMap}
];


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



(* TARGETED INTERVAL SETS *)

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
