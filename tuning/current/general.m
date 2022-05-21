(* SHARED *)

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

getSumDamage[tuningMap_, tuningOptions_] := Total[getTargetedIntervalDamagesL[tuningMap, tuningOptions]];
Square[n_] := n^2;
get2SumDamage[tuningMap_, tuningOptions_] := Total[Square[getTargetedIntervalDamagesL[tuningMap, tuningOptions]]];
getPowerSumDamage[tuningMap_, tuningOptions_, power_] := Total[Power[getTargetedIntervalDamagesL[tuningMap, tuningOptions], power]];
getMaxDamage[tuningMap_, tuningOptions_] := Max[getTargetedIntervalDamagesL[tuningMap, tuningOptions]];

getTargetedIntervalDamagesL[tuningMap_, tuningOptions_] := Module[
  {
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart,
    
    temperedSide,
    justSide
  },
  
  {
    temperedSideGeneratorsPart,
    temperedSideMappingPart,
    justSideGeneratorsPart,
    justSideMappingPart,
    eitherSideIntervalsPart,
    eitherSideMultiplierPart
  } = getParts[tuningOptions];
  
  temperedSide = First[{tuningMap / 1200}.eitherSideIntervalsPart.eitherSideMultiplierPart];
  justSide = First[getSide[justSideGeneratorsPart, justSideMappingPart, eitherSideIntervalsPart, eitherSideMultiplierPart]];
  
  getAbsErrors[temperedSide, justSide]
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
