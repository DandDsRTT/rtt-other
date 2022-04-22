(*
  
  GENERATORS PREIMAGE TRANSVERSAL
  
  
  getGpt[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns a generators preimage transversal 
  (for each generator, one JI interval that maps to it).
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        getGpt[meantoneM]
    
  Out   {{{1, 0, 0}, {-1, 1, 0}}, "contra"}
  
*)
getGpt[t_] := Module[{ma, decomp, left, snf, right, gpt},
  ma = getA[getM[t]];
  decomp = SmithDecomposition[ma];
  left = Part[decomp, 1];
  snf = Part[decomp, 2];
  right = Part[decomp, 3];
  
  gpt = right.Transpose[snf].left;
  
  {Transpose[gpt], "contra"}
];


(*
  
  TUNING
  
  
  optimizeGtm[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimal generator tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 697.563}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 695.294}
*)
Options[optimizeGtm] = tuningOptions;
optimizeGtm[t_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    tuningOptions,
    optimizedGtm,
    tPossiblyWithChangedIntervalBasis
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, tim, tuningIntervalBasis, systematicTuningName, originalTuningName, pureOctaveStretch];
  optimizationPower = Part[tuningOptions, 1];
  tPossiblyWithChangedIntervalBasis = Part[tuningOptions, 4];
  pureOctaveStretch = Part[tuningOptions, 9];
  
  optimizedGtm = 1200 * If[
    optimizationPower == \[Infinity],
    optimizeGtmMinimax[tuningOptions],
    If[
      optimizationPower == 2,
      optimizeGtmMinisos[tuningOptions],
      optimizeGtmMinisum[tuningOptions]
    ]
  ];
  
  If[
    !isStandardPrimeLimitB[getB[t]] && tuningIntervalBasis == "primes",
    optimizedGtm = retrievePrimesIntervalBasisGtm[optimizedGtm, t, tPossiblyWithChangedIntervalBasis]
  ];
  
  If[
    pureOctaveStretch,
    optimizedGtm = getPureOctaveStretchedGtm[optimizedGtm, t]
  ];
  
  optimizedGtm
];


(*
  optimizeTm[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns the optimal tuning map.
  
  The tuning may be specified by original name, systematic name, or by individual parameters.
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTm[meantoneM, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted"]
    
  Out   {1201.69, 1899.26, 2790.25}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTm[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 1899.26, 2790.25} 
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeTm[meantoneM, "systematicTuningName" -> "minisos-MEC"]
    
  Out   {1198.24, 1893.54, 2781.18} 
*)
Options[optimizeTm] = tuningOptions;
optimizeTm[t_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  
  optimizeGtm[t, {
    "optimizationPower" -> optimizationPower,
    "damageWeightingSlope" -> damageWeightingSlope,
    "complexityUnitsMultiplier" -> complexityUnitsMultiplier,
    "complexityNormPower" -> complexityNormPower,
    "tim" -> tim,
    "tuningIntervalBasis" -> tuningIntervalBasis,
    "systematicTuningName" -> systematicTuningName,
    "originalTuningName" -> originalTuningName,
    "pureOctaveStretch" -> pureOctaveStretch
  }].getA[getM[t]]
];


(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGtmMinimax[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] := If[
  damageWeightingSlope == "simplicityWeighted" && Length[tima] == 0,
  optimizeGtmTargetingAll[d, t, ptm, complexityUnitsMultiplier, complexityNormPower],
  If[
    damageWeightingSlope == "unweighted",
    optimizeGtmMinimaxTargetingListAnalytical[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch],
    optimizeGtmTargetingListNumerical[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
  ]
];


(* TARGETING-ALL MINIMAX *)

optimizeGtmTargetingAll[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_] := If[
  complexityNormPower == 2,
  optimizeGtmTargetingAllPseudoInverseAnalytical[d, t, ptm, complexityUnitsMultiplier],
  optimizeGtmTargetingAllNumerical[d, t, ptm, complexityUnitsMultiplier, complexityNormPower]
];

optimizeGtmTargetingAllPseudoInverseAnalytical[d_, t_, ptm_, complexityUnitsMultiplier_] := Module[{Wₚ, tima, damageWeightingSlope, complexityNormPower},
  tima = IdentityMatrix[d];
  damageWeightingSlope = "simplicityWeighted";
  complexityNormPower = 2; (* TODO: is it cleaner to just pass these in ? *)
  Wₚ = getWₚ[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  optimizeGtmWithPseudoInverse[tima, Wₚ, t, ptm]
];

optimizeGtmTargetingAllNumerical[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{gtm, ma, tm},
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tm = gtm.ma;
  
  If[
    complexityUnitsMultiplier == "logIntegerLimit" || complexityUnitsMultiplier == "logOddLimit",
    (* covers Weil and Kees *)
    optimizeGtmTargetingAllNumericalAlmostL1Style[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm],
    (* covers TOP, BOP, and L1-version of Frobenius *)
    optimizeGtmTargetingAllNumericalL1Style[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm]
  ]
];

optimizeGtmTargetingAllNumericalL1Style[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[
  {
    eₚ,
    Wₚ,
    damageWeightingSlope,
    tima,
    solution,
    previousSolution,
    optimizationPower,
    previousPrimesErrorMagnitude,
    primesErrorMagnitude,
    normPowerPower,
    normPower
  },
  
  eₚ = tm - ptm;
  damageWeightingSlope = "simplicityWeighted";
  tima = IdentityMatrix[d];
  Wₚ = getWₚ[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  eₚ = Wₚ.eₚ;
  optimizationPower = dualPower[complexityNormPower];
  previousPrimesErrorMagnitude = \[Infinity];
  primesErrorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  
  While[
    normPowerPower <= 10 && previousPrimesErrorMagnitude - primesErrorMagnitude > 0,
    
    previousPrimesErrorMagnitude = primesErrorMagnitude;
    previousSolution = solution;
    solution = NMinimize[Norm[eₚ, normPower], gtm, WorkingPrecision -> 128];
    primesErrorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  gtm /. Last[previousSolution] // N
];

optimizeGtmTargetingAllNumericalAlmostL1Style[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[
  {augmentedThing, almostL1Norm, solution, middleMan, minimizeSetup},
  
  middleMan = tm / ptm - Table[1, d];
  augmentedThing = AppendTo[middleMan, 0];
  almostL1Norm = Max[augmentedThing] - Min[augmentedThing];
  minimizeSetup = If[complexityUnitsMultiplier == "logOddLimit", {almostL1Norm, augmentedThing[[1]] == 0}, almostL1Norm];
  solution = NMinimize[{almostL1Norm, augmentedThing[[1]] == 0}, gtm, WorkingPrecision -> 128];
  
  gtm /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];


(* TARGETING-LIST MINIMAX *)

optimizeGtmMinimaxTargetingListAnalytical[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch];


(* MINISOS *)

optimizeGtmMinisos[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] := Module[{W},
  W = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  optimizeGtmWithPseudoInverse[tima, W, t, ptm]
];

optimizeGtmWithPseudoInverse[tima_, W_, t_, ptm_] := Module[{ma, weightingMatrix, weightedTimaMapped, g, gtm},
  ma = getA[getM[t]];
  weightedTimaMapped = ma.Transpose[tima].W;
  (* TODO: if we're constantly transposing tima, maybe just do it once up front? or have getA respect the co/contra? *)
  g = Transpose[tima].W.Transpose[weightedTimaMapped].Inverse[weightedTimaMapped.Transpose[weightedTimaMapped]];
  gtm = ptm.g;
  gtm // N
];


(* MINISUM *)

optimizeGtmMinisum[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch];


(* SOLVER (USED BY NUMERICAL MINIMAX AND MINISUM) *)

optimizeGtmTargetingListNumerical[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[
  {
    gtm,
    ma,
    mappedTima,
    pureTimaSizes,
    W,
    solution,
    errorMagnitude,
    previousErrorMagnitude,
    normPower,
    normPowerPower,
    e,
    previousSolution
  },
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  pureTimaSizes = Map[ptm.#&, tima];
  W = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  mappedTima = Transpose[ma.Transpose[tima]];
  
  previousErrorMagnitude = \[Infinity];
  errorMagnitude = 1000000;
  normPowerPower = 1;
  normPower = 2;
  e = Flatten[MapIndexed[
    Function[
      {mappedTi, tiIndex},
      Abs[
        Total[
          MapThread[
            Function[
              {mappedTiEntry, gtmEntry},
              mappedTiEntry * gtmEntry
            ],
            {mappedTi, gtm}
          ]
        ] - pureTimaSizes[[tiIndex]]
      ] * Part[Part[W, tiIndex, tiIndex]]
    ],
    mappedTima
  ]];
  
  While[
    normPowerPower <= 10 && previousErrorMagnitude - errorMagnitude > 0,
    
    previousErrorMagnitude = errorMagnitude;
    previousSolution = solution;
    solution = NMinimize[Norm[e, normPower], gtm, WorkingPrecision -> 128];
    errorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[optimizationPower == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  gtm /. Last[previousSolution] // N
];


(* SIMPLEX (USED BY ANALYTICAL MINIMAX AND MINISUM) *)

optimizeGtmSimplex[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_] := Module[
  {
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialPs,
    potentialTms,
    powerSumOfDamages,
    minMeanIndices,
    minMeanIndex,
    minMeanP,
    gpt,
    projectedGenerators,
    damagePowerSum
  },
  
  damagePowerSum = If[optimizationPower == 1, getSumDamage, getMaxDamage];
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromUnchangedIntervals[t, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  potentialTms = Map[ptm.#&, potentialPs];
  powerSumOfDamages = Map[damagePowerSum[#, {optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch}]&, potentialTms];
  
  minMeanIndices = Position[powerSumOfDamages, Min[powerSumOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    
    minMeanIndex = First[First[Position[powerSumOfDamages, Min[powerSumOfDamages]]]];
    minMeanP = potentialPs[[minMeanIndex]];
    gpt = Transpose[getA[getGpt[t]]];
    projectedGenerators = minMeanP.gpt;
    ptm.projectedGenerators // N,
    
    optimizeGtmTargetingListNumerical[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
  ]
];

getPFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[{commaEigenvectors, eigenvectors, diagonalEigenvalueMatrix},
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueMatrix = getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueMatrix.Inverse[eigenvectors]]
];

getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];


(* TARGET INTERVAL SETS *)

getDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  padD[Map[rationalToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;


(* DAMAGE *)

Options[getDamage] = tuningOptions;
getDamage[t_, gtm_, OptionsPattern[]] := Module[
  {
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    ma,
    tm,
    tuningOptions
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  pureOctaveStretch = OptionValue["pureOctaveStretch"];
  
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, tim, tuningIntervalBasis, systematicTuningName, originalTuningName, pureOctaveStretch, True];
  optimizationPower = Part[tuningOptions, 1];
  
  ma = getA[getM[t]];
  tm = (gtm / 1200).ma;
  
  If[
    optimizationPower == \[Infinity],
    getMaxDamage[tm, tuningOptions],
    If[
      optimizationPower == 2,
      get2SumDamage[tm, tuningOptions],
      getSumDamage[tm, tuningOptions]
    ]
  ]
];

getTid[t_, tm_, tima_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{e, w},
  e = N[tm.Transpose[tima]] - N[ptm.Transpose[tima]];
  W = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  Abs[e].W
];

Square[n_] := n^2;

getSumDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    Total[getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]];

get2SumDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    Total[Square[getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]]];

getMaxDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, pureOctaveStretch_}] :=
    Max[getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]];


(* AKA "Benedetti height" *)
getProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getB[t], pcv}];

(* AKA "Tenney height" *)
getLogProductComplexity[pcv_, t_] := Log[2, getProductComplexity[pcv, t]];

(* AKA "Wilson height" *)
getSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getB[t], pcv}]];

(* This just gives TOP tuning *)
getLogSopfrComplexity[pcv_, t_] := Log[2, getSopfrComplexity[pcv, t]];

(* AKA "Weil height" *)
getIntegerLimitComplexity[pcv_, t_] := Module[{rational},
  rational = pcvToRational[pcv];
  Max[Numerator[rational], Denominator[rational]]
];

(* AKA "Kees height" *)
noTwos[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getOddLimitComplexity[pcv_, t_] := getIntegerLimitComplexity[noTwos[pcv], t];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGtm[optimizedGtm_, originalT_, t_] := Module[
  {ma, optimizedTm, gpt, f},
  
  ma = getA[getM[t]];
  optimizedTm = optimizedGtm.ma;
  gpt = Transpose[getA[getGpt[originalT]]];
  f = Transpose[getF[originalT]];
  
  optimizedTm.f.gpt
];


(* PURE-OCTAVE STRETCH *)

getPureOctaveStretchedGtm[optimizedGtm_, t_] := Module[{periodsPerOctave},
  periodsPerOctave = First[First[getA[getM[t]]]];
  
  (1200 / periodsPerOctave) * (optimizedGtm / First[optimizedGtm])
];



(* SHARED *)

tuningOptions = {
  "tim" -> Null, (* trait 0 *)
  "optimizationPower" -> Null, (* trait 1 *)
  "damageWeightingSlope" -> "", (* trait 2 *)
  "complexityNormPower" -> 1, (* trait 3 *)
  "complexityUnitsMultiplier" -> "standardized", (* trait 4 *)
  "tuningIntervalBasis" -> "primes",
  "systematicTuningName" -> "",
  "originalTuningName" -> "",
  "pureOctaveStretch" -> False
};

processTuningOptions[
  t_,
  inputOptimizationPower_,
  inputDamageWeightingSlope_,
  inputComplexityUnitsMultiplier_,
  inputComplexityNormPower_,
  inputTim_,
  inputTuningIntervalBasis_,
  inputSystematicTuningName_,
  inputOriginalTuningName_,
  inputPureOctaveStretch_,
  forDamage_ : False
] := Module[
  {
    tima,
    d,
    ptm,
    optimizationPower,
    damageWeightingSlope,
    complexityUnitsMultiplier,
    complexityNormPower,
    tim,
    tuningIntervalBasis,
    systematicTuningName,
    originalTuningName,
    pureOctaveStretch,
    commaBasisInNonstandardIntervalBasis,
    primeLimitIntervalBasis,
    commaBasisInPrimeLimitIntervalBasis,
    mappingInPrimeLimitIntervalBasis,
    tPossiblyWithChangedIntervalBasis,
    b,
    ir
  },
  
  optimizationPower = inputOptimizationPower;
  damageWeightingSlope = inputDamageWeightingSlope;
  complexityUnitsMultiplier = inputComplexityUnitsMultiplier;
  complexityNormPower = inputComplexityNormPower;
  tim = inputTim;
  tuningIntervalBasis = inputTuningIntervalBasis;
  systematicTuningName = inputSystematicTuningName;
  originalTuningName = inputOriginalTuningName;
  pureOctaveStretch = inputPureOctaveStretch;
  
  If[
    originalTuningName === "minimax",
    optimizationPower = 1; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "TOP" || originalTuningName === "TIPTOP",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "standardized";
  ];
  If[
    originalTuningName === "TE" || originalTuningName === "Tenney-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "standardized";
  ];
  If[
    originalTuningName === "Frobenius",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "unstandardized";
  ];
  If[
    originalTuningName === "BOP",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "product";
  ];
  If[
    originalTuningName === "BE" || originalTuningName === "Benedetti-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "product";
  ];
  If[
    originalTuningName === "Kees",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "KE" || originalTuningName === "Kees-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "Weil",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logIntegerLimit";
  ];
  If[
    originalTuningName === "WE" || originalTuningName === "Weil-Euclidean",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logIntegerLimit";
  ];
  If[
    originalTuningName === "POTOP" || originalTuningName === "POTT",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "standardized"; pureOctaveStretch = True;
  ];
  If[
    originalTuningName === "POTE",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "standardized"; pureOctaveStretch = True;
  ];
  
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
  If[
    StringMatchQ[systematicTuningName, "*E*"],
    complexityNormPower = 2;
  ];
  If[
    StringMatchQ[systematicTuningName, "*T*"],
    complexityNormPower = 1;
  ];
  If[
    StringMatchQ[systematicTuningName, "*M*"],
    complexityUnitsMultiplier = "unstandardized";
  ];
  If[
    StringMatchQ[systematicTuningName, "*O*"],
    complexityUnitsMultiplier = "standardized";
  ];
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
  
  If[!NumericQ[optimizationPower] && optimizationPower != \[Infinity], Throw["no optimization power"]];
  If[damageWeightingSlope == "", Throw["no damage weighting slope"]];
  
  b = getB[t];
  If[
    !isStandardPrimeLimitB[b] && tuningIntervalBasis == "primes",
    
    commaBasisInNonstandardIntervalBasis = getC[t];
    primeLimitIntervalBasis = getPrimes[getDp[b]];
    commaBasisInPrimeLimitIntervalBasis = changeB[commaBasisInNonstandardIntervalBasis, primeLimitIntervalBasis];
    ir = getIrForC[b, primeLimitIntervalBasis];
    tima = If[tim === Null, getDiamond[d], If[Length[tim] == 0, If[forDamage, ir.getA[getC[t]], {}], ir.getA[tim]]];
    mappingInPrimeLimitIntervalBasis = getM[commaBasisInPrimeLimitIntervalBasis];
    tPossiblyWithChangedIntervalBasis = mappingInPrimeLimitIntervalBasis;
    d = getD[tPossiblyWithChangedIntervalBasis];
    ptm = getPtm[tPossiblyWithChangedIntervalBasis],
    
    tPossiblyWithChangedIntervalBasis = t;
    d = getD[tPossiblyWithChangedIntervalBasis];
    ptm = getPtm[tPossiblyWithChangedIntervalBasis];
    tima = If[tim === Null, getDiamond[d], If[Length[tim] == 0, If[forDamage, getA[getC[t]], {}], getA[tim]]];
  ];
  
  {optimizationPower, tima, d, tPossiblyWithChangedIntervalBasis, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, pureOctaveStretch}
];

getPtm[t_] := Log[2, getB[t]];

getW[t_, tima_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{W},
  W = If[
    damageWeightingSlope != "unweighted",
    DiagonalMatrix[Map[getComplexity[#, t, complexityUnitsMultiplier, complexityNormPower]&, tima]],
    IdentityMatrix[Length[tima]]
  ];
  
  If[damageWeightingSlope == "simplicityWeighted", Inverse[W], W]
];

(*TODO: for now while I iron stuff out this is going to look really redundant with getW, but I just want to avoid terminologically confusing myself for now *)
getWₚ[t_, tima_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{d, Wₚ},
  d = getD[t];
  Wₚ = If[
    (* covers TOP *)
    complexityUnitsMultiplier == "standardized",
    Inverse[DiagonalMatrix[Map[getLogProductComplexity[#, t]&, IdentityMatrix[d]]]],
    If[
      (* covers BOP *)
      complexityUnitsMultiplier == "product",
      Inverse[DiagonalMatrix[Map[getProductComplexity[#, t]&, IdentityMatrix[d]]]],
      If[
        (* also covers TOP (equivalent to "standardized") *)
        complexityUnitsMultiplier == "logSopfr",
        Inverse[DiagonalMatrix[ Map[getLogSopfrComplexity[#, t]&, IdentityMatrix[d]]]],
        If[
          (* also covers BOP (equivalent to "product") *)
          complexityUnitsMultiplier == "sopfr",
          Inverse[DiagonalMatrix[Map[getSopfrComplexity[#, t]&, IdentityMatrix[d]]]],
          
          If[
            (* covers Weil *) (* TODO: note that what we're doing here is like, finding the dual weighting; perhaps work that into the name *)
            complexityUnitsMultiplier == "logIntegerLimit",
            PseudoInverse[Join[DiagonalMatrix[Map[getLogProductComplexity[#, t]&, IdentityMatrix[d]]], { Map[getLogProductComplexity[#, meantone]&, IdentityMatrix[d]]}]],
            
            (* covers L1 version of Frobenius *)
            Inverse[DiagonalMatrix[Map[1&, IdentityMatrix[d]]]]
          ]
        ]
      ]
    ]
  ];
  
  (* always simplicity-weighted *)
  Wₚ
];

(* TODO: rename this to like, get normal complexity or something, because this only handles the (un)standardized power norm types. and/or eventually, as you will eventually want to support whichever complexity they want, BOP or Weil or whatever, for like normal targeting-list minimax, etc. something else *)
getComplexity[pcv_, t_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{weightedPcv},
  weightedPcv = If[complexityUnitsMultiplier == "standardized", pcv * getPtm[t], pcv];
  Norm[weightedPcv, complexityNormPower]
];
