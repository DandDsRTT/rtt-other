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
        optimizeGtm[meantoneM]
    
  Out   {1200., 696.578}
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        optimizeGtm[meantoneM, "originalTuningName" -> "TOP"]
    
  Out   {1201.7, 697.564}
  
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
    tuningOptions,
    optimizedGtm
  },
  
  optimizationPower = OptionValue["optimizationPower"];
  damageWeightingSlope = OptionValue["damageWeightingSlope"];
  complexityUnitsMultiplier = OptionValue["complexityUnitsMultiplier"];
  complexityNormPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  tuningIntervalBasis = OptionValue["tuningIntervalBasis"];
  systematicTuningName = OptionValue["systematicTuningName"];
  originalTuningName = OptionValue["originalTuningName"];
  
  (* TODO: figure out why my defaulting of this final arg doesn't work anymore ... Wolfram Lnaguage bug? I mean obviously I've removed it now due to other stuff, but see if you can't put it back, I bet you can't. maybe it's like a max parameter list size or something crazy and undocumented like that *)
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, tim, tuningIntervalBasis, systematicTuningName, originalTuningName, False];
  optimizationPower = First[tuningOptions];
  
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
    retrievePrimesIntervalBasisGtm[optimizedGtm, t, tuningOptions],
    optimizedGtm
  ]
];




(* ___ PRIVATE ___ *)


(* MINIMAX *)

optimizeGtmMinimax[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_}] := If[
  damageWeightingSlope == "simplicityWeighted" && Length[tima] == 0,
  optimizeGtmTargetingAll[d, t, ptm, complexityUnitsMultiplier, complexityNormPower],
  If[
    damageWeightingSlope == "unweighted",
    optimizeGtmMinimaxTargetingListAnalytical[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower],
    optimizeGtmMinimaxTargetingListNumerical[tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]
  ]
];


(* TARGETING-ALL MINIMAX *)

optimizeGtmTargetingAll[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_] := If[
  complexityNormPower == 2,
  optimizeGtmTargetingAllPseudoInverseAnalytical[d, t, ptm, complexityUnitsMultiplier],
  optimizeGtmTargetingAllSolverNumerical[d, t, ptm, complexityUnitsMultiplier, complexityNormPower]
];

optimizeGtmTargetingAllPseudoInverseAnalytical[d_, t_, ptm_, complexityUnitsMultiplier_] := Module[{w, tima},
  w = If[complexityUnitsMultiplier == "standardized", 1 / ptm, Table[1, d]];
  tima = IdentityMatrix[d];
  
  optimizeGtmWithPseudoInverse[tima, w, t, ptm]
];

optimizeGtmTargetingAllSolverNumerical[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{gtm, ma, tm},
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  tm = gtm.ma;
  
  If[
    (* covers Weil, does the max - min special augmented norm-like thing *)
    complexityUnitsMultiplier == "logIntegerLimit",
    optimizeGtmTargetingAllSolverNumericalAlmostL1StyleLogIntegerLimit[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm],
    If[
      (* covers Kees, does the max - min special augmented norm-like thing like Weil, but with no 2's *)
      complexityUnitsMultiplier == "logOddLimit",
      optimizeGtmTargetingAllSolverNumericalAlmostL1StyleLogOddLimit[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm],
      optimizeGtmTargetingAllSolverNumericalL1Style[d, t, ptm, complexityUnitsMultiplier, complexityNormPower, gtm, tm]
    ]
  ]
];

optimizeGtmTargetingAllSolverNumericalL1Style[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[{e, solution},
  e = If[
    complexityUnitsMultiplier == "standardized",
    tm / ptm - Table[1, d],
    If[
      complexityUnitsMultiplier == "sopfr",
      (tm - ptm) * (1 / Map[getSopfrComplexity[#, t]&, IdentityMatrix[d]]),
      tm - ptm
    ]
  ];
  
  (* TODO: note that this is power is always 1, therefore dual used below is \[Infinity], because in here we're always minimaxing damage *)
  
  solution = NMinimize[Norm[e, dualPower[complexityNormPower]], gtm, Method -> "NelderMead", WorkingPrecision -> 15];
  gtm /. Last[solution] // N
];

optimizeGtmTargetingAllSolverNumericalAlmostL1StyleLogIntegerLimit[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[{augmentedThing, logIntegerLimitNorm, solution, middleMan},
  middleMan = tm / ptm - Table[1, d]; (* TODO: Note: currently weighted *)
  augmentedThing = AppendTo[middleMan, 0];
  logIntegerLimitNorm = Max[augmentedThing] - Min[augmentedThing];
  solution = NMinimize[logIntegerLimitNorm, gtm, Method -> "NelderMead", WorkingPrecision -> 15];
  gtm /. Last[solution] // N
];

optimizeGtmTargetingAllSolverNumericalAlmostL1StyleLogOddLimit[d_, t_, ptm_, complexityUnitsMultiplier_, complexityNormPower_, gtm_, tm_] := Module[{augmentedThing, logOddLimitNorm, solution, middleMan},
  middleMan = Drop[tm - ptm, 1]; (* TODO: Note: currently NOT weighted *)
  augmentedThing = AppendTo[middleMan, 0];
  logOddLimitNorm = Max[augmentedThing] - Min[augmentedThing];
  solution = NMinimize[logOddLimitNorm, gtm, Method -> "NelderMead", WorkingPrecision -> 15];
  gtm /. Last[solution] // N
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];


(* TARGETING-LIST MINIMAX *)

optimizeGtmMinimaxTargetingListAnalytical[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, getMaxDamage];

optimizeGtmMinimaxTargetingListNumerical[tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[
  {
    gtm,
    ma,
    mappedTima,
    pureTimaSizes,
    w,
    solution
  },
  
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
  ma = getA[getM[t]];
  
  mappedTima = Transpose[ma.Transpose[tima]];
  pureTimaSizes = Map[ptm.#&, tima];
  w = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  solution = NMinimize[
    Max[
      MapIndexed[
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
          ] * w[[tiIndex]]
        ],
        mappedTima
      ]
    ],
    gtm,
    Method -> "NelderMead",
    WorkingPrecision -> 15
  ];
  
  gtm /. Last[solution] // N
];


(* MINISOS *)

optimizeGtmMinisos[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_}] := Module[{w},
  w = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  optimizeGtmWithPseudoInverse[tima, w, t, ptm]
];

optimizeGtmWithPseudoInverse[tima_, w_, t_, ptm_] := Module[{ma, weightingMatrix, weightedTimaMapped, g, gtm},
  ma = getA[getM[t]];
  weightingMatrix = DiagonalMatrix[w];
  weightedTimaMapped = ma.Transpose[tima].weightingMatrix;
  (* TODO: if we're constantly transposing tima, maybe just do it once up front? or have getA respect the co/contra? *)
  g = Transpose[tima].weightingMatrix.Transpose[weightedTimaMapped].Inverse[weightedTimaMapped.Transpose[weightedTimaMapped]];
  gtm = ptm.g;
  gtm // N
];


(* MINISUM *)

optimizeGtmMinisum[{optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_}] :=
    optimizeGtmSimplex[optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, getSumDamage];


(* SIMPLEX (USED BY ANALYTICAL MINIMAX AND MINISUM) *)

optimizeGtmSimplex[optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_, damageMean_] := Module[
  {
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialPs,
    potentialTms,
    meanOfDamages,
    minMeanIndices,
    minMeanIndex,
    tiedTms,
    tiedPs,
    minMeanP,
    gpt,
    projectedGenerators
  },
  
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
  potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
  potentialPs = Select[Map[getPFromUnchangedIntervals[t, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
  potentialTms = Map[ptm.#&, potentialPs];
  meanOfDamages = Map[damageMean[#, {optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower}]&, potentialTms];
  
  minMeanIndices = Position[meanOfDamages, Min[meanOfDamages]];
  If[
    Length[minMeanIndices] == 1,
    
    minMeanIndex = First[First[Position[meanOfDamages, Min[meanOfDamages]]]];
    minMeanP = potentialPs[[minMeanIndex]],
    
    tiedTms = Part[potentialTms, Flatten[minMeanIndices]];
    tiedPs = Part[potentialPs, Flatten[minMeanIndices]];
    minMeanIndex = tieBreak[tiedTms, optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
    minMeanP = tiedPs[[minMeanIndex]]
  ];
  
  gpt = Transpose[getA[getGpt[t]]];
  projectedGenerators = minMeanP.gpt;
  ptm.projectedGenerators // N
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
  
  tuningOptions = processTuningOptions[t, optimizationPower, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower, tim, tuningIntervalBasis, systematicTuningName, originalTuningName, True];
  optimizationPower = First[tuningOptions];
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
  w = getW[t, tima, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower];
  
  e * w
];

Square[n_] := n^2;

getSumDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_}] :=
    Total[Map[Abs, getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]]];

get2SumDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_}] :=
    Total[Map[Square, getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]]];

getMaxDamage[tm_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_}] :=
    Max[Map[Abs, getTid[t, tm, tima, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower]]];


(* AKA "Benedetti height" *)
getProductComplexity[pcv_, t_] := Times @@ MapThread[#1^Abs[#2]&, {getB[t], pcv}];

(* AKA "Wilson height" *)
getSopfrComplexity[pcv_, t_] := Total[MapThread[#1 * Abs[#2]&, {getB[t], pcv}]];

(* This just gives TOP originalTuningName *)
getLogSopfrComplexity[pcv_, t_] := Log[2, getSopfrComplexity[pcv, t]];

(* AKA "Weil height" *)
getIntegerLimitComplexity[pcv_, t_] := Module[{rational},
  (* TODO: pcvToRational doesn't support nonstandard interval bases yet; should move this there *)
  rational = pcvToRational[pcv];
  Max[Numerator[rational], Denominator[rational]]
];

(* AKA "Kees height" *)
noTwos[pcv_] := MapIndexed[If[First[#2] == 1, 0, #1]&, pcv];
getOddLimitComplexity[pcv_, t_] := getIntegerLimitComplexity[noTwos[pcv], t];

tieBreak[tiedTms_, optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{meanOfDamages},
  meanOfDamages = Map[get2SumDamage[#, {optimizationPower, tima, d, t, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower}]&, tiedTms];
  
  First[First[Position[meanOfDamages, Min[meanOfDamages]]]]
];


(* INTERVAL BASIS *)

retrievePrimesIntervalBasisGtm[optimizedGtm_, originalT_, {optimizationPower_, tima_, d_, t_, ptm_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_}] := Module[
  {ma, optimizedTm, gpt, f},
  
  ma = getA[getM[t]];
  optimizedTm = optimizedGtm.ma;
  gpt = Transpose[getA[getGpt[originalT]]];
  f = Transpose[getF[originalT]];
  
  optimizedTm.f.gpt
];


(* SHARED *)

tuningOptions = {
  "optimizationPower" -> Null,
  "damageWeightingSlope" -> "",
  "complexityUnitsMultiplier" -> "standardized",
  "complexityNormPower" -> 1,
  "tim" -> Null,
  "tuningIntervalBasis" -> "primes",
  "systematicTuningName" -> "",
  "originalTuningName" -> ""
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
  forDamage_
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
  
  If[
    originalTuningName === "minimax",
    optimizationPower = 1; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "least squares",
    optimizationPower = 2; damageWeightingSlope = "unweighted";
  ];
  If[
    originalTuningName === "TOP" || originalTuningName === "TIPTOP" || originalTuningName === "Tenney OPtimal",
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
    originalTuningName === "BE",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "product";
  ];
  If[
    originalTuningName === "Kees",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 1; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "KE",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logOddLimit";
  ];
  If[
    originalTuningName === "Weil",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logIntegerLimit";
  ];
  If[
    originalTuningName === "WE",
    tim = {}; optimizationPower = \[Infinity]; damageWeightingSlope = "simplicityWeighted"; complexityNormPower = 2; complexityUnitsMultiplier = "logIntegerLimit";
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
  
  {optimizationPower, tima, d, tPossiblyWithChangedIntervalBasis, ptm, damageWeightingSlope, complexityUnitsMultiplier, complexityNormPower}
];

getPtm[t_] := Log[2, getB[t]];

getW[t_, tima_, damageWeightingSlope_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{w},
  w = If[
    damageWeightingSlope != "unweighted",
    Map[getComplexity[#, t, complexityUnitsMultiplier, complexityNormPower]&, tima],
    Map[1&, tima]
  ];
  
  If[damageWeightingSlope == "simplicityWeighted", 1 / w, w]
];

getComplexity[pcv_, t_, complexityUnitsMultiplier_, complexityNormPower_] := Module[{weightedPcv},
  weightedPcv = If[complexityUnitsMultiplier == "standardized", pcv * getPtm[t], pcv];
  Norm[weightedPcv, complexityNormPower]
];

(* TODO: how do I not have this already? *)
getF[t_] := Module[{b},
  b = getB[t];
  padD[Map[rationalToPcv, b], getDp[b]]
];
