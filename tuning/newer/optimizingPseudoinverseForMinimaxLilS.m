(* OVERRIDES *)
(* every one of these user functions has a public and private version. 
the private is consumed by other methods. the public one parses input and formats output. *)
optimizeGeneratorTuningMap[unparsedT_, tuningSchemeSpec_] := formatOutput[optimizeGeneratorTuningMapPrivate[parseTemperamentData[unparsedT], tuningSchemeSpec]];
optimizeGeneratorTuningMapPrivate[t_, tuningSchemeSpec_] := Module[
  {
    forDamage,
    
    tuningSchemeOptions,
    tuningSchemeProperties,
    
    tPossiblyWithChangedDomainBasis,
    targetIntervals,
    unchangedIntervals,
    intervalComplexityNormPreTransformerSizeFactor,
    nonprimeBasisApproach,
    pureStretchedInterval,
    logging,
    quick,
    
    useOnlyUnchangedIntervalsMethod,
    
    tuningMethodArgs,
    powerArg,
    unchangedIntervalsArg,
    
    optimumGeneratorTuningMap
  },
  
  forDamage = False; (* when True, processTargetIntervals sets an empty target-interval set to the primes *)
  
  (* this is how it handles provision of the spec 
  either as a simple string (ID'ing it as either for an original scheme name or for a systematic scheme name) 
  or as an options object, either way converting it to an options object *)
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  (* then this converts that object into "properties", which is similar to "traits"
  but includes the t itself and options for the optimizer not the tuning (e.g. `logging` and `quick`) *)
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  (* mostly we then use the properties to compute args to the tuning method, but we do need several of them here too *)
  tPossiblyWithChangedDomainBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  unchangedIntervals = tuningSchemeProperty[tuningSchemeProperties, "unchangedIntervals"]; (* trait 0 *)
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  nonprimeBasisApproach = tuningSchemeProperty[tuningSchemeProperties, "nonprimeBasisApproach"]; (* trait 7 *)
  pureStretchedInterval = tuningSchemeProperty[tuningSchemeProperties, "pureStretchedInterval"]; (* trait 6 *)
  logging = tuningSchemeProperty[tuningSchemeProperties, "logging"];
  quick = tuningSchemeProperty[tuningSchemeProperties, "quick"];
  
  (* if the count of target-intervals k equals the count of generators (rank) r *)
  useOnlyUnchangedIntervalsMethod = canUseOnlyUnchangedIntervalsMethod[unchangedIntervals, tPossiblyWithChangedDomainBasis];
  
  (* the final transformation of the user input, really, is to take the tuning scheme "properties"
  and convert those into args which are generic to whichever tuning method we end up choosing*)
  tuningMethodArgs = If[
    (* w/o target-intervals, and not the case that we're relying exclusively on unchanged-intervals to use, then it must be all-interval scheme *)
    ToString[targetIntervals] == "Null" && !useOnlyUnchangedIntervalsMethod,
    getAllIntervalTuningSchemeTuningMethodArgs[tuningSchemeProperties],
    getTuningMethodArgs[tuningSchemeProperties]
  ];
  (* generally prefer to wait to unpack these until into the tuning method function, but these two we need here *)
  powerArg = tuningMethodArg[tuningMethodArgs, "powerArg"];
  unchangedIntervalsArg = tuningMethodArg[tuningMethodArgs, "unchangedIntervalsArg"];
  
  optimumGeneratorTuningMap = maxPolytopeMethod[tuningMethodArgs];
  
  (*  Print["checking here: ", optimumGeneratorTuningMap];*)
  
  (* this only happens if the sum polytope method fails to find a unique optimum generator tuning map, or if a computation takes too long *)
  If[
    optimumGeneratorTuningMap == Null,
    If[logging == True, printWrapper["falling back to power limit solver"]];
    optimumGeneratorTuningMap = powerSumLimitMethod[tuningMethodArgs]
  ];
  
  (* for e.g. minimax-lil "Weil" "WE" and pure-stretched-octave minimax-lil-S "Kees" "KE" tunings, remove the junk final entry from the augmentation; 
  I wish this didn't have to bleed up to this level, but better here maybe in one place than in each method individually? *)
  (*  If[
      ToString[targetIntervals] == "Null" && intervalComplexityNormPreTransformerSizeFactor != 0,
      optimumGeneratorTuningMap = rowify[Drop[getL[optimumGeneratorTuningMap], -1]]
    ];*)
  
  If[logging == True, printWrapper["\nSOLUTION FROM METHOD\n", formatOutput[optimumGeneratorTuningMap]]];
  
  (* handle trait 7 - nonprime basis *)
  If[
    !isStandardPrimeLimitDomainBasis[getDomainBasis[t]] && nonprimeBasisApproach == "prime-based",
    optimumGeneratorTuningMap = retrievePrimeDomainBasisGeneratorTuningMap[optimumGeneratorTuningMap, t, tPossiblyWithChangedDomainBasis];
    If[logging == True, printWrapper["\nRESULT AFTER RETURNING TO PRIMES DOMAIN BASIS\n", formatOutput[optimumGeneratorTuningMap]]];
  ];
  
  (* handle trait 6 - pure-stretched interval *)
  If[
    ToString[pureStretchedInterval] != "Null",
    optimumGeneratorTuningMap = getPureStretchedIntervalGeneratorTuningMap[optimumGeneratorTuningMap, t, pureStretchedInterval];
    If[logging == True, printWrapper["\nRESULT AFTER PURE-STRETCHING\n", formatOutput[optimumGeneratorTuningMap]]];
  ];
  
  If[logging == True, printWrapper[""]];
  
  (*  Print["checking here 2: ", optimumGeneratorTuningMap]; *)
  
  optimumGeneratorTuningMap
];
augmentedTemperedSideGeneratorsPartArg[generatorTuningMap_] := generatorTuningMap;
augmentedTemperedSideMappingPartArg[m_, intervalComplexityNormPrescalerSizeFactor_] := m;
augmentedJustSideGeneratorsPartArg[centsConversionAndSummationMapAndLogPrimeA_] := centsConversionAndSummationMapAndLogPrimeA;
augmentedJustSideMappingPartArg[primesI_] := primesI;
augmentedEitherSideIntervalsPartArg[transposedPrimesI_] := transposedPrimesI;
augmentedUnchangedIntervalsArg[unchangedIntervals_] := unchangedIntervals;

(* SETUP *)
getParameterizedPseudoinverseDualPreTransformer[pseudoinverseParameter_, d_] := Module[
  {a},
  
  a = 2 / pseudoinverseParameter;
  
  MapIndexed[
    Function[
      {i},
      Join[
        Map[
          Function[{j}, If[j == i, 2 - a, -a]],
          Range[d]
        ],
        {a}
      ]
    ],
    Range[d]
  ]
];
getSummationMap[d_] := Table[1, d];
getLogPrimeMatrix[d_] := DiagonalMatrix[Log2[getPrimes[d]]];
format = "Wolfram";

(* MAIN FUNCTION *)
checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping[mapping_] := Module[
  {
    d,
    logPrimeMatrix,
    summationMap,
    justMap,
    inverseLogPrimeMatrix,
    parameterizedPseudoinverseDualPreTransformer,
    tuningMap,
    retuningMap,
    minimaxDamage,
    tuningMapForMinMinimaxDamage,
    minMinimaxDamage,
    pseudoinverseParametersForMinMinimaxDamage,
    maxMinimaxDamage,
    pseudoinverseParametersForMaxMinimaxDamage
  },
  
  d = getDPrivate[parseTemperamentData[mapping]];
  logPrimeMatrix = getLogPrimeMatrix[d];
  summationMap = getSummationMap[d];
  justMap = 1200 * summationMap.logPrimeMatrix;
  inverseLogPrimeMatrix = Inverse[logPrimeMatrix];
  
  minMinimaxDamage = Infinity;
  pseudoinverseParametersForMinMinimaxDamage = {};
  maxMinimaxDamage = 0;
  pseudoinverseParametersForMaxMinimaxDamage = {};
  
  Do[
    If[
      pseudoinverseParameter != 0,
      
      parameterizedPseudoinverseDualPreTransformer = getParameterizedPseudoinverseDualPreTransformer[pseudoinverseParameter, d];
      
      (* override with this particular parameterized pseudoinverse *)
      augmentedEitherSideMultiplierPartArg[na_] := {inverseLogPrimeMatrix.parameterizedPseudoinverseDualPreTransformer, "row"};
      
      tuningMap = optimizeTuningMap[mapping, "minimax-lil-S"];
      retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
      minimaxDamage = Max[Abs[retuningMap.inverseLogPrimeMatrix.parameterizedPseudoinverseDualPreTransformer]];
      
      If[
        minimaxDamage > maxMinimaxDamage,
        maxMinimaxDamage = minimaxDamage;
        pseudoinverseParametersForMaxMinimaxDamage = {pseudoinverseParameter},
        If[
          minimaxDamage == maxMinimaxDamage,
          pseudoinverseParametersForMaxMinimaxDamage = Join[pseudoinverseParametersForMaxMinimaxDamage, {pseudoinverseParameter}];
        ]
      ];
      
      If[
        minimaxDamage < minMinimaxDamage,
        minMinimaxDamage = minimaxDamage;
        tuningMapForMinMinimaxDamage = tuningMap;
        pseudoinverseParametersForMinMinimaxDamage = {pseudoinverseParameter},
        If[
          minimaxDamage == minMinimaxDamage,
          pseudoinverseParametersForMinMinimaxDamage = Join[pseudoinverseParametersForMinMinimaxDamage, {pseudoinverseParameter}];
        ]
      ];
      
      (* report *)
      (*Print["pseudoinverseParameter: ", pseudoinverseParameter]; *)
      (* Print["tuningMap: ", tuningMap]; *)
      (* Print["retuningMap: ", retuningMap]; *)
      (* Print["minimaxDamage: ", minimaxDamage]; *)
      (* Print["pseudoinverseParameter: ", pseudoinverseParameter, " minimaxDamage: ", minimaxDamage];*)
    ],
    
    {pseudoinverseParameter, pseudoinverseParameterRange}
  ];
  
  {
    mapping,
    NumberForm[tuningMapForMinMinimaxDamage, {5, 3}],
    minMinimaxDamage,
    pseudoinverseParametersForMinMinimaxDamage,
    maxMinimaxDamage,
    pseudoinverseParametersForMaxMinimaxDamage
  }
];

pseudoinverseParameterRange = Range[4.9, 5.1, 0.001];
checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping["[⟨1 0 0 0 7] ⟨0 1 0 0 1] ⟨0 0 1 0 -1] ⟨0 0 0 1 -1]}"]

(* EXPERIMENTS *)
d3n2mappings = {
  "[⟨12 19 28]}"
  (*"[⟨7 11 16]}",
  "[⟨12 19 28]}",
  "[⟨15 24 35]}",
  "[⟨19 30 44]}",
  "[⟨22 35 51]}",
  "[⟨31 49 72]}",*)
  (*"[⟨34 54 79]}"*)
};
Do[
  Print[checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping[mapping]],
  {mapping, d3n2mappings}
]

d3n1mappings = {
  "[⟨1 1 0] ⟨0 1 4]}"(*,
"[⟨1 0 -4] ⟨0 1 4]}",
"[⟨1 2 4] ⟨0 -1 -4]}",
"[⟨1 2 3] ⟨0 3 5]}",
"[⟨1 2 3] ⟨0 -3 -5]}",
"[⟨1 -1 -2] ⟨0 3 5]}"*)
};
Do[
  Print[checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping[mapping]],
  {mapping, d3n1mappings}
]

checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping["[⟨1 0 -4 -13] ⟨0 1 4 10]}"]

getParameterizedPseudoinverseDualPreTransformer2[pseudoinverseParameters_] := Module[
  {modifiedPseudoinverseParameters},
  
  modifiedPseudoinverseParameters = Map[
    Function[
      {pseudoinverseParameter},
      2 / pseudoinverseParameter
    ],
    pseudoinverseParameters
  ];
  (* Print["modifiedPseudoinverseParameters: ", modifiedPseudoinverseParameters];*)
  
  MapIndexed[
    Function[
      {modifiedPseudoinverseParameter, i},
      Join[
        MapIndexed[
          Function[{na, j}, If[j == i, 2 - modifiedPseudoinverseParameter, -modifiedPseudoinverseParameter]],
          modifiedPseudoinverseParameters
        ],
        {modifiedPseudoinverseParameter}
      ]
    ],
    modifiedPseudoinverseParameters
  ]
];

checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping2[mapping_] := Module[
  {
    d,
    logPrimeMatrix,
    summationMap,
    justMap,
    inverseLogPrimeMatrix,
    parameterizedPseudoinverseDualPreTransformer,
    tuningMap,
    retuningMap,
    minimaxDamage,
    tuningMapForMinMinimaxDamage,
    minMinimaxDamage,
    pseudoinverseParametersForMinMinimaxDamage,
    maxMinimaxDamage,
    pseudoinverseParametersForMaxMinimaxDamage
  },
  
  d = getDPrivate[parseTemperamentData[mapping]];
  logPrimeMatrix = getLogPrimeMatrix[d];
  summationMap = getSummationMap[d];
  justMap = 1200 * summationMap.logPrimeMatrix;
  inverseLogPrimeMatrix = Inverse[logPrimeMatrix];
  
  minMinimaxDamage = Infinity;
  pseudoinverseParametersForMinMinimaxDamage = {};
  maxMinimaxDamage = 0;
  pseudoinverseParametersForMaxMinimaxDamage = {};
  
  pseudoinverseParameterSets = Tuples[Range[2.639 - 0.003, 2.639 + 0.003, 0.001], d];
  Print["length of pseudoinverseParameterSets: ", Length[pseudoinverseParameterSets]];
  
  
  Do[
    parameterizedPseudoinverseDualPreTransformer = getParameterizedPseudoinverseDualPreTransformer2[pseudoinverseParameters];
    
    (* override with this particular parameterized pseudoinverse *)
    augmentedEitherSideMultiplierPartArg[na_] := {inverseLogPrimeMatrix.parameterizedPseudoinverseDualPreTransformer, "row"};
    
    tuningMap = optimizeTuningMap[mapping, "minimax-lil-S"];
    retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
    minimaxDamage = Max[Abs[retuningMap.inverseLogPrimeMatrix.parameterizedPseudoinverseDualPreTransformer]];
    
    If[
      minimaxDamage > maxMinimaxDamage,
      maxMinimaxDamage = minimaxDamage;
      pseudoinverseParametersForMaxMinimaxDamage = {pseudoinverseParameters},
      If[
        minimaxDamage == maxMinimaxDamage,
        pseudoinverseParametersForMaxMinimaxDamage = Join[pseudoinverseParametersForMaxMinimaxDamage, {pseudoinverseParameters}];
      ]
    ];
    
    If[
      minimaxDamage < minMinimaxDamage,
      minMinimaxDamage = minimaxDamage;
      tuningMapForMinMinimaxDamage = tuningMap;
      pseudoinverseParametersForMinMinimaxDamage = {pseudoinverseParameters},
      If[
        minimaxDamage == minMinimaxDamage,
        pseudoinverseParametersForMinMinimaxDamage = Join[pseudoinverseParametersForMinMinimaxDamage, {pseudoinverseParameters}];
      ]
    ];
    
    (* report *)
    (*Print["pseudoinverseParameter: ", pseudoinverseParameter]; *)
    (* Print["tuningMap: ", tuningMap]; *)
    (* Print["retuningMap: ", retuningMap]; *)
    (* Print["minimaxDamage: ", minimaxDamage]; *)
    ,
    
    {pseudoinverseParameters, pseudoinverseParameterSets}
  ];
  
  {
    mapping,
    NumberForm[tuningMapForMinMinimaxDamage, {5, 3}],
    minMinimaxDamage,
    pseudoinverseParametersForMinMinimaxDamage,
    maxMinimaxDamage,
    pseudoinverseParametersForMaxMinimaxDamage
  }
];

checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping2["[⟨1 0 -4 -13] ⟨0 1 4 10]}"]

checkMinimaxDamagesAcrossPseudoinverseParameterRangeForMapping2["[⟨12 19 28]}"]

(* yes, this actually has nothing to do with the mapping at all, only the final tuning map *)
getPseudoinverseParameterForKnownMinMinimaxDamageTuningFromAugmentedMethod[unparsedTuningMap_] := Module[
  {tuningMap, d, logPrimeMatrix, partiallyPreTransformedRetuningMap},
  
  tuningMap = getL[parseTemperamentData[unparsedTuningMap]];
  d = Length[tuningMap];
  logPrimeMatrix = getLogPrimeMatrix[d];
  partiallyPreTransformedRetuningMap = (tuningMap - 1200 * getSummationMap[d].logPrimeMatrix).Inverse[logPrimeMatrix];
  
  NMinimize[
    Max[Abs[partiallyPreTransformedRetuningMap.getParameterizedPseudoinverseDualPreTransformer[pseudoinverseParameter, d]]],
    pseudoinverseParameter,
    WorkingPrecision -> 8
  ]
];

getPseudoinverseParameterForKnownMinMinimaxDamageTuningFromAugmentedMethod["⟨1200.000 1901.887 2786.214 3368.705]"]

FindFormula[{{2.593, 2.187}, {3.166, 3.332}, {3.197, 3.393}, {3.199, 3.398}, {3.327, 3.654}, {3.047, 3.095}}]
    
    - 3 + 2 * 3.327

FindFormula[{{0.093, 2.187}, {0.666, 3.332}, {0.697, 3.393}, {0.699, 3.398}, {0.827, 3.654}, {0.547, 3.095}}]