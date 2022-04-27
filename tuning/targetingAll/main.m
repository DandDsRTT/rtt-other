(* TARGETING-ALL *)

optimizeGeneratorsTuningMapTargetingAll[{
  t_,
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_, (* trait 4d *)
  pureOctaveStretch_
}] := If[
  damageWeightingSlope != "simplicityWeighted",
  Throw["It is not possible to minimize damage over all intervals if it is not simplicity-weighted."],
  If[
    optimizationPower != \[Infinity],
    Throw["It is not possible to optimize for minisum or minisos over all intervals, only minimax."],
    If[
      complexityNormPower == 2 && Length[unchangedIntervals] == 0 && complexityMakeOdd == False,
      
      (* covers TE, Frobenius, WE, BE *)
      optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
        t,
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ],
      
      (* covers TOP, L1 version of Frobenius, BOP, Weil, Kees, KE, CTE *)
      optimizeGeneratorsTuningMapTargetingAllNumerical[
        t,
        unchangedIntervals, (* trait -1 *)
        complexityNormPower, (* trait 3 *)
        complexityNegateLogPrimeCoordination, (* trait 4a *)
        complexityPrimePower, (* trait 4b *)
        complexitySizeFactor, (* trait 4c *)
        complexityMakeOdd (* trait 4d *)
      ]
    ]
  ]
];

optimizeGeneratorsTuningMapTargetingAllPseudoInverseAnalytical[
  t_,
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[{dualMultiplier, targetedIntervalsAsPrimesIdentityA},
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  
  optimizeGeneratorsTuningMapWithPseudoInverse[
    t,
    targetedIntervalsAsPrimesIdentityA,
    dualMultiplier
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumerical[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := If[
  complexitySizeFactor != 0 && complexityNormPower == 1,
  
  (* covers Weil and Kees *)
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ],
  
  (* covers TOP, BOP, and L1-version of Frobenius, and KE and CTE even though they have a complexityNormPower of 2 *)
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNorm[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := If[
  complexityNormPower != 2 && hasNonUniqueTuning[getM[t]],
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormWithoutUniqueResult[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ],
  optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
    t,
    unchangedIntervals, (* trait -1 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormAndHasUniqueResult[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
    
    dualMultiplier,
    targetedIntervalsAsPrimesIdentityA,
    
    normPower,
    errorsMap,
    periodsPerOctave,
    minimizedNorm,
    solution
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  errorsMap = Abs[(tuningMap - primesTuningMap).targetedIntervalsAsPrimesIdentityA.dualMultiplier];
  normPower = dualPower[complexityNormPower];
  
  periodsPerOctave = getPeriodsPerOctave[t];
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {Norm[errorsMap, normPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    Norm[errorsMap, normPower]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  generatorsTuningMap /. Last[solution]
];

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsPowerNormWithoutUniqueResult[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
    
    dualMultiplier,
    targetedIntervalsAsPrimesIdentityA,
    
    errorMagnitude,
    previousErrorMagnitude,
    previousSolution,
    normPower,
    normPowerPower,
    
    normPowerLimit,
    errorsMap,
    periodsPerOctave,
    minimizedNorm,
    solution
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  targetedIntervalsAsPrimesIdentityA = getPrimesIdentityA[t];
  dualMultiplier = getDualMultiplier[
    t,
    targetedIntervalsAsPrimesIdentityA, (* trait 0 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  errorsMap = Abs[(tuningMap - primesTuningMap).targetedIntervalsAsPrimesIdentityA.dualMultiplier];
  normPowerLimit = dualPower[complexityNormPower];
  
  errorMagnitude = 1000000;
  previousErrorMagnitude = \[Infinity];
  normPower = 2;
  normPowerPower = 1;
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  While[
    (* the != bit, while seemingly unnecessary, prevented a certain type of crash *)
    normPowerPower <= 6 && previousErrorMagnitude != errorMagnitude && previousErrorMagnitude - errorMagnitude > 0,
    
    previousErrorMagnitude = errorMagnitude;
    previousSolution = solution;
    minimizedNorm = If[
      Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
      {Norm[errorsMap, normPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
      Norm[errorsMap, normPower]
    ];
    solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
    errorMagnitude = First[solution];
    normPowerPower = normPowerPower += 1;
    normPower = If[normPowerLimit == 1, Power[2, 1 / normPowerPower], Power[2, normPowerPower]];
  ];
  
  generatorsTuningMap /. Last[previousSolution]
];
(* TODO: if you look at this diff, I don't think I was actually finding any TIPTOP tunings! 
because I was always just plugging the optimizationPower in, 
not the actual normPower that I was supposed to be iterating...
whoops!
I also think the normpowerpower max of 10 was too high... constantly veering off into too much error introduced by huge powers computation
note how it's related to the workingprecision
it never goes above working precision
I should realize that
like actually while condition on the normpower and make it < 128 if need be
and also I think ... wow, yeah, there was at least one of these where I wasn't actually taking the Abs of the error!
that seems to be both the targeting-all ones
so that certainly would have affected anything where the norm power wasn't even...
*)

optimizeGeneratorsTuningMapTargetingAllNumericalDualNormIsNotPowerNorm[
  t_,
  unchangedIntervals_, (* trait -1 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
    periodsPerOctave,
    adjustedPrimesErrorMap,
    minimizedNorm,
    solution
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  adjustedPrimesErrorMap = tuningMap / primesTuningMap - Table[1, getD[t]];
  adjustedPrimesErrorMap = AppendTo[adjustedPrimesErrorMap, 0];
  
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {Max[adjustedPrimesErrorMap] - Min[adjustedPrimesErrorMap], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    Max[adjustedPrimesErrorMap] - Min[adjustedPrimesErrorMap]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  
  generatorsTuningMap /. Last[solution]
];

dualPower[power_] := If[power == 1, Infinity, 1 / (1 - 1 / power)];

(* getDamageWeights may be simplicity-weighted or complexity-weighted, 
but this is always essentially "simplicity-weighted" *)
getDualMultiplier[
  t_,
  targetedIntervalsAsPrimesIdentityA_, (* trait 0 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := PseudoInverse[getComplexityMultiplier[
  t,
  complexityNegateLogPrimeCoordination, (* trait 4a *)
  complexityPrimePower, (* trait 4b *)
  complexitySizeFactor, (* trait 4c *)
  complexityMakeOdd (* trait 4d *)
]];
