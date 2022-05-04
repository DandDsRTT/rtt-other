optimizeGeneratorsTuningMapPrimesPowerNormDualOfIntegerLimit[
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
    tuningMap,
    
    damagesL,
    normFn,
    periodsPerOctave,
    minimizedNorm,
    solution
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  tuningMap = Part[tuningMappings, 3];
  
  damagesL = getAugmentedDualMultipliedPrimesErrorL[tuningMap, t];
  normFn = dualNormOfIntegerLimit;
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {normFn[damagesL], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    normFn[damagesL]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  generatorsTuningMap /. Last[solution]
];

getAugmentedDualMultipliedPrimesErrorL[tuningMap_, t_] := Module[
  {primesTuningMap, damagesL},
  
  (* TODO: for simplicity, keep it this way for now, 
  but I don't like the inconsistency in this dividing tuning map by primes tuning map and subtracting 1's...
  what's the difference between that and, well...
  confirm whether multiplying by the appropriate dual multiplier would work out the same *)
  primesTuningMap = getPrimesTuningMap[t];
  
  damagesL = tuningMap / primesTuningMap - Table[1, getD[t]];
  
  (* TODO: see if you can reconsolidate this now, that is no formerly-known-as "middleMan" shenanigans necessary
  but also you should extract following the pattern of the other two *)
  AppendTo[damagesL, 0]
];

(* as described here: https://en.xen.wiki/w/Weil_Norms,_Tenney-Weil_Norms,_and_TWp_Interval_and_Tuning_Space#Dual_Norms *)
dualNormOfIntegerLimit[vector_] := Max[vector] - Min[vector];
