
getComplexityLimit[maxComplexity_, tuningSchemeSpec_] := Module[
  {
    tuningSchemeOptions,
    tuningSchemeProperties,
    
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd, (* trait 5d *)
    
    numerator,
    interval,
    complexityLimit,
    intervalsForThisNumerator,
    index,
    intervalComplexity
  },
  
  (* lots of dummy stuff used in this block to let us leverage the processing stuff normally used for entire optimizations of temperaments, just to process the complexity; although actually I think you ought to do something completely different because this doens't even support lain old product ocplexity as far as I can tell... *)
  tuningSchemeOptions = processTuningSchemeSpec[Join[tuningSchemeSpec, {"targetedIntervals" -> "{1}", "damageWeightingSlope" -> "unweighted"}]];
  tuningSchemeProperties = processTuningSchemeOptions[{{{1}}, "map"}, False, tuningSchemeOptions];
  
  complexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "complexityNormPower"]; (* trait 4 *)
  complexityNegateLogPrimeCoordinator = tuningSchemeProperty[tuningSchemeProperties, "complexityNegateLogPrimeCoordinator"]; (* trait 5a *)
  complexityPrimePower = tuningSchemeProperty[tuningSchemeProperties, "complexityPrimePower"]; (* trait 5b *)
  complexitySizeFactor = tuningSchemeProperty[tuningSchemeProperties, "complexitySizeFactor"]; (* trait 5c *)
  complexityMakeOdd = tuningSchemeProperty[tuningSchemeProperties, "complexityMakeOdd"]; (* trait 5d *)
  
  numerator = 2;
  complexityLimit = {};
  
  While[
    intervalsForThisNumerator = getIntervalsForThisNumerator[numerator];
    intervalComplexity = getIntervalComplexity[
      First[intervalsForThisNumerator],
      complexityNormPower,
      complexityNegateLogPrimeCoordinator,
      complexityPrimePower,
      complexitySizeFactor,
      complexityMakeOdd
    ];
    intervalComplexity <= maxComplexity,
    
    index = 1;
    While[
      interval = If[index > Length[intervalsForThisNumerator], Null, Part[intervalsForThisNumerator, index]];
      ToString[interval] != "Null" && getIntervalComplexity[
        interval,
        complexityNormPower,
        complexityNegateLogPrimeCoordinator,
        complexityPrimePower,
        complexitySizeFactor,
        complexityMakeOdd
      ] <= maxComplexity,
      
      AppendTo[complexityLimit, interval];
      index++;
    ];
    numerator++;
  ];
  
  DeleteDuplicates[complexityLimit]
];
getIntervalsForThisNumerator[numerator_] := Map[numerator / #&, Range[1, numerator - 1]];
getIntervalComplexity[
  interval_,
  complexityNormPower_,
  complexityNegateLogPrimeCoordinator_,
  complexityPrimePower_,
  complexitySizeFactor_,
  complexityMakeOdd_
] := Module[
  {pcv},
  
  pcv = quotientToPcv[interval];
  
  getComplexity[
    colify[pcv],
    rowify[pcv], (* this is "t"; it just has to have the same dimensionality as the pcv so that the complexity multiplier gets done correctly *)
    complexityNormPower, (* trait 4 *)
    complexityNegateLogPrimeCoordinator, (* trait 5a *)
    complexityPrimePower, (* trait 5b *)
    complexitySizeFactor, (* trait 5c *)
    complexityMakeOdd (* trait 5d *)
  ]
];
