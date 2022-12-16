(* GRAPHING *)

powerMean[l_, power_] := If[
  power == \[Infinity],
  Max[l],
  Power[Mean[Power[l, power]], 1 / power]
];

graphTuningDamage[unparsedT_, tuningSchemeSpec_] := Module[
  {
    t,
    
    forDamage,
    
    tuningSchemeOptions,
    optimumGeneratorTuningMap,
    
    tuningSchemeProperties,
    
    optimizationPower,
    damageWeightSlope,
    intervalComplexityNormPower,
    intervalComplexityNormPreTransformerLogPrimePower,
    intervalComplexityNormPreTransformerPrimePower,
    intervalComplexityNormPreTransformerSizeFactor,
    
    tWithPossiblyChangedIntervalBasis,
    targetIntervals,
    
    generatorTuningMap,
    m,
    centsConversionAndSummationMapAndLogPrimeA,
    
    meanPower,
    meanGraph,
    
    plotArgs,
    targetIntervalGraphs,
    r,
    plotStyle,
    image
  },
  
  t = parseTemperamentData[unparsedT];
  
  forDamage = True;
  
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  optimumGeneratorTuningMap = optimizeGeneratorTuningMapPrivate[t, tuningSchemeOptions];
  
  tuningSchemeProperties = processTuningSchemeOptions[t, forDamage, tuningSchemeOptions];
  
  tWithPossiblyChangedIntervalBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  damageWeightSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightSlope"]; (* trait 3 *)
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA} = getTuningSchemeMappings[t];
  
  plotArgs = {};
  
  (* data *)
  targetIntervalGraphs = Map[
    Function[
      {targetIntervalPcv},
      
      complexity = getComplexity[
        targetIntervalPcv,
        tWithPossiblyChangedIntervalBasis,
        intervalComplexityNormPower, (* trait 4 *)
        intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
        intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
        intervalComplexityNormPreTransformerSizeFactor (* trait 5c *)
      ];
      weighting = If[
        damageWeightSlope == "unityWeight",
        1,
        If[
          damageWeightSlope == "complexityWeight",
          complexity,
          1 / complexity
        ]
      ];
      error = getL[subtractT[
        multiplyToRows[generatorTuningMap, m, targetIntervalPcv],
        multiplyToRows[centsConversionAndSummationMapAndLogPrimeA, targetIntervalPcv]
      ]];
      damage = Abs[error] * weighting;
      
      damage
    ],
    breakByRowsOrCols[targetIntervals]
  ];
  
  meanPower = If[
    optimizationPower == \[Infinity] && damageWeightSlope == "simplicityWeight" && ToString[targetIntervals] == "Null",
    getDualPower[intervalComplexityNormPower],
    optimizationPower
  ];
  Print["mean power: ", meanPower];
  meanGraph = powerMean[targetIntervalGraphs, 1] + 0.01;
  meanGraph2 = powerMean[targetIntervalGraphs, 1.25] + 0.01;
  meanGraph3 = powerMean[targetIntervalGraphs, 1.5] + 0.01;
  meanGraph4 = powerMean[targetIntervalGraphs, 2] + 0.01;
  meanGraph5 = powerMean[targetIntervalGraphs, 4] + 0.01;
  meanGraph6 = powerMean[targetIntervalGraphs, 8] + 0.01;
  meanGraph7 = powerMean[targetIntervalGraphs, \[Infinity]] + 0.01;
  
  AppendTo[plotArgs, {targetIntervalGraphs, meanGraph, meanGraph2, meanGraph3, meanGraph4, meanGraph5, meanGraph6, meanGraph7 }];
  
  image = Image[
    Map[
      Map[
        If[
          # == 1,
          {0, 0, 0, 1},
          {0, 0, 0, 0}
        ]&,
        #
      ]&,
      Array[(-1)^+ ## &, {32, 32}]
    ],
    ColorSpace -> "RGB"
  ];
  image = ImageResize[image, 256, Resampling -> "Constant"];
  plotStyle = Join[
    Table[{Black, Opacity[0.666]}, Length[targetIntervalGraphs]],
    {
      If[r == 1, {Blend[{Red, Red, Gray}]}, {Texture[image]}],
      If[r == 1, {Blend[{Orange, Orange, Gray}]}, {Texture[image]}],
      If[r == 1, {Blend[{Yellow, Yellow, Gray}]}, {Texture[image]}],
      If[r == 1, {Blend[{Green, Green, Gray}]}, {Texture[image]}],
      If[r == 1, {Blend[{Cyan, Cyan, Gray}]}, {Texture[image]}],
      If[r == 1, {Blend[{Blue, Blue, Gray}]}, {Texture[image]}],
      If[r == 1, {Blend[{Purple, Purple, Gray}]}, {Texture[image]}],
    }
  ];
  
  If[debug == True, printWrapper[plotStyle]];
  
  (* range *)
  MapIndexed[
    Function[
      {optimumGeneratorTuningMapEntry, index},
      
      AppendTo[
        plotArgs,
        (* this is where we give it \[PlusMinus]2 ¢ around the exact tuning map *)
        {Part[getL[generatorTuningMap], First[index]], optimumGeneratorTuningMapEntry - 0.15, optimumGeneratorTuningMapEntry + 0.05}
      ]
    ],
    
    getL[ optimumGeneratorTuningMap]
  ];
  
  (* settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  AppendTo[plotArgs, PlotStyle -> plotStyle];
  AppendTo[plotArgs, MaxRecursion -> 6];
  
  (* plot type *)
  r = getRPrivate[tWithPossiblyChangedIntervalBasis];
  If[
    r == 1,
    Apply[Plot, plotArgs],
    If[
      r == 2,
      Apply[Plot3D, plotArgs],
      Throw["4D and higher visualizations not supported"]
    ]
  ]
];


graphTuningDamage["[⟨12 19]}", "primes minimean-U"]
