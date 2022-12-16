(* GRAPHING *)

powerMean[l_, power_] := If[
  power == \[Infinity],
  Max[l],
  Power[Mean[Power[l, power]], 1 / power]
];

nestedGraphTuningDamage[unparsedT_, tuningSchemeSpec_, whichOne_] := Module[
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
    nonprimeBasisApproach,
    
    tWithPossiblyChangedDomainBasis,
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
  
  tWithPossiblyChangedDomainBasis = tuningSchemeProperty[tuningSchemeProperties, "t"];
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"]; (* trait 1 *)
  optimizationPower = tuningSchemeProperty[tuningSchemeProperties, "optimizationPower"]; (* trait 2 *)
  damageWeightSlope = tuningSchemeProperty[tuningSchemeProperties, "damageWeightSlope"]; (* trait 3 *)
  intervalComplexityNormPower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPower"]; (* trait 4 *)
  intervalComplexityNormPreTransformerLogPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerLogPrimePower"]; (* trait 5a *)
  intervalComplexityNormPreTransformerPrimePower = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerPrimePower"]; (* trait 5b *)
  intervalComplexityNormPreTransformerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPreTransformerSizeFactor"]; (* trait 5c *)
  nonprimeBasisApproach = tuningSchemeProperty[tuningSchemeProperties, "nonprimeBasisApproach"]; (* trait 7 *)
  
  {generatorTuningMap, m, centsConversionAndSummationMapAndLogPrimeA} = getTuningSchemeMappings[t];
  
  plotArgs = {};
  
  opacities = Table[0.5, 8](*{0,0,0,0,0,0,0, 0.5}*);
  colors = {"#FFDF80", "#B8C8DF", "#CCDCA2", "#F7BDAB", "#C9C2DD", "#E8C6A5", "#BCD7E8", "#F4DAAD"};
  
  (* data *)
  targetIntervalGraphs = MapIndexed[
    
    Function[
      {targetIntervalPcv, index},
      (* Print["opacities[[index]]: ", opacities[[index]]];*)
      (*      If[
            
            First[opacities[[index]]] ==0,
            {Infinity},*)
      complexity = getComplexity[
        targetIntervalPcv,
        tWithPossiblyChangedDomainBasis,
        intervalComplexityNormPower, (* trait 4 *)
        intervalComplexityNormPreTransformerLogPrimePower, (* trait 5a *)
        intervalComplexityNormPreTransformerPrimePower, (* trait 5b *)
        intervalComplexityNormPreTransformerSizeFactor, (* trait 5c *)
        nonprimeBasisApproach (* trait 7 *)
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
      (*    ]*)
    ],
    breakByRowsOrCols[targetIntervals]
  ];
  
  (*  meanPower = If[
      optimizationPower == \[Infinity] && damageWeightSlope == "simplicityWeight" && ToString[targetIntervals] == "Null",
      getDualPower[intervalComplexityNormPower],
      optimizationPower
    ];*)
  (*  meanGraph =(*{Infinity};*) powerMean[targetIntervalGraphs, 1] + 0.0001;*)
  (*meanGraph2 = (*{Infinity};*)powerMean[targetIntervalGraphs, 2] + 0.0001;*)
  (*  meanGraph3 =(* {Infinity};*)powerMean[targetIntervalGraphs, \[Infinity]] + 0.0001;*)
  
  targetIntervalGraphs = MapIndexed[
    Function[
      {targetIntervalGraph, index},
      If[
        First[opacities[[index]]] == 0,
        {Infinity},
        targetIntervalGraph
      ]
    ],
    targetIntervalGraphs
  ];
  
  (*AppendTo[plotArgs, {targetIntervalGraphs(*, meanGraph,meanGraph2,meanGraph3*)}];*)
  AppendTo[
    plotArgs,
    If[
      ToString[whichOne] == "mean",
      powerMean[targetIntervalGraphs, 1] + 0.0001,
      If[
        ToString[whichOne] == "RMS",
        powerMean[targetIntervalGraphs, 2] + 0.0001,
        If[
          ToString[whichOne] == "max",
          powerMean[targetIntervalGraphs, \[Infinity]] + 0.0001,
          Part[targetIntervalGraphs, whichOne]
        ]
      ]
    ]
  ];
  
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
  
  plotStyle = (*Join[*)Table[{Auto, Opacity[1]}, Length[targetIntervalGraphs]](*, {If[r == 1, {Black, Dashed}, {(*Opacity[0]*)Texture[image]}],If[r == 1, {Black, Dashed}, {(*Opacity[0]*)Texture[image]}],If[r == 1, {Black, Dashed}, {(*Opacity[0]*)Texture[image]}]}]*);
  
  If[debug == True, printWrapper[plotStyle]];
  
  (* range *)
  MapIndexed[
    Function[
      {optimumGeneratorTuningMapEntry, index},
      
      AppendTo[
        plotArgs,
        (* this is where we give it \[PlusMinus]2 ¢ around the exact tuning map *)
        {Part[getL[generatorTuningMap], First[index]], optimumGeneratorTuningMapEntry - 2, optimumGeneratorTuningMapEntry + 2}
      ]
    ],
    
    getL[ optimumGeneratorTuningMap]
  ];
  
  (* settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  (*  AppendTo[plotArgs, PlotStyle -> plotStyle];
    AppendTo[plotArgs, PlotRange -> {0,15}];*)
  AppendTo[plotArgs, ContourShading -> None];
  AppendTo[plotArgs, Contours -> Range[0, 15, 1]];
  AppendTo[
    plotArgs,
    ContourStyle -> (* {Dashed}*)If[
      ToString[whichOne] == "mean",
      {Dotted},
      If[
        ToString[whichOne] == "RMS",
        {Dashed},
        If[
          ToString[whichOne] == "mean",
          Black,
          RGBColor[Part[colors, whichOne]]
        ]
      ]
    ]
  ];
  AppendTo[plotArgs, MaxRecursion -> 6];
  
  (* plot type *)
  r = getRPrivate[tWithPossiblyChangedDomainBasis];
  
  Print["plotArgs: ", plotArgs];
  If[
    r == 1,
    Apply[Plot, plotArgs],
    If[
      r == 2,
      Apply[ContourPlot, plotArgs],
      Throw["4D and higher visualizations not supported"]
    ]
  ]
];


graphTuningDamage[unparsedT_, tuningSchemeSpec_] := Show[
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 1],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 2],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 3],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 4],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 5],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 6],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 7],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, 8],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, "mean"],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, "RMS"],
  nestedGraphTuningDamage[unparsedT, tuningSchemeSpec, "max"]
];
