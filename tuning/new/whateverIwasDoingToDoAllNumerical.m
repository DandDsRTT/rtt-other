In[1308]:= (*
  
  GENERATORS PREIMAGE TRANSVERSAL
  
  
  getGeneratorsPreimageTransversal[t]
  
  Given a representation of a temperament as a mapping or comma basis,
  returns a generators preimage transversal 
  (for each generator, one JI interval that maps to it).
  
  Examples:
  
  In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
        getGeneratorsPreimageTransversal[meantoneM]
    
  Out   {{{1, 0, 0}, {-1, 1, 0}}, "contra"}
  
*)
    getGeneratorsPreimageTransversal[t_] := Module[{ma, decomp, left, snf, right, generatorsPreimageTransversal},
      ma = getA[getM[t]];
      decomp = SmithDecomposition[ma];
      left = Part[decomp, 1];
      snf = Part[decomp, 2];
      right = Part[decomp, 3];
      
      generatorsPreimageTransversal = right.Transpose[snf].left;
      
      {Transpose[generatorsPreimageTransversal], "contra"}
    ];

In[1309]:=
    (*
      
      TUNING
      
      
      optimizeGeneratorsTuningMap[t]
      
      Given a representation of a temperament as a mapping or comma basis,
      returns the optimal generator tuning map.
      
      The tuning may be specified by name, or by individual parameters.
      
      Examples:
      
      In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
            optimizeGeneratorsTuningMap[meantoneM]
        
      Out   {1200., 696.578}
      
      In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
            optimizeGeneratorsTuningMap[meantoneM, "originalTuningName" -> "TOP"]
        
      Out   {1201.7, 697.564}
      
      In    meantoneM = {{{1, 1, 0}, {0, 1, 4}}, "co"};
            optimizeGeneratorsTuningMap[meantoneM, "optimization" -> "minisos", "damage" -> "MEC"]
        
      Out   {1198.24, 695.294}
    *)
    Options[optimizeGeneratorsTuningMap] = tuningOptions;
optimizeGeneratorsTuningMap[t_, OptionsPattern[]] := Module[
  {
    meanPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean,
    tuningOptions
  },
  
  meanPower = OptionValue["optimizationPower"];
  weighted = OptionValue["weighted"];
  weightingDirection = OptionValue["damageWeightingSlope"];
  complexityWeighting = OptionValue["complexityUnitsMultiplier"];
  complexityPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  damage = OptionValue["damage"];
  tuning = OptionValue["originalTuningName"];
  mean = OptionValue["optimization"];
  
  tuningOptions = processTuningOptions[t, meanPower, weighted, weightingDirection, complexityWeighting, complexityPower, tim, damage, tuning, mean];
  meanPower = First[tuningOptions];
  
  1200 * If[
    meanPower == \[Infinity],
    optimizeGtmMinimax[tuningOptions],
    If[
      meanPower == 2,
      optimizeGtmLeastSquares[tuningOptions],
      optimizeGtmLeastAbsolutes[tuningOptions]
    ]
  ]
];

In[1311]:=
    
    In[1312]:=
        (* ___ PRIVATE ___ *)
        
        In[1313]:=
            (* MINIMAX *)
            
            In[1314]:= optimizeGtmMinimax[{meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] := If[
              weighted == True && weightingDirection == "simplicityWeighted" && Length[tima] == 0,
              optimizeGtmMinimaxPLimit[d, t, ptm, complexityWeighting, complexityPower],
              (*If[
                weighted == False,
                optimizeGtmMinimaxConsonanceSetAnalytical[meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower],*)
              optimizeGtmMinimaxConsonanceSetNumerical[tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]
              (*  ]*)
            ];

In[1315]:=
    (* TARGETING-ALL MINIMAX *)
    
    In[1608]:= optimizeGtmMinimaxPLimit[d_, t_, ptm_, complexityWeighting_, complexityPower_] :=(* If[
  complexityPower == 2,
  optimizeGtmMinimaxPLimitPseudoInverseAnalytical[d, t, ptm, complexityWeighting],*)
        optimizeGtmMinimaxPLimitLinearProgrammingNumerical[d, t, ptm, complexityWeighting, complexityPower];
(*]*)(*;*)

In[1317]:= (*optimizeGtmMinimaxPLimitPseudoInverseAnalytical[d_, t_, ptm_, complexityWeighting_] := Module[{w, tima, weightedTima, unchangedIntervals, g, gtm},
  w = If[complexityWeighting == "logProduct", 1 / ptm, Table[1, d]];
  tima = IdentityMatrix[d];
  
  optimizeGtmWithPseudoInverse[tima, w, t, ptm]
];*)
    
    In[1318]:= optimizeGtmMinimaxPLimitLinearProgrammingNumerical[d_, t_, ptm_, complexityWeighting_, complexityPower_] := Module[{gtm, ma, tm, e, solution},
      gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, getR[t]}];
      ma = getA[getM[t]];
      tm = gtm.ma;
      e = If[complexityWeighting == "logProduct", tm / ptm - Table[1, d], tm - ptm];
      
      solution = NMinimize[Norm[e, dualPower[complexityPower]], gtm, Method -> "NelderMead",WorkingPrecision -> 15];
      gtm /. Last[solution] // N
    ];

In[1319]:= dualPower[power_] := If[power == 1, 128, 1 / (1 - 1 / power)];

In[1320]:=
    (* TARGETING-LIST MINIMAX *)
    
    In[1321]:= (*optimizeGtmMinimaxConsonanceSetAnalytical[meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] :=
    optimizeGtmSimplex[meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower, getMaxDamage];*)
        
        In[1322]:= optimizeGtmMinimaxConsonanceSetNumerical[tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[
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
          w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
          
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

In[1323]:=
    (* MINISOS *)
    
    In[1324]:= optimizeGtmLeastSquares[{meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] := Module[{w, weightedTima, unchangedIntervals, g, gtm},
      w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
      
      optimizeGtmWithPseudoInverse[tima, w, t, ptm]
    ];

In[1325]:= optimizeGtmWithPseudoInverse[tima_, w_, t_, ptm_] := Module[{ma, weightedTima, unchangedIntervals, g, gtm},
  ma = getA[getM[t]];
  weightedTima = tima * w;
  unchangedIntervals = ma.Transpose[weightedTima].weightedTima;
  g = Transpose[unchangedIntervals].Inverse[unchangedIntervals.Transpose[ma]];
  gtm = ptm.g;
  gtm // N
  (*thing = ma.weightedTima;
  ptm.weightedTima.PseudoInverse[thing] // N*)
];

In[1326]:=
    (* MINISUM *)
    
    In[1327]:= optimizeGtmLeastAbsolutes[{meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
        optimizeGtmSimplex[meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower, getSumOfAbsolutesDamage];

In[1328]:=
    (* SIMPLEX (USED BY ANALYTICAL MINIMAX AND MINISUM) *)
    
    In[1329]:= optimizeGtmSimplex[meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_, damageMean_] := Module[
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
        generatorsPreimageTransversal,
        projectedGenerators
      },
      
      r = getR[t];
      unchangedIntervalSetIndices = Subsets[Range[Length[tima]], {r}];
      potentialUnchangedIntervalSets = Map[Map[tima[[#]]&, #]&, unchangedIntervalSetIndices];
      normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
      filteredNormalizedPotentialUnchangedIntervalSets = Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&];
      potentialPs = Select[Map[getPFromUnchangedIntervals[t, #]&, filteredNormalizedPotentialUnchangedIntervalSets], Not[# === Null]&];
      potentialTms = Map[ptm.#&, potentialPs];
      meanOfDamages = Map[damageMean[#, {meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}]&, potentialTms];
      
      minMeanIndices = Position[meanOfDamages, Min[meanOfDamages]];
      If[
        Length[minMeanIndices] == 1,
        
        minMeanIndex = First[First[Position[meanOfDamages, Min[meanOfDamages]]]];
        minMeanP = potentialPs[[minMeanIndex]],
        
        tiedTms = Part[potentialTms, Flatten[minMeanIndices]];
        tiedPs = Part[potentialPs, Flatten[minMeanIndices]];
        minMeanIndex = tieBreak[tiedTms, meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower];
        minMeanP = tiedPs[[minMeanIndex]]
      ];
      
      generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[t]]];
      projectedGenerators = minMeanP.generatorsPreimageTransversal;
      ptm.projectedGenerators // N
    ];

In[1330]:= getPFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[{commaEigenvectors, eigenvectors, diagonalEigenvalueMatrix},
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueMatrix = getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueMatrix.Inverse[eigenvectors]]
];

In[1331]:= getDiagonalEigenvalueMatrix[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];

In[1332]:=
    (* TARGET INTERVAL SETS *)
    
    In[1333]:= getDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
      oddLimit = oddLimitFromD[d];
      oddsWithinLimit = Range[1, oddLimit, 2];
      rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
      
      padVectorsWithZerosUpToD[Map[quotientToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
    ];

In[1334]:= octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

In[1335]:= oddLimitFromD[d_] := Prime[d + 1] - 2;

In[1336]:=
    (* DAMAGE *)
    
    In[1337]:= Options[getDamage] = tuningOptions;
getDamage[t_, gtm_, OptionsPattern[]] := Module[
  {
    meanPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean,
    ma,
    tm,
    tuningOptions
  },
  
  meanPower = OptionValue["optimizationPower"];
  weighted = OptionValue["weighted"];
  weightingDirection = OptionValue["damageWeightingSlope"];
  complexityWeighting = OptionValue["complexityUnitsMultiplier"];
  complexityPower = OptionValue["complexityNormPower"];
  tim = OptionValue["tim"];
  damage = OptionValue["damage"];
  tuning = OptionValue["originalTuningName"];
  mean = OptionValue["optimization"];
  
  tuningOptions = processTuningOptions[t, meanPower, weighted, weightingDirection, complexityWeighting, complexityPower, tim, damage, tuning, mean, True];
  meanPower = First[tuningOptions];
  ma = getA[getM[t]];
  
  tm = (gtm / 1200).ma;
  
  If[
    meanPower == \[Infinity],
    getMaxDamage[tm, tuningOptions],
    If[
      meanPower == 2,
      getSumOfSquaresDamage[tm, tuningOptions],
      getSumOfAbsolutesDamage[tm, tuningOptions]
    ]
  ]
];

In[1339]:= getTid[tm_, tima_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{e, w},
  e = N[tm.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = getW[tima, weighted, weightingDirection, complexityWeighting, complexityPower];
  
  e * w
];

In[1340]:= Square[n_] := n^2;

In[1341]:= getSumOfAbsolutesDamage[tm_, {meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Total[Map[Abs, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

In[1342]:= getSumOfSquaresDamage[tm_, {meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Total[Map[Square, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

In[1343]:= getMaxDamage[tm_, {meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] :=
    Max[Map[Abs, getTid[tm, tima, ptm, weighted, weightingDirection, complexityWeighting, complexityPower]]];

In[1344]:= tieBreak[tiedTms_, meanPower_, tima_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{meanOfDamages},
  meanOfDamages = Map[getSumOfSquaresDamage[#, {meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}]&, tiedTms];
  
  First[First[Position[meanOfDamages, Min[meanOfDamages]]]]
];

In[1345]:=
    (* SHARED *)
    
    In[1346]:= tuningOptions = {
      "optimizationPower" -> \[Infinity],
      "weighted" -> False,
      "damageWeightingSlope" -> "simplicityWeighted",
      "complexityUnitsMultiplier" -> "logProduct",
      "complexityNormPower" -> 1,
      "tim" -> Null,
      "damage" -> "",
      "optimization" -> "",
      "originalTuningName" -> ""
    };

In[1347]:= processTuningOptions[t_, inputMeanPower_, inputWeighted_, inputWeightingDirection_, inputComplexityWeighting_, inputComplexityPower_, inputTim_, inputDamage_, inputTuning_, inputMean_, forDamage_ : False] := Module[
  {
    tima,
    damageParts,
    d,
    ptm,
    meanPower,
    weighted,
    weightingDirection,
    complexityWeighting,
    complexityPower,
    tim,
    damage,
    tuning,
    mean
  },
  
  meanPower = inputMeanPower;
  weighted = inputWeighted;
  weightingDirection = inputWeightingDirection;
  complexityWeighting = inputComplexityWeighting;
  complexityPower = inputComplexityPower;
  tim = inputTim;
  damage = inputDamage;
  tuning = inputTuning;
  mean = inputMean;
  
  If[
    tuning === "TOP",
    damage = "S"; tim = {},
    If[
      tuning === "TE",
      damage = "ES"; tim = {},
      If[
        tuning === "Partch",
        damage = "C",
        If[
          tuning === "Frobenius",
          damage = "MES"; tim = {},
          If[
            tuning === "least squares",
            meanPower = 2,
            If[
              tuning === "least absolutes",
              meanPower = 1,
              If[
                tuning === "Tenney least squares",
                meanPower = 2; damage = "S"
              ]
            ]
          ]
        ]
      ]
    ]
  ];
  
  damageParts = StringPartition[damage, 1];
  If[
    Length[damageParts] === 3,
    weighted = True;
    weightingDirection = "complexityWeighted";
    complexityWeighting = damageParts[[2]];
    complexityPower = ToExpression[damageParts[[3]]],
    If[
      Length[damageParts] === 2,
      weighted = True;
      weightingDirection = "simplicityWeighted";
      complexityWeighting = damageParts[[1]];
      complexityPower = ToExpression[damageParts[[2]]];
    ]
  ];
  
  If[
    mean === "minisum",
    meanPower = 1,
    If[
      mean === "minisos",
      meanPower = 2,
      If[
        mean === "minimax",
        meanPower = \[Infinity]
      ]
    ]
  ];
  
  d = getD[t];
  ptm = getPrimesTuningMap[d];
  
  tima = If[tim === Null, getDiamond[d], If[Length[tim] == 0, If[forDamage, getA[getC[t]], {}], getA[tim]]];
  
  {meanPower, tima, d, t, ptm, weighted, weightingDirection, complexityWeighting, complexityPower}
];

In[1348]:= getPrimesTuningMap[d_] := Log[2, getPrimes[d]];

In[1349]:= getW[tima_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_] := Module[{w},
  w = If[
    weighted,
    Map[getComplexity[#, complexityWeighting, complexityPower]&, tima],
    Map[1&, tima]
  ];
  
  If[weightingDirection == "simplicityWeighted", 1 / w, w]
];

In[1350]:= getComplexity[pcv_, complexityWeighting_, complexityPower_] := Module[{weightedPcv},
  weightedPcv = If[complexityWeighting == "logProduct", pcv * getPrimesTuningMap[Length[pcv]], pcv];
  Norm[weightedPcv, complexityPower]
];
