wthThing[tima_, mappedTiIndex_] := Module[{ti, tiAsQuotient, tiAsOctaves},
  ti = First[Part[tima, mappedTiIndex]];
  tiAsQuotient = pcvToQuotient[ti];
  tiAsOctaves = Log[2, pcvToQuotient[ti]]; 
  
  tiAsOctaves
];

minimaxStyle[tima_, mappedTima_, w_, gtm_(*, bestGuess_*)] := NMinimize[
  (*  {*)
  Max[
    MapIndexed[
      Function[
        {mappedTi, mappedTiIndex},
        Abs[
          Total[
            MapThread[
              Function[
                {mappedTiEntry, gtmEntry},
                mappedTiEntry * gtmEntry
              ],
              {mappedTi, gtm}
            ]
          ] - wthThing[tima, mappedTiIndex]
        ] * w[[mappedTiIndex]]
      ],
      mappedTima
    ]
  ](*// N,
  (* would actually generatoe this for ptoentially many more generators *)
  bestGuess[[1]] * 0.99 <= gtm[[1]] <= bestGuess[[1]] * 1.01,
  bestGuess[[2]] * 0.99 <=  gtm[[2]] <= bestGuess[[2]] * 1.01
  }*),
  gtm,
  Method -> "NelderMead",
  WorkingPrecision -> 10
];

leastSquaresStyle[tima_, mappedTima_, w_, gtm_(*, bestGuess_*)] := NMinimize[
  (* {*)
  Total[
    MapIndexed[
      Function[
        {mappedTi, mappedTiIndex},
        Power[
          (
            Total[
              MapThread[
                Function[
                  {mappedTiEntry, gtmEntry},
                  mappedTiEntry * gtmEntry
                ],
                {mappedTi, gtm}
              ]
            ] - wthThing[tima, mappedTiIndex]
          ) * w[[mappedTiIndex]],
          2
        ]
      ],
      mappedTima
    ]
  ](*// N,
   bestGuess[[1]] * 0.99 <= gtm[[1]] <= bestGuess[[1]] * 1.01,
  bestGuess[[2]] * 0.99 <=  gtm[[2]] <= bestGuess[[2]] * 1.01
  }*),
  gtm,
  Method -> "NelderMead",
  WorkingPrecision ->10
];

leastAbsolutesStyle[tima_, mappedTima_, w_, gtm_(*, bestGuess_*)] := NMinimize[
  (*{*)
  Total[
    MapIndexed[
      Function[
        {mappedTi, mappedTiIndex},
        Abs[
          (
            Total[
              MapThread[
                Function[
                  {mappedTiEntry, gtmEntry},
                  mappedTiEntry * gtmEntry
                ],
                {mappedTi, gtm}
              ]
            ] - wthThing[tima, mappedTiIndex]
          ) * w[[mappedTiIndex]](*,
            2*)
        ]
      ],
      mappedTima
    ]
  ] (*// N,
   bestGuess[[1]] * 0.99 <= gtm[[1]] <= bestGuess[[1]] * 1.01,
  bestGuess[[2]] * 0.99 <=  gtm[[2]] <= bestGuess[[2]] * 1.01
  }*),
  gtm,
  Method -> "NelderMead",
  WorkingPrecision -> 10
];


getTuningLinearProgrammingStyle[m_, meanP_, weighting_ : "unweighted", complexityWeighting_ : Null, complexityP_ : Null, tim_ : Null] := Module[
  {
    r,
    d,
    tima,
    ma,
    gtm,
    mappedTima,
    w,
    solution,
    result(*,
    bestGuess*)
  },
  
  r = getR[m];
  d = getD[m];
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  
  mappedTima = Transpose[ ma.Transpose[tima]];
  w = getW[tima, weighting, complexityWeighting, complexityP];
  
  (* bestGuess =optimizeGtm[m, 2] // N;
   Print["bestGeuss", bestGuess];*)
  
  solution = If[
    meanP == \[Infinity],
    minimaxStyle[tima, mappedTima, w, gtm(*, bestGuess*)] ,
    If[
      meanP == 2,
      leastSquaresStyle[tima, mappedTima, w, gtm(*, bestGuess*)],
      leastAbsolutesStyle[tima, mappedTima, w, gtm(*, bestGuess*)]
    ]
  ];
  
  (*Print["minimized damage mean is: "];
  Print[First[solution]];*)
  
  
  result = gtm /. Last[solution] // N;
  
  Print[meanP, weighting, complexityWeighting, complexityP(*, tima*)];
  Print[1200 * First[result]];
  Print[1200 * Last[result]];
  Print[First[solution]];
  
  result
];

m = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)
(*m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)
m = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}; (*miracle *)
m = {{{1, 2, 1,1}, {0, -1, 3, 4}}, "co"}; (*pelogic *)
m = {{{1, 2, 3}, {0, -3, -5}}, "co"}; (*porcupine*)
m = {{{5, 8, 12}, {0, 0, -1}}, "co"}; (* blackwood *)*)

1200 * getTuningLinearProgrammingStyle[m, \[Infinity]]

1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "simplicityWeighted", "noop", 1]
1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "simplicityWeighted", "noop", 2]
1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "simplicityWeighted", "logProduct", 1]
1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "simplicityWeighted", "logProduct", 2]

1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "complexityWeighted", "noop", 1]
1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "complexityWeighted", "noop", 2]
1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "complexityWeighted", "logProduct", 1]
1200 * getTuningLinearProgrammingStyle[m, \[Infinity], "complexityWeighted", "logProduct", 2]


1200 * getTuningLinearProgrammingStyle[m, 2]

1200 * getTuningLinearProgrammingStyle[m, 2, "simplicityWeighted", "noop", 1]
1200 * getTuningLinearProgrammingStyle[m, 2, "simplicityWeighted", "noop", 2]
1200 * getTuningLinearProgrammingStyle[m, 2, "simplicityWeighted", "logProduct", 1]
1200 * getTuningLinearProgrammingStyle[m, 2, "simplicityWeighted", "logProduct", 2]

1200 * getTuningLinearProgrammingStyle[m, 2, "complexityWeighted", "noop", 1]
1200 * getTuningLinearProgrammingStyle[m, 2, "complexityWeighted", "noop", 2]
1200 * getTuningLinearProgrammingStyle[m, 2, "complexityWeighted", "logProduct", 1]
1200 * getTuningLinearProgrammingStyle[m, 2, "complexityWeighted", "logProduct", 2]


1200 * getTuningLinearProgrammingStyle[m, 1]

1200 * getTuningLinearProgrammingStyle[m, 1, "simplicityWeighted", "noop", 1]
1200 * getTuningLinearProgrammingStyle[m, 1, "simplicityWeighted", "noop", 2]
1200 * getTuningLinearProgrammingStyle[m, 1, "simplicityWeighted", "logProduct", 1]
1200 * getTuningLinearProgrammingStyle[m, 1, "simplicityWeighted", "logProduct", 2]

1200 * getTuningLinearProgrammingStyle[m, 1, "complexityWeighted", "noop", 1]
1200 * getTuningLinearProgrammingStyle[m, 1, "complexityWeighted", "noop", 2]
1200 * getTuningLinearProgrammingStyle[m, 1, "complexityWeighted", "logProduct", 1]
1200 * getTuningLinearProgrammingStyle[m, 1, "complexityWeighted", "logProduct", 2]
