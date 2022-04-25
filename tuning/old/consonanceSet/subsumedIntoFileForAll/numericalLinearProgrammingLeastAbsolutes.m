wthThing[tima_, mappedTiIndex_] := Module[{ti, tiAsQuotient, tiAsOctaves},
  ti = First[Part[tima, mappedTiIndex]];
  tiAsQuotient = pcvToQuotient[ti];
  tiAsOctaves = Log[2, pcvToQuotient[ti]]; 
  
  tiAsOctaves
];

getLeastAbsolutesTuningLinearProgrammingStyle[m_, weighting_ : "unweighted", complexityWeighting_ : Null, complexityP_ : Null, tim_ : Null] := Module[
  {
    r,
    d,
    tima,
    ma,
    gtm,
    mappedTima,
    w,
    solution
  },
  
  r = getR[m];
  d = getD[m];
  tima = If[tim === Null, getDiamond[d], getA[tim]];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  
  mappedTima = Transpose[ma.Transpose[tima]];
  w = getW[tima, weighting, complexityWeighting, complexityP];
  
  solution = NMinimize[
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
    ],
    gtm,
    Method -> "NelderMead",
    WorkingPrecision -> 15
  ];
  
  Print["min of sum-of-absolutes: ", First[solution]];
  
  gtm /. Last[solution] // N
];

m = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)
(*m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)
m = {{{1, 1, 3, 3}, {0, 6, -7, -2}}, "co"}; (*miracle *)
m = {{{1, 0, 7, 9}, {0, 1, -3, -4}}, "co"}; (*pelogic *)
m = {{{1, 2, 3}, {0, -3, -5}}, "co"}; (*porcupine*)
m = {{{5, 8, 12}, {0, 0, -1}}, "co"}; (* blackwood *)*)

1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m]

1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "simplicityWeighted", "noop", 1]
1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "simplicityWeighted", "noop", 2]
1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "simplicityWeighted", "logProduct", 1]
1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "simplicityWeighted", "logProduct", 2]

1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "complexityWeighted", "noop", 1]
1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "complexityWeighted", "noop", 2]
1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "complexityWeighted", "logProduct", 1]
1200 * getLeastAbsolutesTuningLinearProgrammingStyle[m, "complexityWeighted", "logProduct", 2]
