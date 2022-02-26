(*thingFunction[gtm:{__?NumericQ}, complexities_, tim_, mappedTim_, meanPower_] := Module[{result},
  result = If[
    meanPower == \[Infinity],
    Max[
      MapIndexed[
        Abs[Total[MapThread[
          #1 * #2&,
          {#1, gtm}
        ]] - 1200 * Log[2, tim[[#2]]]] * complexities[[#2]]&,
        mappedTim
      ]
    ],
    Mean[
      MapIndexed[
        Power[Abs[Total[MapThread[
          #1 * #2&,
          {#1, gtm}
        ]] - 1200 * Log[2, tim[[#2]]]] * complexities[[#2]], meanPower]&,
        mappedTim
      ]
    ]
  ] // N;
  
  (*If[
    NumberQ[result],
    result,
    100000000000
  ]*)
  result
];*)









getW[tim_, unweighted_, gression_, weighting_, normNumber_] := Module[{complexities},
  complexities = Map[complexity[#, weighting, normNumber]&, tim];
  
  If[
    unweighted == True,
    Map[1, complexities],
    If[
      gression == "regressive",
      1 / complexities,
      complexities
    ]
  ] // N
];

ensureNumeric[n_, gtm_] := If[NumberQ[n], n, Print["well what the fuck is it? does this even owrk", (*gtm,  n,*) {g1 → 0.918621, g2 → 0.716689} /. n // N]];

(*given a mapping, a target intervals matrix (currently defaults to 5-odd-limit diamond), which mean to take (minimax, least squares, least absolutes; currently hardcoded to minimax),and a weighting (unweighted, or {progressive/regressive, P/F/T, 1/2/\[Infinity]}; currently defaults to progressive P1 (partch))*)
tune[m_, meanPower_ : \[Infinity], unweighted_ : False, gression_ : "regressive", weighting_ : "P", normNumber_ : "1"] := Module[{r, d, tim, ma, gtm, mappedTim, complexities, solution},
  r = getR[m];
  d = getD[m];
  tim = getDiamond[If[d == 3, 5, 7]];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  
  mappedTim = mapTim[m, tim];
  complexities = getW[tim, unweighted, gression, weighting, normNumber];
  (* Print["compsapgd", complexities];*)
  
  Print["well let's see what we got", meanPower, unweighted, gression, weighting, normNumber, gtm, mappedTim];
  
  solution = NMinimize[(*thingFunction[gtm, complexities, tim, mappedTim, meanPower]*)
    If[
      meanPower == \[Infinity],
      Max[
        MapIndexed[
          Abs[Total[MapThread[
            #1 * #2&,
            {#1, gtm}
          ]] - 1200 * Log[2, tim[[#2]]]] * complexities[[#2]]&,
          mappedTim
        ]
      ],
      Total[
        MapIndexed[
          Power[Abs[Total[MapThread[
            #1 * #2&,
            {#1, gtm}
          ]] - 1200 * Log[2, tim[[#2]]]] * complexities[[#2]], meanPower]&,
          mappedTim
        ]
      ]
    ] // ensureNumeric,
    gtm(*,
  Method -> "NelderMead"*)
  ];
  gtm /. Last[solution] // N
];



partch = tune[m, \[Infinity], False, "progressive", "P", 1]
minimax = tune[m, \[Infinity], True]
