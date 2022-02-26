(*given a mapping, a target intervals matrix (currently defaults to 5-odd-limit diamond), which mean to take (minimax, least squares, least absolutes; currently hardcoded to minimax),and a weighting (unweighted, or {progressive/regressive, P/F/T, 1/2/\[Infinity]}; currently defaults to progressive P1 (partch))*)
tune[m_, meanPower_ : \[Infinity], unweighted_ : False, gression_ : "regressive", weighting_ : "P", normNumber_ : "1"(*, tim_, mPower,weighting_*)] := Module[{solution, thing, mappedTim, complexities},
  
  Print["well let's see what we got", meanPower, unweighted, gression, weighting, normNumber];
  
  r = getR[m];
  d = getD[m];
  tim = getDiamond[If[d == 3, 5, 7]];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  
  mappedTim = mapTim[m, tim];
  
  complexities = Map[complexity[#, weighting, normNumber]&, tim];
  
  meanPower = Min[meanPower, 16];
  
  complexities = If[
    unweighted == True,
    Map[1, complexities],
    If[
      gression == "regressive",
      1 / complexities,
      complexities
    ]
  ];
  
  thing =
      (*      If[*)
      (*    meanPower == \[Infinity],*)
      (*    Max[*)
      (*      MapIndexed[*)
      (*        Abs[Total[MapThread[*)
      (*          #1 * #2&,*)
      (*          {#1, gtm}*)
      (*        ]] - 1200 * Log[2, tim[[#2]]]] * complexities[[#2]]&,*)
      (*        mappedTim*)
      (*      ]*)
      (*    ],*)
      Mean[
        MapIndexed[
          Power[Abs[Total[MapThread[
            #1 * #2&,
            {#1, gtm}
          ]] - 1200 * Log[2, tim[[#2]]]] * complexities[[#2]], meanPower]&,
          mappedTim
        ]
        (*    ]*)
      ];
  
  solution = NMinimize[thing, gtm];
  
  gtm /. Last[solution] // N
];

partch = tune[m, \[Infinity], False, "progressive", "P", 1]
minimax = tune[m, \[Infinity], True]
