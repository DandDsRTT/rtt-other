thingFunction[gtm_, complexities_, tim_, mappedTim_, meanPower_] := Module[{result},
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
  ];
  
  If[
    NumberQ[result],
    result,
    100000000000
  ]
];

getW[tim_, unweighted_, gression_, weighting_, normNumber_] := Module[{complexities},
  complexities = Map[complexity[#, weighting, normNumber]&, tim];
  
  complexities = If[
    unweighted == True,
    Map[1, complexities],
    If[
      gression == "simplicityWeighted",
      1 / complexities,
      complexities
    ]
  ]
];

(*given a mapping, a target intervals matrix (currently defaults to 5-odd-limit diamond), which mean to take (minimax, least squares, least absolutes; currently hardcoded to minimax),and a weighting (unweighted, or {progressive/regressive, P/F/T, 1/2/\[Infinity]}; currently defaults to progressive P1 (partch))*)
tune[m_, meanPower_ : \[Infinity], unweighted_ : False, gression_ : "simplicityWeighted", weighting_ : "standardized", normNumber_ : "1"] := Module[{r, d, tim, ma, gtm, mappedTim, complexities, solution},
  r = getR[m];
  d = getD[m];
  tim = getDiamond[If[d == 3, 5, 7]];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  
  mappedTim = mapTim[m, tim];
  complexities = getW[tim, unweighted, gression, weighting, normNumber];
  
  solution = NMinimize[thingFunction[gtm, complexities, tim, mappedTim, meanPower], gtm, Method -> "NelderMead"];
  gtm /. Last[solution] // N
];



partch = tune[m, \[Infinity], False, "complexityWeighted", "standardized", 1]
minimax = tune[m, \[Infinity], True]



(*thingFunction[gtm : {__?NumericQ}, complexities_, tim_, mappedTim_, meanPower_] := Module[{result},
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
  ];
  
  If[
    NumberQ[result],
    result,
    100000000000
  ]
];


(*okSeparate[thing_?NumericQ, gtm:{__?NumericQ}] := Module[{solution}, 
  
];*)

(*given a mapping, a target intervals matrix (currently defaults to 5-odd-limit diamond), which mean to take (minimax, least squares, least absolutes; currently hardcoded to minimax),and a weighting (unweighted, or {progressive/regressive, P/F/T, 1/2/\[Infinity]}; currently defaults to progressive P1 (partch))*)
tune[m_, meanPower_ : \[Infinity], unweighted_ : False, gression_ : "simplicityWeighted", weighting_ : "standardized", normNumber_ : "1"(*, tim_, mPower,weighting_*)] := Module[{thing, mappedTim, complexities},
  
  
  r = getR[m];
  d = getD[m];
  tim = getDiamond[If[d == 3, 5, 7]];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  (*gtm = Array[g, r];*)
  
  mappedTim = mapTim[m, tim];
  
  complexities = Map[complexity[#, weighting, normNumber]&, tim];
  
  (* meanPower = Min[inputMeanPower, 16];*)
  
  Print["well let's see what we got", meanPower, unweighted, gression, weighting, normNumber, gtm, mappedTim];
  
  complexities = If[
    unweighted == True,
    Map[1, complexities],
    If[
      gression == "simplicityWeighted",
      1 / complexities,
      complexities
    ]
  ];
  
  
  (* Print["huhhhhhh"];
   okSeparate[thing, gtm]*)
  
  (*solution = NMinimize[{thingFunction[gtm, complexities, tim, mappedTim, meanPower], AllTrue[gtm,NumericQ]}(*?NumericQ*), gtm];*)
  (* solution = NMinimize[Hold[thingFunction[gtm, complexities, tim, mappedTim, meanPower],gtm](*?NumericQ*), gtm];*)
  solution = NMinimize[thingFunction[gtm, complexities, tim, mappedTim, meanPower](*?NumericQ*), gtm, Method -> "NelderMead"];
  (*  Print["hmm???"];*)
  gtm /. Last[solution] // N
];



partch = tune[m, \[Infinity], False, "complexityWeighted", "standardized", 1]
minimax = tune[m, \[Infinity], True]
*)
