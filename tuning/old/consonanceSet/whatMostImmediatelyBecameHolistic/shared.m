getPrimesTuningMap[d_] := Map[Log2, Map[Prime, Range[d]]];

getDiamond[d_] := Module[{oddLimit, oddsWithinLimit, rawDiamond},
  oddLimit = oddLimitFromD[d];
  oddsWithinLimit = Range[1, oddLimit, 2];
  rawDiamond = Map[Function[outer, Map[Function[inner, outer / inner], oddsWithinLimit]], oddsWithinLimit];
  
  padVectorsWithZerosUpToD[Map[quotientToPcv, Map[octaveReduce, Select[DeleteDuplicates[Flatten[rawDiamond]], # != 1&]]], d]
];

getDiamond[3]

octaveReduce[inputI_] := Module[{i},
  i = inputI;
  While[i >= 2, i = i / 2];
  While[i < 1, i = i * 2];
  
  i
];

oddLimitFromD[d_] := Prime[d + 1] - 2;

getComplexity[pcv_, complexityWeighting_ : "noop", complexityP_ : 1] := Module[{d, weightedPcv},
  d = Length[pcv];
  weightedPcv = If[
    complexityWeighting == "noop",
    pcv,
    If[
      complexityWeighting == "logProduct",
      pcv * getPrimesTuningMap[d],
      If[
        complexityWeighting == "I",
        pcv / getPrimesTuningMap[d]
      ]
    ]
  ];
  
  Norm[weightedPcv, complexityP]
];

(* sanity checking *)
getComplexity[{-1, 1}, "logProduct", 1] // N (* 2.584962500721156` *)
getComplexity[{-2, 0, 1}, "logProduct", 1] // N (* 4.321928094887362` *)
getComplexity[{0, -1, 1}, "logProduct", 1] // N (* 3.9068905956085187` *)
getComplexity[{2, -1}, "logProduct", 1] // N (* 3.584962500721156` *)
getComplexity[{3, 0, -1}, "logProduct", 1] // N (* 5.321928094887362` *)
getComplexity[{1, 1, -1}, "logProduct", 1] // N (* 4.906890595608518` *)

getW[tima_, weighting_ : "unweighted", complexityWeighting_ : Null, complexityP_ : Null] := Module[{w},
  w = If[
    weighting == "unweighted",
    Map[1&, tima],
    Map[getComplexity[#, complexityWeighting, complexityP]&, tima]
  ];
  
  If[
    weighting == "simplicityWeighted",
    1 / w,
    w
  ]
];
