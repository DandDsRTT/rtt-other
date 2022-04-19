jip[d_] := Map[Log2, Map[Prime, Range[d]]];

complexity[i_, weighting_, normPower_] := Module[{pcv,d,  weightedPcv},
  pcv = Map[Abs,rationalToPcv[i]];
  d = Length[pcv];
  weightedPcv = If[
    weighting == "unstandardized",
    pcv,
    If[
      weighting == "standardized",
      pcv * jip[d],
      If[
        weighting == "I",
        pcv / jip[d]
      ]
    ]
  ];
  
  Norm[weightedPcv, normPower]
];

(* sanity checking *)
complexity[3/2, "standardized", 1] // N (* 2.584962500721156` *)
complexity[5/4, "standardized", 1] // N (* 4.321928094887362` *)
complexity[5/3, "standardized", 1] // N (* 3.9068905956085187` *)
complexity[4/3, "standardized", 1] // N (* 3.584962500721156` *)
complexity[8/5, "standardized", 1] // N (* 5.321928094887362` *)
complexity[6/5, "standardized", 1] // N (* 4.906890595608518` *)

(* setup for hard-coded comparison example, for both graph and minimized result; this is 5-odd-limit tonality diamond including octave compements (to support tempered octaves) for the diamond PC-vectors mapped to GC-vectors under 5-limit meantone [⟨1 1 0] ⟨0 1 4]⟩, and we're PROGRESSIVELY weighting it (multiplying by the complexity!), and that complexity is Tenney = P1 = taxicab norm of prime tuning map weighted prime counts, so progressive Tenney = Partch, and then we're minimizing the max, so this is just Partch minimax of meantone *)
    
    complexityWeighting = "standardized";
normPower = 1;

c1 = complexity[3/2, complexityWeighting, normPower];
c2 =  complexity[5/4, complexityWeighting, normPower];
c3 = complexity[5/3, complexityWeighting, normPower];
c4 = complexity[4/3, complexityWeighting, normPower];
c5 = complexity[8/5, complexityWeighting, normPower];
c6 = complexity[6/5, complexityWeighting, normPower];

l1 = 1200 * Log[2, 3 / 2];
l2 = 1200 * Log[2, 5 /4];
l3 = 1200 * Log[2, 5 / 3];
l4 = 1200 * Log[2, 4 / 3];
l5 = 1200 * Log[2,8 / 5];
l6 = 1200 * Log[2, 6 / 5];

thing = Max[
  Abs[0 * g1 +1 * g2 - l1] * c1,
  Abs[-2 * g1 + 4 * g2 -l2 ]*c2,
  Abs[-1 * g1 + 3 * g2 -l3]*c3 ,
  Abs[1 * g1 + -1 * g2 - l4]*c4 ,
  Abs[3 * g1 + -4 * g2 -l5]*c5 ,
  Abs[2 * g1 + -3 * g2 - l6]*c6
];


    (* a 3D plot of the hard-coded example, for visual confirmation *)
    Plot3D[
      thing,
      {g1, 1190, 1210},
      {g2, 690, 710},
      PlotTheme -> "Web",
      MaxRecursion -> 8
    ]


    (* an actual solve of the hard-coded example *)
    solution =NMinimize[thing,{g1, g2}];

{g1, g2} /. Last[solution] // N


    
    
    
    
    
    
    (*******************)
    
    (* here's where you set M for the rest of it!!!! *)
    (*m = {{{1,1,0},{0,1,4}},"co"}; (* meantone *)*)
    m = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)
(*m = {{{1,1,3,3},{0,6,-7,-2}}, "co"}; (*miracle *)*)
(*m = {{{1,0,7,9},{0,1,-3,-4}},"co"}; (*pelogic *)*)
(*m = {{{1,2,3},{0,-3,-5}},"co"}; (* porcupine *)*)
(*m  = {{{5, 8, 0},{0,0,1}},"co"};(* blackwood *)*)

(*******************)











(* currently only does the 5-odd-limit diamond hard-coded *)
    getDiamond[oddLimit_] := Module[{},
      If[oddLimit == 5,
        {3/2, 5/4, 5/3, 4/3, 8/5,6/5},
        {3/2, 5/4, 5/3,7/4, 7/6, 7/5, 4/3, 8/5,6/5, 8/7, 12/7, 10/7}
      ]
    ];


    
    (* maps the target interval matrix of PC-vectors to a matrix of GC-vectors under the given temperament... well but not exactly, it's more like a list of vectors than an actual matrix... which you may or may not want to refactor *)
    mapTim[m_, tim_] := Module[{},
      d = getD[m];
      ma = getA[m];
      Map[
        (*getA[m].Transpose[{rationalToPcv[#]}]&,*)
        ma.Transpose[padD[{rationalToPcv[#]}, d]]&,
        tim
      ]
    ];

(* sanity check *)
mapTim[m, getDiamond[5]] (* {{{0},{1}},{{-2},{4}},{{-1},{3}},{{1},{-1}},{{3},{-4}},{{2},{-3}}} *)



    
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
        
        getComplexities[tim_, unweighted_, gression_, weighting_, normNumber_] := Module[{complexities},
          complexities = Map[complexity[#, weighting, normNumber]&, tim];
          
          If[
            unweighted == True,
            Map[1, complexities],
            If[
              gression == "simplicityWeighted",
              1 / complexities,
              complexities
            ]
          ] // N
        ];

ensureNumeric[n_] := If[NumberQ[n], n, Print["well what the fuck is it? does this even owrk",n /.{g1 ->(*648.26400003575*)550.26400003575,g2->(*139.86884032701985*)58.35662109968045} // N(*,(*gtm,  n,*)  // N*)]];

(*given a mapping, a target intervals matrix (currently defaults to 5-odd-limit diamond), which mean to take (minimax, least squares, least absolutes; currently hardcoded to minimax),and a weighting (unweighted, or {progressive/regressive, P/F/T, 1/2/\[Infinity]}; currently defaults to progressive P1 (partch))*)
tune[m_, meanPower_ : \[Infinity], unweighted_ : False, gression_ : "simplicityWeighted", weighting_ : "standardized", normNumber_ : "1"] := Module[{r, d, tim, ma, gtm, mappedTim, complexities, solution, guessRange},
  r = getR[m];
  d = getD[m];
  tim = getDiamond[If[d == 3, 5, 7]];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  
  mappedTim = mapTim[m, tim];
  complexities = getComplexities[tim, unweighted, gression, weighting, normNumber];
  (* Print["compsapgd", complexities];*)
  
  Print["well let's see what we got", meanPower, unweighted, gression, weighting, normNumber, gtm, mappedTim];
  
  guessRange =  1200*optimizeGtm[m, 2];
  
  solution = NMinimize[(*thingFunction[gtm, complexities, tim, mappedTim, meanPower]*)
    {
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
      ](*// ensureNumeric*),
      guessRange[[1]] - 50<=g1 <= guessRange[[1]] + 50,
      guessRange[[2]] - 50<=g2 <= guessRange[[2]] + 50
    },
    gtm,
    Method -> "NelderMead"
  ];
  Print["last soltiuon", Last[solution]];
  gtm /. Last[solution] // N
];



partch = tune[m, \[Infinity], False, "complexityWeighted", "standardized", 1]
minimax = tune[m, \[Infinity], True]


dualNorm[norm_] := If[norm == 1, Infinity, 1 / (1 - 1 / norm)];

getW[d_, weight_] := If[
  weight == "I",
  DiagonalMatrix[jip[d]],
  If[
    weight == "standardized",
    DiagonalMatrix[1 / jip[d]],
    If[
      weight == "unstandardized",
      DiagonalMatrix[Table[1, d]],
      Error
    ]
  ]
];

precision = 7;

optimizeGtm[m_, norm_, weight_ : "unstandardized"] := If[
  norm == 2,
  optimizeGtmWithPseudoinverse[m, weight],
  optimizeGtmWithMinimizer[m, norm, weight]
];

optimizeGtmWithPseudoinverse[m_, weight_] := Module[{d, ma, w, g, ptm, gtm, tm, e},
  d = getD[m];
  ma = getA[getM[m]];
  w = getW[d, weight];
  g = PseudoInverse[ma.w];
  ptm = w.Log[2, getPrimes[d]];
  gtm = ptm.g;
  
  (* this stuff is all only for debugging *)
  (*  tm = gtm.ma.w;
    e = tm - ptm;
    Print["\n w: ", w, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ", precision * 1200 // N, "\n p: ", ptm, " = ", ptm * 1200 // N, "\n t: ", tm, " = ", tm * 1200 // N, "\n e: ", e, " = ", e * 1200 // N, "\n G: ", g];*)
  
  gtm // N
];

optimizeGtmWithMinimizer[m_, norm_, weight_] := Module[{r, d, ma, gtm, ptm, tm, e, optimalGtmSolution, w},
  
  r = getR[m];
  d = getD[m];
  ma = getA[m];
  gtm = Table[Symbol["g" <> ToString@gtmIndex], {gtmIndex, 1, r}];
  ptm = Log[2, getPrimes[d]];
  tm = gtm.ma;
  e = tm - ptm;
  w = getW[d, weight];
  
  (*Print["about to minimize! here's e.w: ", e.w," and the dual norm is: ", dualNorm[norm]];*)
  optimalGtmSolution = NMinimize[Norm[e.w, dualNorm[norm]], gtm, Method -> "NelderMead"];
  
  
  (* debugging *)
  (*  Print["\n w: ", w, "\n r: ", r, "\n d: ", d, "\n m: ", ma, "\n g: ", gtm, " = ", 1200 * gtm /. Last[optimalGtmSolution] // N, "\n p: ", ptm, " = ", 1200 * ptm // N, "\n t: ", tm, " = ", 1200 * tm /. Last[optimalGtmSolution] // N, "\n e: ", e, " = ", 1200 * e /. Last[optimalGtmSolution] // N];*)
  
  gtm /. Last[optimalGtmSolution] // N
];

f1 = 1200*optimizeGtm[m, 1]
f2 = 1200*optimizeGtm[m, 2]
f\[Infinity] = 1200*optimizeGtm[m, \[Infinity]]

p1 = 1200*optimizeGtm[m, 1, "standardized"]
p2 = 1200*optimizeGtm[m, 2, "standardized"]
p\[Infinity] = 1200*optimizeGtm[m, \[Infinity], "standardized"]

i1 = 1200*optimizeGtm[m, 1, "I"]
i2 = 1200*optimizeGtm[m, 2, "I"]
i\[Infinity] = 1200*optimizeGtm[m, \[Infinity], "I"]

data = {
  {f1, "MS"},
  {f2, "MES"},
  {f\[Infinity], "F\[Infinity]"},
  {p1, "P1 (Tenney)"},
  {p2, "P2 (Breed)"},
  {p\[Infinity], "P\[Infinity]"},
  {i1, "I1"},
  {i2, "I2"},
  {i\[Infinity], "I\[Infinity]"},
  {partch, "progressive P1 (Partch)"}
};

Show[ListPlot[MapThread[Callout[#1, #2, LabelVisibility->All]&,{Map[First,data], Map[Last, data]}], PlotRange->All], ImageSize ->1000]

1200 *optimizeGtm[{{{1,1,0},{0,1,4}},"co"},1, "standardized"]
