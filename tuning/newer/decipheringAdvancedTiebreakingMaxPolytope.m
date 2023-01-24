graphTuningDamage[unparsedT_, tuningSchemeSpec_, optimumGeneratorTuningMap_] := Module[
  {
    T,
    forDamage,
    tuningSchemeOptions,
    tuningSchemeProperties,
    targetIntervals,
    generatorTuningMap,
    m,
    justTuningMap,
    M,
    g,
    t,
    j,
    r,
    L,
    i,
    e,
    c,
    targetIntervalGraphs,
    plotArgs,
  },
  
  plotArgs = {};
  
  T = parseTemperamentData[unparsedT];
  forDamage = True;
  tuningSchemeOptions = processTuningSchemeSpec[tuningSchemeSpec];
  tuningSchemeProperties = processTuningSchemeOptions[T, forDamage, tuningSchemeOptions];
  targetIntervals = tuningSchemeProperty[tuningSchemeProperties, "targetIntervals"];
  {generatorTuningMap, m, justTuningMap} = getTuningSchemeMappings[T];
  
  (* data *)
  M = getA[m];
  g = getL[generatorTuningMap];
  t = g.M;
  j = getL[justTuningMap];
  r = t - j;
  L = DiagonalMatrix[Log2[{2, 3, 5, 7}]];
  targetIntervalGraphs = Map[
    Function[
      {pcv},
      i = Transpose[getL[pcv]];
      e = r.i;
      c = Total[Abs[L.i]];
      (*Print["i: ", i, " e: ", e // N, " c: ", c // N];*)
      Abs[e] / c
    ],
    breakByRowsOrCols[targetIntervals]
  ];
  (*AppendTo[plotArgs,{targetIntervalGraphs, 0.01 +Max[targetIntervalGraphs]}];*)
  AppendTo[plotArgs, Max[targetIntervalGraphs]];
  
  (* range *)
  MapIndexed[
    Function[
      {optimumGeneratorTuningMapEntry, index},
      AppendTo[
        plotArgs,
        (* this is where we give it \[PlusMinus]2 ¢ around the exact tuning map *)
        {Part[getL[generatorTuningMap], First[index]], optimumGeneratorTuningMapEntry - 2.5, optimumGeneratorTuningMapEntry + 2.5}
      ]
    ],
    getL[optimumGeneratorTuningMap]
  ];
  
  (* other settings *)
  AppendTo[plotArgs, ImageSize -> 1000];
  (*AppendTo[plotArgs, ContourShading -> None];*)
  AppendTo[plotArgs, Contours -> Range[0, 10 , 0.25]];
  AppendTo[ plotArgs, ContourStyle -> Black];
  (*AppendTo[plotArgs, Exclusions->None];*)
  (*  AppendTo[plotArgs, MaxRecursion -> 6];*)
  
  (* Print[plotArgs]; *)
  
  Apply[ContourPlot, plotArgs]
  (*Apply[Plot3D, plotArgs]*)
];

optimizeGeneratorTuningMap[meantone, "TILT minimax-U"]

debug = True;
optimizeGeneratorTuningMap[meantone, {"tuningSchemeSystematicName" -> "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5, 81/80} minimax-U", "logging" -> True}]

graphTuningDamage[meantone, "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5} minimax-U", {{1200.000, 696.578}, "row"}]

graphTuningDamage[meantone, "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5, 81/80} minimax-U", {{1200.000, 696.578}, "row"}]

graphTuningDamage["[⟨5 8 0] ⟨0 0 12]}", "TILT minimax-U", {{240.000, 232.945}, "row"}]

graphTuningDamage[pajara, "OLD minimax-S", {{598.447, 107.711}, "row"}]

graphTuningDamage[pajara, "OLD minimax-S", {{598.447, 107.711}, "row"}]

r = t - j;
j = 1200 * Log2[{2, 3, 5, 7}];
t = g.M;
M = {{2, 3, 5, 6}, {0, 1, -2, -2}};
g = {g1, g2};
g1 = 598.44671;
L = DiagonalMatrix[Log2[{2, 3, 5, 7}]];

damage[i_] := Abs[r.i] / Total[Abs[L.i]];

Plot[
  {
    damage[{1, 0, 0, 0}],
    damage[{2, -1, 0, 0}],
    damage[{3, 0, -1, 0}],
    damage[{3, 0, 0, -1}],
    damage[{4, -2, 0, 0}],
    damage[{-1, 1, 0, 0}],
    damage[{1, 1, -1, 0}],
    damage[{2, 1, 0, -1}],
    damage[{-2, 0, 1, 0}],
    damage[{0, -1, 1, 0}],
    damage[{1, 0, 1, -1}],
    damage[{1, -2, 1, 0}],
    damage[{-2, 0, 0, 1}],
    damage[{-1, -1, 0, 1}],
    damage[{0, 0, -1, 1}],
    damage[{1, -2, 0, 1}],
    damage[{-3, 2, 0, 0}],
    damage[{0, 2, -1, 0}],
    damage[{0, 2, 0, -1}]
  },
  (*{g2, 106.56655,108.22392},*)
  {g2, 105, 110},
  PlotRange -> {0, 5},
  PlotLabels -> "Expressions",
  ImageSize -> 1000
]

getOld[9]

t1 = {598.44671, 108.22392}.{{2, 3, 5, 6}, {0, 1, -2, -2}}
t2 = {598.44671, 106.56655}.{{2, 3, 5, 6}, {0, 1, -2, -2}}
t1 - t2

(* 𝐺 = T𝑊𝐾(𝑀T𝑊𝐾)⁻¹ *)
j = 1200 * Log2[{{2, 3, 5, 7}}];
T = Transpose[{
  {1, 0, 0, 0},
  {2, -1, 0, 0},
  {3, 0, -1, 0},
  {3, 0, 0, -1},
  {4, -2, 0, 0},
  {-1, 1, 0, 0},
  {1, 1, -1, 0},
  {2, 1, 0, -1},
  {-2, 0, 1, 0},
  {0, -1, 1, 0},
  {1, 0, 1, -1},
  {1, -2, 1, 0},
  {-2, 0, 0, 1},
  {-1, -1, 0, 1},
  {0, 0, -1, 1},
  {1, -2, 0, 1},
  {-3, 2, 0, 0},
  {0, 2, -1, 0},
  {0, 2, 0, -1}
}];
W = Inverse[DiagonalMatrix[Log2[Map[Numerator[#] * Denominator[#]&, getOld[9]]]]];
Kprime = Transpose[{{0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0}}]; (* 8/7 and 5/3 tie damage *)
M = {{2, 3, 5, 6}, {0, 1, -2, -2}};
gZero = {{598.44671, 106.56655}}; (* arbitrarily chosen (first) generator tuning map which minimaxed damage at basic level *)
gOne = {{598.44671, 108.22392}}; (* the other one that tied *)

gDiffsWithZero = {
  First[gOne - gZero] (*{0,1.65738}*)
  (* only one in this case, but could be many *)
};
Mprime = gDiffsWithZero.{{2, 3, 5, 6}, {0, 1, -2, -2}};
tZero = gZero.M;
jprime = j - tZero;


(*
this may be key:
jprime.Gprime.Mprime.T.W.Kprime = jprime.T.W.Kprime 

so 
jPrime is the retuning map, the diff between the arbitrarily chosen tuning map and just
Gprime is like the matrix equiiv of X, like it's trying to tell us the amounts of each tuning to blend
Mprime is the mapping but then also converted by the gDiffsWithZero into the deviations from gZero
T and W are normal
Kprime is just a more limited set of rows of target-intervals that could tie
*)
Gprime = T.W.Kprime.Inverse[Mprime.T.W.Kprime]
gprime = jprime.Gprime

K = Transpose[{
  {0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  {0, 0, 0, 1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0} (* 8/7, 12/7, and 5/3 tie damage *)
}];
G = T.W.K.Inverse[M.T.W.K] // N
j.G // N

j.T.W // N

{g1, g2}.M.T.W // N

rZero = tZero - j
B = T.W.Kprime.Inverse[Mprime.T.W.Kprime]
rZero.B

tZero

gZero + gprime.Mprime

{598.447, 106.567} + {-3.107, -0.048, -7.213, 8.721}.{-0.28023, -0.138848, 0.138848, 0.0934101} * {0, 1.657}

{-3.107, -0.048, -7.213, 8.721}.{0.28023, 0.138848, -0.138848, -0.0934101} * {0, 1.657}

{-3.107, -0.048, -7.213, 8.721}.{-0.28023, -0.138848, 0.138848, 0.0934101}

j = 1200 * Log2[{{2, 3, 5}}];
T = Transpose[{
  {1, 0, 0},
  {0, 1, 0},
  {-1, 1, 0},
  {2, -1, 0},
  {-1, 0, 1},
  {0, -1, 1},
  {-2, 0, 1},
  {1, 1, -1}
}];
W = DiagonalMatrix[Log2[Map[Numerator[#] * Denominator[#]&, {2 / 1, 3 / 1, 3 / 2, 4 / 3, 5 / 2, 5 / 3, 5 / 4, 6 / 5}]]];
M = {{5, 8, 0}, {0, 0, 1}};

j.T.W // MatrixForm // N
{g1, g2}.M.T.W // MatrixForm // N

K = Transpose[{{1, 0, 1, 0, 0, 0, 0, 0}, {1, 0, 0, 0, 0, 0, 0, 1}}];
j.T.W.K // MatrixForm // N
{g1, g2}.M.T.W.K // MatrixForm // N

({236.43, 2754.56}.{{5, 8, 0}, {0, 0, 1}} - 1200 * Log2[{2, 3, 5}])



Abs[{{{2, 3, 4}, {4, -4, 3}}, "row"}]

r = {400, g2}.{{3, 0, 7}, {0, 1, 0}} - 1200 * Log2[{2, 3, 5}];

Plot[
  {
    Abs[r.{0, 1, 0}],
    Abs[r.{5, 0, -2}],
    Abs[r.{-2, 0, 1}],
    Abs[r.{0 , -1, 1}]
  },
  (*{g2, 106.56655,108.22392},*)
  {g2, 1850, 1950},
  PlotRange -> {0, 50},
  PlotLabels -> "Expressions",
  ImageSize -> 1000
]



(* HERE ON OUT IS MY FINAL EXAMPLE *)
M = {{2, 3, 5, 6}, {0, 1, -2, -2}, {0, 0, -1, -1}};
r = {600, g2, g3}.M - 1200 * Log2[{2, 3, 5, 7}];

Plot3D[
  {
    (* the flat ceiling *)
    Abs[r.{0, 0, -1, 1}], (* 7/5 *) (* yellow *)
    
    (* the ones that will make the tied line directly underneath that ceiling *)
    Abs[r.{0, 2, 0, -1}], (* 9/7 *) (* blue *)
    Abs[r.{0, 4, -2, 0}], (*81/25*)  (* green *)
    
    (* these two come in at an angle to that line, creating an interesting meta-diagonal line underneath *)
    Abs[r.{-2, 0, 1, 0}], (* 5/4 *) (* red *)
    Abs[r.{-2, 0, 0, 1}] (* 7/4 *) (* purple *)
  },
  {g2, 90, 115},
  {g3, -5, 50},
  PlotRange -> {0, 50},
  ClippingStyle -> None,
  MaxRecursion -> 6,
  ImageSize -> 1000
]

(* HERE'S WITH A BIGGER TARGET-INTERVAL SET WITHOUT COMPROMISING RESULT, SO THIS VIEW LOOKS MORE INTERESTING / REALISTIC; BASICALLY GOTTA DROP 7/3 AND 7/6 FROM THE 10-TILT, ADD THIS 81/25, AND REMOVE 2/1 AND THINGS OFF BY 2/1 *)
debug = True;
optimizeGeneratorTuningMap["[⟨2 3 5 6] ⟨0 1 -2 -2] ⟨0 0 -1 -1]}", {"tuningSchemeSystematicName" -> "held-octave {3/2, 5/3, 5/4, 7/4, 7/5, 9/4, 9/5, 9/7,  81/25} minimax-U"}]

Unprotect[D];
j = 1200 * Log2[{2, 3, 5, 7}];
L = DiagonalMatrix[Log2[{2, 3, 5, 7}]];
D = {{0, 2.9146, -11.6585}}; (* that's -4x the change for g3 as for g2, so techincally it could be {{0,1,-4}}, but then the scale on the diagram wouldn't be quite right, i.e. from 0 to 1 the b1 value *)
b = {b1};
gZero = {600, 99.0404, 27.2641};(*{600,1899.04,2774.66};*)
tZero = gZero.M;
negativeRZero = j - tZero;

damage[i_] := Abs[(b.D.M - negativeRZero).i];

Plot[
  {
    damage[{-2, 2, 0, 0}], (* 9/4 *)
    
    damage[{0, 2, 0, -1}], (* 9/7 *) (* blue (but hidden with the other green one, which is fine) *)
    damage[{0, 4, -2, 0}], (* 81/25 *) (* green *) (* looks tied with 9/7, but only in this cross-section; that's the point *)
    
    
    damage[{-2, 0, 1, 0}], (* 5/4, same damage as 5/2 and 8/5 *) (* red *)
    damage[{-2, 0, 0, 1}], (* 7/4 *) (* purple *)
    
    damage[{-1, 1, 0, 0}], (* 3/2 same damage as 3/1, 4/3, 8/3*)
    damage[{0, -1, 1, 0}], (* 5/3, same damage as 6/5 *)
    
    damage[{0, 0, -1, 1}], (* 7/5, same damage as 10/7 *) (* yellow *)
    damage[{0, 2, -1, 0}] (* 9/5 *)
    
  },
  {b1, -0.25, 1.25},
  PlotRange -> {0, 20},
  PlotLabels -> "Expressions",
  ImageSize -> 1000
]
