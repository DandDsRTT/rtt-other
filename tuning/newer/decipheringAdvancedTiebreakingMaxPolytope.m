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
  (*  AppendTo[plotArgs, Contours -> Range[0, 60,0.25]];
    AppendTo[ plotArgs,ContourStyle ->  Black];*)
  (*  AppendTo[plotArgs, Exclusions->None];*)
  (*  AppendTo[plotArgs, MaxRecursion -> 6];*)
  
  (* Print[plotArgs]; *)
  
  (*   Apply[ContourPlot, plotArgs]*)
  Apply[Plot3D, plotArgs]
];

graphTuningDamage[meantone, "TILT minimax-S", {{1201.699, 697.564}, "row"}]

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

rZero = tZero - j
B = T.W.Kprime.Inverse[Mprime.T.W.Kprime]
rZero.B

tZero

gZero + gprime.Mprime

{598.447, 106.567} + {-3.107, -0.048, -7.213, 8.721}.{-0.28023, -0.138848, 0.138848, 0.0934101} * {0, 1.657}

{-3.107, -0.048, -7.213, 8.721}.{0.28023, 0.138848, -0.138848, -0.0934101} * {0, 1.657}

{-3.107, -0.048, -7.213, 8.721}.{-0.28023, -0.138848, 0.138848, 0.0934101}
