optimizeGeneratorsTuningMap[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "minisos-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}]
optimizeGeneratorsTuningMap[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "diamond minisos-U"]
optimizeGeneratorsTuningMap[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "minimax-S"]

N[{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]] // MatrixForm



N[{12g1 - 1200 , 19g1 - 1901.955, 28g1 - 2786.31}.{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]]

optimizeGeneratorsTuningMap[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "minisos-C", "targetedIntervals" -> {{{-2, 0, 1}, {1, 1, -1}, {-1, 1, 0}}, "contra"}]
plotDamage[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "minisos-C", "targetedIntervals" -> {{{-2, 0, 1}, {1, 1, -1}, {-1, 1, 0}}, "contra"}]

N[{12, 19, 28}.{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]] // MatrixForm

A = {12, 19, 28}.{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]
N[A.Transpose[A]]

N[{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]].{12, 19, 28}.{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]] // MatrixForm

1 / 842.9827025500633`

N[Transpose[{{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]].{12, 19, 28}.{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]}].{{1 / 842.9827025500633`}}] // MatrixForm

N[{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]].{12, 19, 28}.{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]] // MatrixForm

MTWT = {12, 19, 28}.{{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]
% // N // MatrixForm

TW = {{-2, 1, -1}, {0, 1, 1}, {1, -1, 0}}.DiagonalMatrix[Log2[{20, 30, 6}]]
% // N // MatrixForm

TW.MTWT
% // N // MatrixForm


TW.MTWT * 1 / 842.9827025500633`

testClose[optimizeGeneratorsTuningMap, {{{12, 19, 28}}, "co"}, "targetedIntervals" -> "diamond", "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted", {1200.000, 696.578}];

testClose[optimizeGeneratorsTuningMap, sensamagic, "targetedIntervals" -> "diamond", "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted", {1200.000, 696.578, 100}];

optimizeGeneratorsTuningMap[meantone7, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> {{{-2, 0, 1, 0}, {-1, 1, 0, 0}, {-2, 0, 0, 1}, {1, 1, -1, 0}, {0, 0, -1, 1}, {-1, -1, 0, 1}}, "contra"}]

getGeneratorsTuningMapDamageMean[meantone7, {1211.3614691597649652977`7.0832737555602145, 1913.924794386560051862`7.281924868642176}, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> {{{-2, 0, 1, 0}, {-1, 1, 0, 0}, {-2, 0, 0, 1}, {1, 1, -1, 0}, {0, 0, -1, 1}, {-1, -1, 0, 1}}, "contra"}]

plotDamage[meantone7, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> {{{-2, 0, 1, 0}, {-1, 1, 0, 0}, {-2, 0, 0, 1}, {1, 1, -1, 0}, {0, 0, -1, 1}, {-1, -1, 0, 1}}, "contra"}]

plotDamage[meantone, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}]

img = Module[
  {col = 0},
  
  bob = Table[
    col = 1 - col;
    {0, 0, 0, col}, {j, 4}, {i, 4}
  ];
  Print[bob // MatrixForm];
  Print[bob ];
  
  Image[
    {
      {{0, 0, 0, 1}, {0, 0, 0, 0}},
      {{0, 0, 0, 0}, {0, 0, 0, 1}}
    },
    ColorSpace -> "RGB"
  ]
]

Map[
  Map[
    If[
      # == 1,
      {0, 0, 0, 1},
      {0, 0, 0, 0}
    ]&,
    #
  ]&,
  Array[(-1)^+ ## &, {8, 8}]
]

plotDamage[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "unchanged-octave minimax-U", "targetedIntervals" -> {{{1, 0, 0}, {0, 0, 1}, {-1, 0, 1}}, "contra"}]

rank1 = {{{87, 138, 202, 244, 301, 322}}, "co"};
(* dual[rank1]; *)
(* {{{-46, 29, 0, 0, 0, 0}, {-26, 12, 3, 0, 0, 0}, {-36, 18, 2, 1, 0, 0}, {-43, 22, 2, 0, 1, 0}, {-48, 25, 2, 0, 0, 1}}, "contra"} *)
rank2 = dual[{{{-26, 12, 3, 0, 0, 0}, {-36, 18, 2, 1, 0, 0}, {-43, 22, 2, 0, 1, 0}, {-48, 25, 2, 0, 0, 1}}, "contra"}];
rank3 = dual[{{{-36, 18, 2, 1, 0, 0}, {-43, 22, 2, 0, 1, 0}, {-48, 25, 2, 0, 0, 1}}, "contra"}];
rank4 = dual[{{{-43, 22, 2, 0, 1, 0}, {-48, 25, 2, 0, 0, 1}}, "contra"}];
rank5 = dual[{{{-48, 25, 2, 0, 0, 1}}, "contra"}];

targetedIntervals = {IdentityMatrix[6], "contra"};

g1 = optimizeGeneratorsTuningMap[rank1, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals];
(*g2 = optimizeGeneratorsTuningMap[rank2, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals];
g3 = optimizeGeneratorsTuningMap[rank3, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals];
g4 = optimizeGeneratorsTuningMap[rank4, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals];
g5 = optimizeGeneratorsTuningMap[rank5, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals];

getGeneratorsTuningMapDamageMean[rank1, g1, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals]
getGeneratorsTuningMapDamageMean[rank2, g2, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals]
getGeneratorsTuningMapDamageMean[rank3, g3, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals]
getGeneratorsTuningMapDamageMean[rank4, g4, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals]
getGeneratorsTuningMapDamageMean[rank5, g5, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> targetedIntervals]*)


optimizeGeneratorsTuningMap[meantone7, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> {{{-2, 0, 1, 0}, {-1, 1, 0, 0}, {-2, 0, 0, 1}, {1, 1, -1, 0}, {0, 0, -1, 1}, {-1, -1, 0, 1}}, "contra"}]

optimizeGeneratorsTuningMap[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}]

M = {{1, 1, 1, 2}, {0, 2, 3, 2}, {0, 0, 2, 1}};(*{{1,1,-1,1},{0,2,-1,0},{0,0,2,1}};*) (* breed temperament *)
T = Transpose[{{1, 1, -1, 0}, {0, 0, -1, 1}, {3, 0, -1, 0}, {0, 2, -1, 0}, {-1, -1, 0, 1}, {2, -1, 0, 0}, {-1, 1, 0, 0}, {3, 0, 0, -1}, {0, 2, 0, -1}, {-3, 2, 0, 0}}];
W = DiagonalMatrix[Log2[{6 * 5, 7 * 5, 8 * 5, 9 * 5, 7 * 6, 4 * 3, 3 * 2, 8 * 7, 9 * 7, 9 * 8}]];
j = 1200 * Log2[{2, 3, 5, 7}];

MTW = Transpose[M.T.W];
jTW = Transpose[j.T.W];

constraint = {{0, 1, 1, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, -1, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, -1, 0, 0, 0, 0}};

M // MatrixForm
T // MatrixForm
SetAccuracy[N[W], 4] // MatrixForm

SetAccuracy[N[MTW], 4] // MatrixForm
SetAccuracy[N[jTW], 4] // MatrixForm
SetAccuracy[N[constraint.MTW.{{g1}, {g2}, {g3}}], 4] // MatrixForm
SetAccuracy[N[constraint.jTW], 4] // MatrixForm

solution = LinearSolve[constraint.MTW, constraint.jTW];
SetAccuracy[N[solution], 4] // MatrixForm

M.{3, 0, -2, 1}
M.{0, 2, 0, -1}
M.{-2, 1, -1, 1}
