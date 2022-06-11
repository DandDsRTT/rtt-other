edo17first = {{{29, 46, 65}}, "co"};
realSetTuning = optimizeGeneratorsTuningMap[edo17first, "systematicTuningName" -> "primes minimax-U"]
getGeneratorsTuningMapDamageMean[edo17first, realSetTuning, "systematicTuningName" -> "primes minimax-U"]

getGeneratorsTuningMapDamages[meantone, {1197.85, 694.965} , "systematicTuningName" -> "diamond minisum-U"]
getGeneratorsTuningMapDamageMean[meantone, {1197.85, 694.965} , "systematicTuningName" -> "diamond minisum-U"]

{{1, 1, 0}, {0, 1, 4}}.{1, 1, -1}

Transpose[{{1, 0, 0}, {0, 0, 1 / 4}}].{{1, 1, 0}, {0, 1, 4}} // MatrixForm

plotDamage[pajara, "systematicTuningName" -> "diamond minisum-U"]

optimizeGeneratorsTuningMap[{{{12, 19, 28}}, "co"}, "systematicTuningName" -> "diamond minisum-U"]

plotDamage[pajara, "systematicTuningName" -> "diamond minisum-U"]

plotDamage[pajara, "systematicTuningName" -> "pure-octave-constrained diamond minimax-U"]

plotDamage[{{{12, 19}}, "co"}, "systematicTuningName" -> "primes minisum-U"]

plotDamage[{{{12, 19, 28, 35}}, "co"}, "systematicTuningName" -> "unchanged-octave minisum-U", "targetedIntervals" -> {{{-1, 1, 0, 0}, {0, -1, 1, 0}, {0, 0, -1, 1}}, "contra"} ]

M = {{1, 2, 3}, {0, -3, -5}};
T = Transpose[{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}];
W = DiagonalMatrix[Log2[{6, 12, 20, 40, 15, 30}]];

prnt[a_] := MatrixForm[SetAccuracy[N[a], 4]];

M.T // prnt
T.W // prnt
M.T.W // prnt
M.T.W.Transpose[M.T.W] // prnt
Inverse[M.T.W.Transpose[M.T.W]] // prnt
T.W.Transpose[M.T.W].Inverse[M.T.W.Transpose[M.T.W]] // prnt

optimizeGeneratorsTuningMap[{{{12, 19, 28}, {7, 11, 16}}, "co"}, "systematicTuningName" -> "diamond minimax-U"]

Transpose[{{1, 0, 0}, {0, 0, 1 / 4}}].{{1, 1, 0}, {0, 1, 4}}
Transpose[{{1, 0, 0}, {1, 0, -1 / 4}}].{{1, 2, 4}, {0, -1, -4}}

{2, 3, 5}.{-3, -1, 2}
