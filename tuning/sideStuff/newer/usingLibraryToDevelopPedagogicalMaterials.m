
getTuningMapMeanDamage[meantone, {1202, 1900, 2792}, "systematicTuningSchemeName" -> "diamond minisum-C", "debug" -> True]

graphTuningDamage[{{{17, 27, 40}}, "co"}, "systematicTuningSchemeName" -> "diamond minimax-U"]

getTuningMapMeanDamage[meantone, {1200.000, 1894.786, 2779.144}, "systematicTuningSchemeName" -> "diamond minisum-S"]

graphTuningDamage[meantone, "systematicTuningSchemeName" -> "diamond minisum-U"]

SetAccuracy[{{1200.000, 694.786}}.getA[meantone], 4]

optimizeGeneratorsTuningMap[{{{1, 2, 3}, {0, -3, -5}}, "co"}, "systematicTuningSchemeName" -> "diamond minisos-C"]

{{1, 2, 3}, {0, -3, -5}}.Transpose[{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}] // MatrixForm

SetAccuracy[N[Transpose[{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}] .DiagonalMatrix[Log2[{6, 12, 20, 40, 15, 30}]]], 4] // MatrixForm

dummy = {{{12, 19, 28}}, "co"}
getPcvLogProductComplexity[{0, 0, 0}, dummy]
getPcvLogProductComplexity[{1, 0, 0}, dummy]
getPcvLogProductComplexity[{2, 0, 0}, dummy]
prime 5
N[getPcvLogProductComplexity[{0, 0, 2}, dummy]]
N[getPcvLogProductComplexity[{1, 1, 1}, dummy]]


N[getPcvLogProductComplexity[{0, 0, 3}, dummy]]
N[getPcvLogProductComplexity[{0, 0, 1}, dummy]]
N[getPcvLogProductComplexity[{4, 1, 1}, dummy]]

optimizeGeneratorsTuningMap[meantone, "systematicTuningSchemeName" -> "minimax-S"]

edo17first = {{{29, 46, 67}}, "co"};(* {{{17, 27, 39}},"co"};*)

a = optimizeGeneratorsTuningMap[edo17first, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}, "contra"} ]
b = optimizeGeneratorsTuningMap[edo17first, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"} ]

getGeneratorsTuningMapMeanDamage[edo17first, a, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}, "contra"} ]
getGeneratorsTuningMapMeanDamage[edo17first, b, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}, "contra"}  ]

(*getGeneratorsTuningMapMeanDamage[edo17first, a, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" ->{IdentityMatrix[3], "contra"} ]*)
getGeneratorsTuningMapMeanDamage[edo17first, b, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}  ]

edo17second = (*{{{29,46, 66}},"co"};*){{{17, 27, 40}}, "co"};

realSetTuning = optimizeGeneratorsTuningMap[edo17second, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}, "contra"} ]
onlyPrimesTuning = optimizeGeneratorsTuningMap[edo17second, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"} ]

getGeneratorsTuningMapMeanDamage[edo17second, realSetTuning, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}, "contra"} ]
getGeneratorsTuningMapMeanDamage[edo17second, onlyPrimesTuning, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}, "contra"}  ]

(*getGeneratorsTuningMapMeanDamage[edo17second, realSetTuning, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" ->{IdentityMatrix[3], "contra"} ]*)
getGeneratorsTuningMapMeanDamage[edo17second, onlyPrimesTuning, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}  ]



graphTuningDamage[meantone, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}, "contra"}]

graphTuningDamage[meantone, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{0, 1, 0}, {0, 1, 0}, {0, 1, 0}}, "contra"}]

graphTuningDamage[meantone, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}]

graphTuningDamage[meantone, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}]



graphTuningDamage[{{{12, 19, 28}}, "co"}, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {IdentityMatrix[3], "contra"}]

graphTuningDamage[{{{12, 19, 28}}, "co"}, "systematicTuningSchemeName" -> "minimax-U", "targetedIntervals" -> {{{-1, 1, 0}, {2, -1, 0}, {-2, 0, 1}, {3, 0, -1}, {0, -1, 1}, {1, 1, -1}}, "contra"}]

InterpretationBox[RowBox[{"\"Transpose[temperedSideMinusGeneratorsPart].Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]: \"", "\[InvisibleSpace]", TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{{ItemBox["0.06862211662442956`"], ItemBox["0.001408659436162714`"]}, {ItemBox["0.0381896564402779`"], ItemBox["0.010776282186612947`"]}, {ItemBox["0.08403917845918134`"], ItemBox[RowBox[{"-", "0.006305843235162428`"}]]}, {ItemBox["0.09448848512024252`"], ItemBox["0.026662574999428368`"]}, {ItemBox["0.11758791399690893`"], ItemBox["0.006043698599540275`"]}, {ItemBox["0.03484787097933358`"], ItemBox["0.00983330373404234`"]}}, RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}], SequenceForm["Transpose[temperedSideMinusGeneratorsPart].Inverse[temperedSideMinusGeneratorsPart.Transpose[temperedSideMinusGeneratorsPart]]: ", MatrixForm[{{0.06862211662442956`, 0.001408659436162714`}, {0.0381896564402779`, 0.010776282186612947`}, {0.08403917845918134`, -0.006305843235162428`}, {0.09448848512024252`, 0.026662574999428368`}, {0.11758791399690893`, 0.006043698599540275`}, {0.03484787097933358`, 0.00983330373404234`}}]], Editable -> False]
