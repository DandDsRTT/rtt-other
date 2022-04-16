meantoneM7 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
marvelM11 = {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "co"};
gamelanM7 = {{{1, 1, 0, 3}, {0, 3, 0, -1}, {0, 0, 1, 0}}, "co"};
et5M5 = {{{5, 8, 12}}, "co"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "co"};
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "co"};

checkKeesIsPotopConjecture[t_] := Module[{keesTuning, topTuning},
  (*  keesTuning = optimizeGtm[t, "mean" -> "MAV", "weighted" -> True, "complexityWeighting" -> "oddLimit", "tim" -> {}];*)
  keesTuning = optimizeGtm[t, "tuning" -> "Kees"];
  topTuning = optimizeGtm[t, "tuning" -> "TOP"];
  potopTuning = 1200 * (topTuning / First[topTuning]); (* TODO: this doesn't handle nonoctave periods *)
  
  Print["keesTuning: ", keesTuning];
  Print["topTuning: ", topTuning];
  Print["potopTuning: ", potopTuning];
  
  keesTuning == topTuning
];
checkKeIsPoteConjecture[t_] := Module[{}, True]; (*same but change complexity power to 2? *)
checkLogSopfrIsTopConjecture[t_] := Module[{logSopfrTuning, topTuning},
  (*  logSopfrTuning = optimizeGtm[t, "mean" -> "MAV", "weighted" -> True, "complexityWeighting" -> "logSopfr", "tim" -> {}];*)
  logSopfrTuning = optimizeGtm[t, "tuning" -> "logSopfr"];
  topTuning = optimizeGtm[t, "tuning" -> "TOP"];
  
  Print["logSopfrTuning: ", logSopfrTuning];
  Print["topTuning: ", topTuning];
  
  logSopfrTuning == topTuning
];
checkSopfrIsBopConjecture[t_] := Module[{sopfrTuning, bopTuning},
  sopfrTuning = optimizeGtm[t, "tuning" -> "sopfr"];
  bopTuning = optimizeGtm[t, "tuning" -> "BOP"];
  (*  sopfrTuning = optimizeGtm[t, "mean" -> "MAV", "weighted" -> True, "complexityWeighting" -> "sopfr", "tim" -> {}];*)
  (*  bopTuning = optimizeGtm[t, "mean" -> "MAV", "weighted" -> True, "complexityWeighting" -> "product", "tim" -> {}];*)
  
  Print["sopfrTuning: ", sopfrTuning];
  Print["bopTuning: ", bopTuning];
  
  sopfrTuning == bopTuning
];

checkKeesIsPotopConjecture[meantoneM7]
checkKeesIsPotopConjecture[marvelM11]
checkKeesIsPotopConjecture[gamelanM7]
checkKeesIsPotopConjecture[et5M5]
checkKeesIsPotopConjecture[septimalBlackwoodM]
checkKeesIsPotopConjecture[tetracotM]

checkLogSopfrIsTopConjecture[meantoneM7]
checkLogSopfrIsTopConjecture[marvelM11]
checkLogSopfrIsTopConjecture[gamelanM7]
checkLogSopfrIsTopConjecture[et5M5]
checkLogSopfrIsTopConjecture[septimalBlackwoodM]
checkLogSopfrIsTopConjecture[tetracotM]

checkSopfrIsBopConjecture[meantoneM7]
checkSopfrIsBopConjecture[marvelM11]
checkSopfrIsBopConjecture[gamelanM7]
checkSopfrIsBopConjecture[et5M5]
checkSopfrIsBopConjecture[septimalBlackwoodM]
checkSopfrIsBopConjecture[tetracotM]





getKeesComplexity[{1, -2, 1}]
getKeesComplexity[{1, 0, 0}]
getKeesComplexity[{4, 0, 0}]

getBopComplexity[{1, -2, 1}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{1, -2, 1}, {{{12, 19, 28}}, "co"}]

getBopComplexity[{4, -1, -1}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{4, -1, -1}, {{{12, 19, 28}}, "co"}]

(* And this is why BOP tuning and Wilson tuning are the same ... *)
getBopComplexity[{1, 0, 0}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{1, 0, 0}, {{{12, 19, 28}}, "co"}]

getBopComplexity[{0, 1, 0}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{0, 1, 0}, {{{12, 19, 28}}, "co"}]

getBopComplexity[{0, 0, 1}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{0, 0, 1}, {{{12, 19, 28}}, "co"}]
