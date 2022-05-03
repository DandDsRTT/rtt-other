failures = 0;
passes = 0;
accuracy = 3;

testClose[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AllTrue[MapThread[Abs[#1 - #2] < 10^-accuracy&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    Print[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testCloseNotList[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    Abs[actual - expectation] < 10^-accuracy,
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    Print[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testNotClose[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AnyTrue[MapThread[Abs[#1 - #2] > 10^-accuracy&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] = `` but it was not supposed to", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
  ]
];


(* GENERATORS PREIMAGE TRANSVERSAL *)


(* getGeneratorsPreimageTransversal *)
test[getGeneratorsPreimageTransversal, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{{1, 0, 0}, {-1, 1, 0}}, "contra"}];
test[getGeneratorsPreimageTransversal, {{{4, -4, 1}}, "contra"}, {{{1, 0, 0}, {0, 1, 0}}, "contra"}];


(* TUNING *)

(* some temperaments to check against *)

meantone = {{{1, 1, 0}, {0, 1, 4}}, "co"};
blackwood = {{{5, 8, 0}, {0, 0, 1}}, "co"};
dicot = {{{1, 1, 2}, {0, 2, 1}}, "co"};
augmented = {{{3, 0, 7}, {0, 1, 0}}, "co"};
mavila = {{{1, 0, 7}, {0, 1, -3}}, "co"};
porcupine = {{{1, 2, 3}, {0, 3, 5}}, "co"};
srutal = {{{2, 0, 11}, {0, 1, -2}}, "co"};
hanson = {{{1, 0, 1}, {0, 6, 5}}, "co"};
magic = {{{1, 0, 2}, {0, 5, 1}}, "co"};
negri = {{{1, 2, 2}, {0, -4, 3}}, "co"};
tetracot = {{{1, 1, 1}, {0, 4, 9}}, "co"};
meantone7 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
magic7 = {{{1, 0, 2, -1}, {0, 5, 1, 12}}, "co"};
pajara = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"};
augene = {{{3, 0, 7, 18}, {0, 1, 0, -2}}, "co"};
sensi = {{{1, -1, -1, -2}, {0, 7, 9, 13}}, "co"};
sensamagic = {{{1, 0, 0, 0}, {0, 1, 1, 2}, {0, 0, 2, -1}}, "co"};

(* optimizeGeneratorsTuningMap, by individual tuning properties *)

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted", {1200.000, 696.578}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1202.390, 697.176}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1202.728, 697.260}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", {1201.699, 697.564}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.600, 697.531}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1197.610, 694.786}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1197.435, 694.976}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", {1197.979, 694.711}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1198.155, 695.010}];


testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted", {1199.022, 695.601}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1200.070, 696.005}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1200.742, 696.205}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", {1200.985, 696.904}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.127, 696.905}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1198.396, 695.289}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1198.244, 695.294}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", {1198.085, 694.930}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1197.930, 694.911}];


testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted", {1200.000, 696.578}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1195.699, 693.352}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1195.699, 693.352}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", {1200.000, 696.578}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1200.000, 696.578}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1200.000, 696.578}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1200.000, 696.578}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", {1195.699, 693.352}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1195.699, 693.352}];


(* optimizeGeneratorsTuningMap, by "systematicTuningName" *)
(* TODO: you should probably instead just test that these map to the correct traits, or something, both for these, and for the by "originalTuningName" *)
(* TODO: you should make some diagrams and actually visually check some of these non-unique ones for pajara *)

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-U", {600.000, 108.128}];

(*TODO: seems to be some bug here ... First {} has zero length and no first element ... like it doesn't find any candidates maybe? for this and -NC *)
(*testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NS", {596.502, 106.058}];*)
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NES", {598.233, 106.938}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-S", {598.447, 107.711}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-ES", {599.682, 108.375}];

(*testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NC", {601.515, 108.014}];*)
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NEC", {601.826, 108.325}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-C", {601.553, 108.015}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-EC", {600.318, 108.188}];


testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-U", {599.450, 107.15}];

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-NS", {597.851, 106.643}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-NES", {598.310, 106.798}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-S", {598.436, 106.672}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-ES", {598.762, 106.835}];

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-NC", {601.653, 107.288}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-NEC", {601.522, 107.178}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-C", {600.655, 107.426}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisos-EC", {600.263, 107.259}];


testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-U", {600.000, 106.843}];

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NS", {596.741, 105.214}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NES", {596.741, 105.214}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-S", {596.741, 105.214}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-ES", {596.741, 105.214}];

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NC", {601.397, 106.145}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NEC", {601.397, 106.145}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-C", {600.000, 106.843}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-EC", {600.000, 106.843}];


(* and then in this section I want to have a bunch of external examples, organized by tuning first, then temperament
sources:
[1] Facebook - I have searched this for and added notes below - well no not yet, just added to Google Keep - about (Kees, KE,), not yet for (POTOP, POTT, BOP, BE, Weil, WE, minimax, least squares, TOP, TE)
[1a] https://www.facebook.com/groups/xenharmonicmath/posts/2363908480416027/?comment_id=2363994823740726 
[2] Yahoo posts - I have searched this for and added notes below about (), not yet for (POTOP, POTT, BOP, BE, Kees, KE, Weil, WE, minimax, least squares, TOP, TE)
[2a] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_21029.html
[3] Graham's temperament app - supports TE, POTE only
[3a] 
[4] Flora's temperament app - supports TE, TOP, POTE, POTOP, CTE (not BOP, BE, Kees, KE, Weil, WE, minimax, least squares)
[5] Paul's papers - pretty sure we're just talking Middle Path, so that's literally just TOP, nothing else
[5a] 
[6] Graham's papers - searched his site for POTOP, POTT, BOP, BE, Kees, KE, Weil, WE (but not yet TOP, TE, minimax, least squares) 
[6a] 
[7] Xen wiki - I have searched this for and added notes below about (), not yet for (POTOP, POTT, BOP, BE, Kees, KE, Weil, WE, minimax, least squares, TOP, TE)
[7a] https://en.xen.wiki/w/Target_tunings#Example
[7b] https://en.xen.wiki/w/Augene
[8] Sintel's app https://github.com/Sin-tel/temper (the surfaced app only has TE, and CTE, but the code itself may have more -- looks like only WE)
[9] Scala - has TOP, RMS-TOP = TE, Frobenius, that's it
[10] Discord history ... not checked yet
[11] Keenan Pepper's tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/KeenanPepper/tiptop.py
[12] Mike Battaglia's tipweil.py variation on tiptop.py https://github.com/YahooTuningGroupsUltimateBackup/YahooTuningGroupsUltimateBackup/blob/master/src/tuning-math/files/MikeBattaglia/tipweil.py
*)

(* minimax *)
(* TODO: gather some more; seems like a lot might be available on the xen wiki https://en.xen.wiki/index.php?title=Special:Search&limit=20&offset=20&profile=default&search=minimax but I think a lot of these might enforce pure octaves like the instructions on the Target tunings page do *)
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "minimax", {1200.000, 1896.578, 2786.314}]; (* [7a] *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(*testClose[optimizeTuningMap, augene, "originalTuningName" -> "minimax", {1200.000, 1908.771, 2800.000, 3382.458}]; *)(* [7b] 708.798 is all it says, and these actual numbers are mine; maybe try using the accuracy= trick now that I've developed it *)
(* sensi *)
(* sensamagic *)

(* least squares *)
(* TODO: gather some; some on wiki, but you may have to use a different set of temperaments than the above, and again as with minimax I predict you'll find lot of these might enforce pure octaves like the instructions on the Target tunings page do *)
(* meantone *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)


(* pure-octave stretch *)

(* TODO: test cover at all *)
(* TODO: test cover to error in the case that more than one generator row affects the first column *)
(* TODO: test cover to error if the subgroup doesn't include prime 2 *)
(* TODO: test cover to find prime 2 if it's not the first prime for some reason *)



(* ___ PRIVATE ___ *)



(* getProjectionAFromUnchangedIntervals *)
test[getProjectionAFromUnchangedIntervals, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{1, 0, 0}, {-2, 0, 1}}, {{1, 1, 0}, {0, 0, 0}, {0,FractionBox["1", "4"], 1}}];

(* getDiagonalEigenvalueA *)
test[getDiagonalEigenvalueA, {{1, 0, 0}, {-2, 0, 1}}, {{-4, 4, -1}}, {{1, 0, 0}, {0, 1, 0}, {0, 0, 0}}];

(* getPrimesTuningMap *)
test[getPrimesTuningMap, {{{12, 19, 28}}, "co", {2, 3, 5}}, {Log2[2], Log2[3], Log2[5]}];
test[getPrimesTuningMap, {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}}, {Log2[2], Log2[9], Log2[5], Log2[21]}];

(* getDiamond *)
test[getDiamond, 2, {{2, -1}, {-1, 1}}];
test[getDiamond, 3, {{2, -1, 0}, {3, 0, -1}, {-1, 1, 0}, {1, 1, -1}, {-2, 0, 1}, {0, -1, 1}}];
test[getDiamond, 4, {{2, -1, 0, 0}, {3, 0, -1, 0}, {3, 0, 0, -1}, {4, -2, 0, 0}, {-1, 1, 0, 0}, {1, 1, -1, 0}, {2, 1, 0, -1}, {-2, 0, 1, 0}, {0, -1, 1, 0}, {1, 0, 1, -1}, {1, -2, 1, 0}, {-2, 0, 0, 1}, {-1, -1, 0, 1}, {0, 0, -1, 1}, {1, -2, 0, 1}, {-3, 2, 0, 0}, {0, 2, -1, 0}, {0, 2, 0, -1}}];

(* octaveReduce *)
test[octaveReduce, 3, 3 / 2];
test[octaveReduce, 5, 5 / 4];
test[octaveReduce, 2 / 3, 4 / 3];

(* oddLimitFromD *)
test[oddLimitFromD, 2, 3];
test[oddLimitFromD, 3, 5];
test[oddLimitFromD, 4, 9];
test[oddLimitFromD, 5, 11];
test[oddLimitFromD, 6, 15];

(* getComplexity *)
dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "co"};
test[getComplexity, {1, 1, -1}, dummy5limitTemp, 1, True, 0, 0, False, 3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, 2, True, 0, 0, False, \[Sqrt]3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, 1, False, 0, 0, False, 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];

pcv = {1, -2, 1};
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "copfr", 1, 4];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "copfr", 2, 2.449];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 1, 6.492];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 2, 4.055];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 1, 3.322];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 2, 2.029];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 1, 3.170];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 2, 2.010];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 1, 13];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 2, 8.062];

testCloseNotList[getComplexity, pcv, dummy5limitTemp, "copfr", 1, getPcvCopfrComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 1, getPcvLogProductComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 1, getPcvLogIntegerLimitComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 1, getPcvLogOddLimitComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 1, getPcvProductComplexity[pcv]];

(* getDamage *)
(*testCloseNotList[getDamage, meantone, {1201.7, 999697.564}, "originalTuningName" -> "TOP", 0.00000172]; TODO: figure out what this freaking "malformed real" pink box error is about *)
(*testCloseNotList[getDamage, meantone, {1199.02, 695.601}, "originalTuningName" -> "least squares", 0.0000729989];TODO: figure out why this started crashing when these tests came back online because getDamage actually hadn't even been running correctly because these tests don't catch when functions are just crashing so that's another thing you could try to figure out *)
testCloseNotList[getDamage, meantone, {1200., 696.578}, "originalTuningName" -> "minimax", 5.377];
(* TODO: I'm not sure this handles pure-octave stretch and interval basis properly *)


(* primesInLockedRatio *)
test[primesInLockedRatio, {{{5, 8}}, "co"}, True];
test[primesInLockedRatio, {{{5, 8, 12}}, "co"}, True];
test[primesInLockedRatio, {{{1, 0, -4}, {0, 1, 4}}, "co"}, False];
test[primesInLockedRatio, {{{5, 8, 0}, {0, 0, 1}}, "co"}, True];
test[primesInLockedRatio, {{{5, 8, 12, 14}}, "co"}, True];
test[primesInLockedRatio, {{{1, 2, 3, 2}, {0, -3, -5, -6}}, "co"}, False];
test[primesInLockedRatio, {{{3, 0, 7, 18}, {0, 1, 0, -2}}, "co"}, True];
test[primesInLockedRatio, {{{3, 0, 7, 0}, {0, 4, 0, 7}}, "co"}, True];
test[primesInLockedRatio, {{{5, 8, 12, 0}, {0, 0, 0, 1}}, "co"}, True];
test[primesInLockedRatio, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}, False];
test[primesInLockedRatio, {{{1, 0, -4, 0}, {0, 1, 4, 0}, {0, 0, 0, 1}}, "co"}, False];
test[primesInLockedRatio, {{{5, 8, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}, "co"}, True];

(* hasIndependentGenerator *)
test[hasIndependentGenerator, {{{5, 8}}, "co"}, False];
test[hasIndependentGenerator, {{{5, 8, 12}}, "co"}, False];
test[hasIndependentGenerator, {{{1, 0, -4}, {0, 1, 4}}, "co"}, False];
test[hasIndependentGenerator, {{{5, 8, 0}, {0, 0, 1}}, "co"}, True];
test[hasIndependentGenerator, {{{5, 8, 12, 14}}, "co"}, False];
test[hasIndependentGenerator, {{{1, 2, 3, 2}, {0, -3, -5, -6}}, "co"}, False];
test[hasIndependentGenerator, {{{3, 0, 7, 18}, {0, 1, 0, -2}}, "co"}, False];
test[hasIndependentGenerator, {{{3, 0, 7, 0}, {0, 4, 0, 7}}, "co"}, False];
test[hasIndependentGenerator, {{{5, 8, 12, 0}, {0, 0, 0, 1}}, "co"}, True];
test[hasIndependentGenerator, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}, False];
test[hasIndependentGenerator, {{{1, 0, -4, 0}, {0, 1, 4, 0}, {0, 0, 0, 1}}, "co"}, True];
test[hasIndependentGenerator, {{{5, 8, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}, "co"}, True];

(* hasNonUniqueTuning *)
test[hasNonUniqueTuning, {{{5, 8}}, "co"}, False];
test[hasNonUniqueTuning, {{{5, 8, 12}}, "co"}, False];
test[hasNonUniqueTuning, {{{1, 0, -4}, {0, 1, 4}}, "co"}, False];
test[hasNonUniqueTuning, {{{5, 8, 0}, {0, 0, 1}}, "co"}, True];
test[hasNonUniqueTuning, {{{5, 8, 12, 14}}, "co"}, False];
test[hasNonUniqueTuning, {{{1, 2, 3, 2}, {0, -3, -5, -6}}, "co"}, False];
test[hasNonUniqueTuning, {{{3, 0, 7, 18}, {0, 1, 0, -2}}, "co"}, True];
test[hasNonUniqueTuning, {{{3, 0, 7, 0}, {0, 4, 0, 7}}, "co"}, True];
test[hasNonUniqueTuning, {{{5, 8, 12, 0}, {0, 0, 0, 1}}, "co"}, True];
test[hasNonUniqueTuning, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}, False];
test[hasNonUniqueTuning, {{{1, 0, -4, 0}, {0, 1, 4, 0}, {0, 0, 0, 1}}, "co"}, True];
test[hasNonUniqueTuning, {{{5, 8, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}, "co"}, True];




Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
