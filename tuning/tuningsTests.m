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

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1202.387, 697.173}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1202.726, 697.258}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", {1201.695, 697.563}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.596, 697.530}];

testClose[optimizeGeneratorsTuningMap, meantone, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1202.390, 697.176}];
testClose[optimizeGeneratorsTuningMap, meantone, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1202.607, 696.741}];
testClose[optimizeGeneratorsTuningMap, meantone, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", {1201.699, 697.564}];
testClose[optimizeGeneratorsTuningMap, meantone, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.397, 697.049}];



testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1197.613, 694.787}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1197.437, 694.976}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", {1197.983, 694.712}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1198.160, 695.012}];


testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted", {1199.022, 695.601}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1200.070, 696.005}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1200.742, 696.205}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", {1200.985, 696.904}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.127, 696.905}];

testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, {1198.396, 695.289}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNegateLogPrimeCoordination" -> True, "complexityNormPower" -> 2, {1198.244, 695.294}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", {1198.085, 694.930}];
testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1197.930, 694.911}];


testClose[optimizeGeneratorsTuningMap, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted", {1199.022, 695.601}];

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

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-U", {600.000, 108.125}];

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NS", {596.496, 106.108}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NES", {598.230, 106.547}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-S", {598.444, 107.706}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-ES", {599.682, 108.372}];

testClose[optimizeGeneratorsTuningMap, pajara, "targetedIntervals" -> {}, "systematicTuningName" -> "minimax-NS", {597.120, 103.295}];
testClose[optimizeGeneratorsTuningMap, pajara, "targetedIntervals" -> {}, "systematicTuningName" -> "minimax-NES", {598.345, 106.693}];
testClose[optimizeGeneratorsTuningMap, pajara, "targetedIntervals" -> {}, "systematicTuningName" -> "minimax-S", {598.447, 106.567}];
testClose[optimizeGeneratorsTuningMap, pajara, "targetedIntervals" -> {}, "systematicTuningName" -> "minimax-ES", {598.859, 106.844}];

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NC", {601.517, 108.012}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-NEC", {601.829, 108.324}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-C", {601.556, 108.012}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minimax-EC", {600.318, 108.185}];


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

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NS", {597.851, 106.643}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NES", {598.310, 106.798}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-S", {598.436, 106.672}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-ES", {598.762, 106.835}];

testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NC", {601.397, 106.145}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-NEC", {601.397, 106.145}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-C", {600.000, 106.843}];
testClose[optimizeGeneratorsTuningMap, pajara, "systematicTuningName" -> "minisum-EC", {600.000, 106.843}];


(* optimizeGeneratorsTuningMap, by "originalTuningName" *)

(* this is simply some coverage that they match the above, to make sure that I dunno some of the originalTuningName things map to the correct options *)
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "minimax", {600.000, 108.125}];
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "least squares", {599.450, 107.15}];
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "TOP", {598.447, 106.567}];
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "TIPTOP", {598.447, 106.567}];
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "TE", {598.859, 106.844}];
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "Frobenius", {598.345, 106.693}];
(* TODO: include inharmonic TE in my list of originalTuningName (and support in systematicTuningName somehow... do something for now, and then ask Dave what he thinks) *)

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

(* TOP / TIPTOP *)
(* TODO: resolve discrepancies; for these sorts of things, graphs would be really good, to visually point out reasonable answers *)
accuracy = 1;
testClose[optimizeGeneratorsTuningMap, meantone, "originalTuningName" -> "TOP", {1201.70, 1201.70 - 504.13}]; (* [5](Table 1) *)
(*testClose[optimizeGeneratorsTuningMap, blackwood, "originalTuningName" -> "TOP", {238.87, 238.87*11 +158.78}]; (* [5](Table 1)*) (*this one is way off *) *)
testClose[optimizeGeneratorsTuningMap, dicot, "originalTuningName" -> "TOP", {1207.66, 353.22}];(* [5](Table 1) *)
(*testClose[optimizeGeneratorsTuningMap, augmented, "originalTuningName" -> "TOP", {399.02,399.02*5- 93.15}]; (* [5](Table 1) *) (* this one is way off *)*)
testClose[optimizeGeneratorsTuningMap, mavila, "originalTuningName" -> "TOP", {1206.55, 1206.55 + 685.03}];(* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, porcupine, "originalTuningName" -> "TOP", {1196.91, 1034.59 - 1196.91}]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, srutal, "originalTuningName" -> "TOP", {599.56, 599.56 * 4 - 494.86}];(* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, hanson, "originalTuningName" -> "TOP", {1200.29, 317.07}];(* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, magic, "originalTuningName" -> "TOP", {1201.28, 380.80}]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, negri, "originalTuningName" -> "TOP", {1201.82, 1201.82 - 1075.68}]; (* [5] as "negripent" (Table 1) *)
testClose[optimizeGeneratorsTuningMap, tetracot, "originalTuningName" -> "TOP", {1199.03, 176.11}]; (* [5](Table 1) *)
testClose[optimizeGeneratorsTuningMap, meantone7, "originalTuningName" -> "TOP", {1201.70, 1201.70 * 2 - 504.13}]; (* [5](Table 2) *)
testClose[optimizeGeneratorsTuningMap, magic7, "originalTuningName" -> "TOP", {1201.28, 380.80}]; (* [5] (Table 3) *)
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "TOP", {598.45, 598.45 - 491.88}];  (* [5](Table 2) *)
(*testClose[optimizeGeneratorsTuningMap, augene, "originalTuningName" -> "TOP", {399.02, 399.02 * 5 - 90.59}]; (* [5] (Table 2) slightly off *)*)
testClose[optimizeGeneratorsTuningMap, sensi, "originalTuningName" -> "TOP", {1198.39, 1198.39 - 755.23}]; (* [5] as "sensisept" (Table 2) *)
accuracy = 3;

(* TE *)
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "TE", {1201.397, 1898.446, 2788.196}]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "TE", {1194.308, 1910.892, 2786.314}]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "originalTuningName" -> "TE", {1206.410, 1907.322, 2763.276}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=3_7&limit=5 *)
testClose[optimizeTuningMap, augmented, "originalTuningName" -> "TE", {1197.053, 1901.955, 2793.123}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=12_3&limit=5 *)
testClose[optimizeTuningMap, mavila, "originalTuningName" -> "TE", {1208.380, 1892.933, 2779.860}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=7_2p&limit=5 *)
testClose[optimizeTuningMap, porcupine, "originalTuningName" -> "TE", {1199.562, 1907.453, 2779.234}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=7_15&limit=5 *)
testClose[optimizeTuningMap, srutal, "originalTuningName" -> "TE", {1198.823, 1903.030, 2787.467}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=12_34&limit=5 *)
testClose[optimizeTuningMap, hanson, "originalTuningName" -> "TE", {1200.166, 1902.303, 2785.418}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=53_19&limit=5 *)
testClose[optimizeTuningMap, magic, "originalTuningName" -> "TE", {1201.248, 1902.269, 2782.950}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=5 *)
testClose[optimizeTuningMap, negri, "originalTuningName" -> "TE", {1202.347, 1900.691, 2782.698}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=19_10&limit=5 *)
testClose[optimizeTuningMap, tetracot, "originalTuningName" -> "TE", {1199.561, 1903.942, 2784.419}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=7_34&limit=5 *)
testClose[optimizeTuningMap, meantone7, "originalTuningName" -> "TE", {1201.242, 1898.458, 2788.863, 3368.432}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=12_19&limit=7 *)
testClose[optimizeTuningMap, magic7, "originalTuningName" -> "TE", {1201.082, 1903.476, 2782.860, 3367.259}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=19_22&limit=7 *)
testClose[optimizeTuningMap, pajara, "originalTuningName" -> "TE", {1197.719, 1903.422, 2780.608, 3379.468}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=12_10&limit=7 *)
testClose[optimizeTuningMap, augene, "originalTuningName" -> "TE", {1196.255, 1903.298, 2791.261, 3370.933}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=12_15&limit=7 *)
testClose[optimizeTuningMap, sensi, "originalTuningName" -> "TE", {1199.714, 1903.225, 2789.779, 3363.173}]; (* http://x31eq.com/cgi-bin/rt.cgi?ets=19_27&limit=7 *)
testClose[optimizeTuningMap, sensamagic, "originalTuningName" -> "TE", {1200.000, 1903.742, 2785.546, 3366.583}]; (* as "octorod" http://x31eq.com/cgi-bin/rt.cgi?ets=27_19_22&limit=7 *)

(* Frobenius *)
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "Frobenius", {1202.6068, 1899.3482, 2786.9654}]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "Frobenius", {1191.8899, 1907.0238, 2786.3137}]; (* [4] *)
testClose[optimizeTuningMap, dicot, "originalTuningName" -> "Frobenius", {1215.1441, 1907.0030, 2776.2177}]; (* [4] *)
testClose[optimizeTuningMap, augmented, "originalTuningName" -> "Frobenius", {1195.0446, 1901.9550, 2788.4374}]; (* [4] *)
testClose[optimizeTuningMap, mavila, "originalTuningName" -> "Frobenius", {1210.9365, 1897.2679, 2784.7514}]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "originalTuningName" -> "Frobenius", {1198.5953, 1908.9787, 2782.0995}]; (* [4] *)
testClose[optimizeTuningMap, srutal, "originalTuningName" -> "Frobenius", {1198.4746, 1902.5097, 2786.5911}]; (* [4] *)
testClose[optimizeTuningMap, hanson, "originalTuningName" -> "Frobenius", {1200.5015, 1902.3729, 2785.8122}]; (* [4] *)
testClose[optimizeTuningMap, magic, "originalTuningName" -> "Frobenius", {1202.3503, 1902.1900, 2785.1386}]; (* [4] *)
testClose[optimizeTuningMap, negri, "originalTuningName" -> "Frobenius", {1203.2384, 1901.2611, 2785.3885}]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "originalTuningName" -> "Frobenius", {1198.8664, 1903.9955, 2785.4068}]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "originalTuningName" -> "Frobenius", {1201.3440, 1898.5615, 2788.8699, 3368.1428}]; (* [4] *)
testClose[optimizeTuningMap, magic7, "originalTuningName" -> "Frobenius", {1202.0285, 1904.1849, 2784.8940, 3368.0151}]; (* [4] *)
testClose[optimizeTuningMap, pajara, "originalTuningName" -> "Frobenius", {1196.6908, 1901.7292, 2778.3407, 3376.6861}]; (* [4] *)
testClose[optimizeTuningMap, augene, "originalTuningName" -> "Frobenius", {1195.2617, 1901.4887, 2788.9439, 3368.5928}]; (* [4] *)
testClose[optimizeTuningMap, sensi, "originalTuningName" -> "Frobenius", {1198.2677, 1904.0314, 2790.4025, 3364.8772}]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "originalTuningName" -> "Frobenius", {1200.0000, 1904.3201, 2785.8407, 3367.8799}]; (* [4] *)

(* POTE *)
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "POTE", {1200, 1896.239, 2784.955}]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "POTE", {1200, 1920, 2799.594}]; (* [1a] *)
testClose[optimizeTuningMap, dicot, "originalTuningName" -> "POTE", {1200.000, 1897.189, 2748.594}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=3_7&tuning=po *)
testClose[optimizeTuningMap, augmented, "originalTuningName" -> "POTE", {1200.000, 1906.638, 2800.000}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_3&tuning=po *)
testClose[optimizeTuningMap, mavila, "originalTuningName" -> "POTE", {1200.000, 1879.806, 2760.582}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_2p&tuning=po *)
testClose[optimizeTuningMap, porcupine, "originalTuningName" -> "POTE", {1200.000, 1908.149, 2780.248}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_15&tuning=po *)
testClose[optimizeTuningMap, srutal, "originalTuningName" -> "POTE", {1200.000, 1904.898, 2790.204}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=12_34&tuning=po *)
testClose[optimizeTuningMap, hanson, "originalTuningName" -> "POTE", {1200.000, 1902.039, 2785.033}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=53_19&tuning=po *)
testClose[optimizeTuningMap, magic, "originalTuningName" -> "POTE", {1200.000, 1900.292, 2780.058}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_22&tuning=po *)
testClose[optimizeTuningMap, negri, "originalTuningName" -> "POTE", {1200.000, 1896.980, 2777.265}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=19_10&tuning=po *)
testClose[optimizeTuningMap, tetracot, "originalTuningName" -> "POTE", {1200.000, 1904.639, 2785.438}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=5&ets=7_34&tuning=po *)
testClose[optimizeTuningMap, meantone7, "originalTuningName" -> "POTE", {1200.000, 1896.495, 2785.980, 3364.949}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_19&tuning=po *)
testClose[optimizeTuningMap, magic7, "originalTuningName" -> "POTE", {1200.000, 1901.760, 2780.352, 3364.224}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_22&tuning=po *)
testClose[optimizeTuningMap, pajara, "originalTuningName" -> "POTE", {1200.000, 1907.048, 2785.905, 3385.905}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_10&tuning=po *)
testClose[optimizeTuningMap, augene, "originalTuningName" -> "POTE", {1200.000, 1909.257, 2800.000, 3381.486}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=12_15&tuning=po *)
testClose[optimizeTuningMap, sensi, "originalTuningName" -> "POTE", {1200.000, 1903.679, 2790.444, 3363.975}]; (* http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=19_27&tuning=po *)
testClose[optimizeTuningMap, sensamagic, "originalTuningName" -> "POTE", {1200.000, 1903.742, 2785.546, 3366.583}]; (* as "octorod" http://x31eq.com/cgi-bin/rt.cgi?limit=7&ets=27_19_22&tuning=po *)

(* POTOP / POTT *)
(* TODO: this is everything we have on the wiki and Facebook. nothing in Graham's site. nothing in Yahoo archives. but there's more places to search still. and resolve discrepancies too *)
testClose[optimizeGeneratorsTuningMap, {{{1, 4, 4}, {0, -4, -1}}, "co", {2, 7, 13}}, "originalTuningName" -> "POTOP", {1200.000, 357.794}]; (* https://en.xen.wiki/w/Chromatic_pairs#Voltage *)
(*testClose[optimizeGeneratorsTuningMap, {{{2, 2, 7, 8, 14, 5}, {0, 1, -2, -2, -6, 2}}, "co"}, "originalTuningName" -> "POTOP", {600.000, 709.184}]; *)(* https://en.xen.wiki/w/Pajara#Tuning_spectrum has {600.000, 706.843} *)
testClose[optimizeGeneratorsTuningMap, {{{1, -1, 0, 1}, {0, 10, 9, 7}}, "co"}, "originalTuningName" -> "POTOP", {1200.000, 310.196}]; (* https://en.xen.wiki/w/Myna#Tuning_spectrum *)
accuracy = 2;
testClose[optimizeTuningMap, {{{1, 3, 0, 0, 3}, {0, -3, 5, 6, 1}}, "co"}, "originalTuningName" -> "POTOP", {1200.00, 1915.71, 2807.15, 3368.59, 4161.43}]; (* Mike himself says that maybe he got this one wrong because it should have been TIP... and yeah, I can see that this one has a pair of locked primes! https://www.facebook.com/groups/xenharmonicmath/posts/2086012064872338/ but I agree with it, so it doesn't match the Kees tuning of it elsewhere in here *)
accuracy = 1;
testClose[optimizeGeneratorsTuningMap, {{{1, 2, 6, 2, 10}, {0, -1, -9, 2, -16}}, "co"}, "originalTuningName" -> "POTOP", {1200.0, 490.4}]; (* https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064  *)
testClose[optimizeGeneratorsTuningMap, {{{1, 2, 6, 2, 1}, {0, -1, -9, 2, 6}}, "co"}, "originalTuningName" -> "POTOP", {1200.0, 490.9}];(* https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064  *)
testClose[optimizeGeneratorsTuningMap, {{{1, 2, -3, 2, 1}, {0, -1, 13, 2, 6}}, "co"}, "originalTuningName" -> "POTOP", {1200.0, 491.9}];(* https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064  *)
accuracy = 3;
(*testClose[optimizeGeneratorsTuningMap, {{{1, 1, 2, 1}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}, "originalTuningName" -> "POTOP", {1200.000, 700.795, 380.759}]; *)(* https://www.facebook.com/groups/xenharmonicmath/posts/738498989623659/?comment_id=738515309622027 has {1200, 700.3907806, 384.0221726} and this was passing before introducing the non-unique check code... and it really does seem like it should have a unique solution, so the condition on that must be wrong... you should really plot this one visually and see what's happening *)
accuracy = 2;
testClose[optimizeGeneratorsTuningMap, {{{1, 1, 0}, {0, 1, 4}}, "co"}, "originalTuningName" -> "POTOP", {1200, 696.58}]; (* The POTOP generators for Septimal Meantone and 5-limit meantone, meanwhile, are identical at about 696.58 cents. (some Facebook thing sorry I lost the link *)
testClose[optimizeGeneratorsTuningMap, {{{1, 1, 0, -3}, {0, 1, 4, 10}}, "co"}, "originalTuningName" -> "POTOP", {1200, 696.58}]; (* The POTOP generators for Septimal Meantone and 5-limit meantone, meanwhile, are identical at about 696.58 cents. (some Facebook thing sorry I lost the link *)
accuracy = 3;
(*testClose[optimizeGeneratorsTuningMap, {{{1, 1, 4}, {0, 1, -2}}, "co", {2, 3, 7}}, "originalTuningName" -> "POTOP", {1200.000, 707.949}]; (* https://www.facebook.com/groups/xenharmonicmath/posts/1035558283251060/?comment_id=1041634519310103&reply_comment_id=1041649585975263 has {1200, 709.18447040211} *) *) (* TODO: comment out discrepancy ones so I dont find them hard to find later *)

(* BOP *)
(* TODO: resolve disagreement between Flora's results and mine... we disagree on ones with non-unique tunings, and I know that I take specific steps to contend with that while she doesn't, so I suspect that I'm the one who is correct here, but we'll see *)
accuracy = 1;
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "BOP", {1201.7205, 1899.3743, 2790.6149}];  (* [4] *)
(*testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "BOP", {1194.2, 1910.7, 2784.5}];  *)(* [4] has {1194.179, 1910.6865, 2784.755} *)
testClose[optimizeTuningMap, dicot, "originalTuningName" -> "BOP", {1207.4392, 1913.1137, 2767.7157}]; (* [4] *)
(*testClose[optimizeTuningMap, augmented, "originalTuningName" -> "BOP", {1197.2, 1903.6, 2793.4}];  *)(* [4] has {1197.1684, 1901.5207, 2793.3928} *)
testClose[optimizeTuningMap, mavila, "originalTuningName" -> "BOP", {1206.5842, 1892.0787, 2769.8534}];  (* [4] *)
testClose[optimizeTuningMap, porcupine, "originalTuningName" -> "BOP", {1196.9273, 1906.5643, 2778.6315}];  (* [4] *)
testClose[optimizeTuningMap, srutal, "originalTuningName" -> "BOP", {1199.1112, 1903.288, 2788.5356}];  (* [4] *)
testClose[optimizeTuningMap, hanson, "originalTuningName" -> "BOP", {1200.2845, 1902.3816, 2785.6025}];  (* [4] *)
testClose[optimizeTuningMap, magic, "originalTuningName" -> "BOP", {1201.2339, 1903.8058, 2783.229 }]; (* [4] *)
testClose[optimizeTuningMap, negri, "originalTuningName" -> "BOP", {1201.7937, 1899.2646, 2781.8295}]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "originalTuningName" -> "BOP", {1199.0295, 1903.4108, 2783.8874}];  (* [4] *)
testClose[optimizeTuningMap, meantone7, "originalTuningName" -> "BOP", {1201.7205, 1899.3742, 2790.615, 3371.376 }]; (* [4] *)
testClose[optimizeTuningMap, magic7, "originalTuningName" -> "BOP", {1201.2339, 1903.8057, 2783.2289, 3367.8997}];  (* [4] *)
testClose[optimizeTuningMap, pajara, "originalTuningName" -> "BOP", {1197.3096, 1902.8075, 2779.5876, 3378.2424}];  (* [4] *)
(*testClose[optimizeTuningMap, augene, "originalTuningName" -> "BOP", {1197.2, 1905.1, 2793.4, 3372.8}];  *)(* [4] has  {1197.1684, 1903.995, 2793.3928, 3375.0201} *)
testClose[optimizeTuningMap, sensi, "originalTuningName" -> "BOP", {1198.5891, 1903.5232, 2789.8411, 3363.8876}]; (* [4] *)
(*testClose[optimizeTuningMap, sensamagic, "originalTuningName" -> "BOP", {1199.5, 1903.2, 2784.2, 3365.9}]; *)(* [4] has  {1200.3433, 1903.2071, 2784.2269, 3365.9043}*)
accuracy = 3;

(* BE *)
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "BE", {1201.4768, 1898.6321, 2788.6213}]; (* [4] *)
testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "BE", {1193.9975, 1910.396, 2786.3137}]; (* [4] *)
testClose[optimizeTuningMap, dicot, "originalTuningName" -> "BE", {1205.8488, 1906.3416, 2761.9439}]; (* [4] *)
testClose[optimizeTuningMap, augmented, "originalTuningName" -> "BE", {1197.2692, 1901.9550, 2793.6282}]; (* [4] *)
testClose[optimizeTuningMap, mavila, "originalTuningName" -> "BE", {1208.5464, 1893.7139, 2778.683 }]; (* [4] *)
testClose[optimizeTuningMap, porcupine, "originalTuningName" -> "BE", {1199.5668, 1906.8283, 2778.1916}]; (* [4] *)
testClose[optimizeTuningMap, srutal, "originalTuningName" -> "BE", {1198.8183, 1902.9219, 2787.6566}]; (* [4] *)
testClose[optimizeTuningMap, hanson, "originalTuningName" -> "BE", {1200.1533, 1902.2425, 2785.3554}]; (* [4] *)
testClose[optimizeTuningMap, magic, "originalTuningName" -> "BE", {1201.1456, 1902.2128, 2782.7337}]; (* [4] *)
testClose[optimizeTuningMap, negri, "originalTuningName" -> "BE", {1202.2630, 1900.8639, 2782.2726}]; (* [4] *)
testClose[optimizeTuningMap, tetracot, "originalTuningName" -> "BE", {1199.5499, 1903.7780, 2784.0631}]; (* [4] *)
testClose[optimizeTuningMap, meantone7, "originalTuningName" -> "BE", {1201.3847, 1898.6480, 2789.0531, 3368.4787}]; (* [4] *)
testClose[optimizeTuningMap, magic7, "originalTuningName" -> "BE", {1200.9990, 1903.1832, 2782.6345, 3366.6407}]; (* [4] *)
testClose[optimizeTuningMap, pajara, "originalTuningName" -> "BE", {1197.9072, 1903.2635, 2781.9626, 3380.9162}]; (* [4] *)
testClose[optimizeTuningMap, augene, "originalTuningName" -> "BE", {1196.4076, 1903.1641, 2791.6178, 3372.1175}]; (* [4] *)
testClose[optimizeTuningMap, sensi, "originalTuningName" -> "BE", {1199.7904, 1902.7978, 2789.2516, 3362.3687}]; (* [4] *)
testClose[optimizeTuningMap, sensamagic, "originalTuningName" -> "BE", {1200.0000, 1903.3868, 2785.5183, 3365.7078}]; (* [4] *)

(* Weil *)
(* TODO: resolve discrepancies... I'll bet it has something to do with it not doing TIP... check out tipweil.py from Mike in Yahoo archives? yes I think it's because it's using a sort of simplex type algorithm that hits these results directly. i bet my results have the tied for same damage. not sure if tehre's an equivalent to ramping up the powers on max and min... well we talked about this right? well the max is just toward infinty, and couldn't min be expressed by the oppsoite, tiny powers and then a big power? maybe? try to find the old email with Dave*)
(*testClose[optimizeTuningMap, meantone, "originalTuningName" -> "Weil", {1200.006, 1896.587, 2786.327}]; *)(* [2a] has {1200.0, 1896.578, 2786.314}*)
(*testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "Weil", {1188.722, 1901.955, 2783.470}]; *)(* [2a] has {1188.722, 1901.955, 2773.22}*)
testClose[optimizeTuningMap, dicot, "originalTuningName" -> "Weil", {1200.000, 1901.955, 2750.978}]; (* [2a] *)
(*testClose[optimizeTuningMap, augmented, "originalTuningName" -> "Weil", {1194.134, 1899.801, 2786.314}]; *)(* [2a] has  {1194.134, 1897.307, 2786.314}  *)
testClose[optimizeTuningMap, mavila, "originalTuningName" -> "Weil", {1200.0, 1881.31, 2756.07}]; (* [2a] *)
(*testClose[optimizeTuningMap, porcupine, "originalTuningName" -> "Weil", {1193.834, 1901.964, 2771.996}]; *)(* [2a] has {1193.828, 1901.955, 2771.982}*)
testClose[optimizeTuningMap, srutal, "originalTuningName" -> "Weil", {1198.222, 1901.955, 2786.314}]; (* [2a]  *)
testClose[optimizeTuningMap, hanson, "originalTuningName" -> "Weil", {1200.0, 1901.955, 2784.963}]; (* [2a] *)
(*testClose[optimizeTuningMap, magic, "originalTuningName" -> "Weil", {1200.001, 1901.956, 2780.392}]; *)(* [2a] has  {1200.0, 1901.955, 2780.391} *)
testClose[optimizeTuningMap, negri, "originalTuningName" -> "Weil", {1200.0, 1896.185, 2777.861}]; (* [2a] *)
testClose[optimizeTuningMap, tetracot, "originalTuningName" -> "Weil", {1198.064, 1901.955, 2781.819}]; (* [2a]  *)
testClose[optimizeTuningMap, meantone7, "originalTuningName" -> "Weil", {1200.0, 1896.578, 2786.314, 3365.784}]; (* [2a] *)
testClose[optimizeTuningMap, magic7, "originalTuningName" -> "Weil", {1200.0, 1901.955, 2780.391, 3364.692}]; (* [2a] *)
(*testClose[optimizeTuningMap, pajara, "originalTuningName" -> "Weil", {1193.804, 1896.997, 2771.926, 3368.828} ]; *)(* [2a] has {1193.803, 1896.996, 2771.924, 3368.826}  *)
(*testClose[optimizeTuningMap, augene, "originalTuningName" -> "Weil", {1194.134, 1901.798, 2786.314, 3361.211}]; *)(* [2a] has {1194.134, 1899.852, 2786.314, 3365.102} *)
testClose[optimizeTuningMap, sensi, "originalTuningName" -> "Weil", {1196.783, 1901.181, 2786.314, 3359.796}]; (* [2a]   *)

(* WE *)
(* TODO: I searched on Facebook, Yahoo, Xenwiki, ... also need to check in Flora's app, Sintel's app, Paul's papers, Graham's app, Scala, Discord history, and Graham's papers  *)
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "WE", {1201.391, 1898.436, 2788.182}]; (* [1a] *)
testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "WE", {1194.254, 1910.807, 2786.189}]; (* [1a] *)

(* Kees *)
(* I searched on Facebook, Yahoo, Xenwiki, Flora's app, Sintel's app, Paul's papers, Graham's app, Scala, Discord history, and Graham's papers, and this is the only actual example of a Kees tuning ever stated publicly by a human *)
accuracy = 2;
(* https://www.facebook.com/groups/xenharmonicmath/posts/2086012064872338/ *)
testClose[optimizeTuningMap, {{{1, 3, 0, 0, 3}, {0, -3, 5, 6, 1}}, "co"}, "originalTuningName" -> "Kees", {1200., 1915.93, 2806.79, 3368.14, 4161.36}];
accuracy = 3;

(* KE *)
testClose[optimizeTuningMap, meantone, "originalTuningName" -> "KE", {1200, 1896.651, 2786.605} ]; (* [1a]  *)
testClose[optimizeTuningMap, blackwood, "originalTuningName" -> "KE", {1200, 1920, 2795.126}]; (* [1a] *)
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

(* CTE *)
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=81%2F80&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, meantone, "originalTuningName" -> "CTE", {1200.000, 697.214}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=256%2F243&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, blackwood, "originalTuningName" -> "CTE", {240.000, 1200.000 * 2 + 386.314}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=25%2F24&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, dicot, "originalTuningName" -> "CTE", {1200.000, 354.664}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=128%2F125&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, augmented, "originalTuningName" -> "CTE", {400.000, 1200.000 + 701.955}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=135%2F128&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, mavila, "originalTuningName" -> "CTE", {1200.000, 1200.000 + 677.145}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=250%2F243&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, porcupine, "originalTuningName" -> "CTE", {1200.000, -164.166}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=2048%2F2025&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, srutal, "originalTuningName" -> "CTE", {600.000, 1200.000 + 705.136}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=15625%2F15552&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, hanson, "originalTuningName" -> "CTE", {1200.000, 317.059}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=3125%2F3072&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, magic, "originalTuningName" -> "CTE", {1200.000, 380.499}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=16875%2F16384&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, negri, "originalTuningName" -> "CTE", {1200.000, 125.396}];
(* https://sintel.pythonanywhere.com/result?subgroup=5&reduce=on&tenney=on&target=&edos=&commas=20000%2F19683&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, tetracot, "originalTuningName" -> "CTE", {1200.000, 176.028}];
(* https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=81%2F80%2C+126%2F125&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, meantone7, "originalTuningName" -> "CTE", {1200.000, 1200.000 + 696.952}];
(* https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=225%2F224%2C+245%2F243&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, magic7, "originalTuningName" -> "CTE", {1200.000, 380.651}];
(* https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=50%2F49%2C+64%2F63&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, pajara, "originalTuningName" -> "CTE", {600.000, 600.000 * -1 + 708.356}];
(* https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=64%2F63%2C+126%2F125&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, augene, "originalTuningName" -> "CTE", {400.000, 1200.000 + 709.595}];
(* https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=126%2F125%2C+245%2F243&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, sensi, "originalTuningName" -> "CTE", {1200.000, 1200.000 - 756.683}];
(* https://sintel.pythonanywhere.com/result?subgroup=7&reduce=on&tenney=on&target=&edos=&commas=245%2F243&submit_comma=submit *)
testClose[optimizeGeneratorsTuningMap, sensamagic, "originalTuningName" -> "CTE", {1200.000, 1200.000 + 703.742, 440.902}];


(* tuning equivalences *)

dummyTestFn[result_] := result;

(* Kees \[TildeTilde] POTOP/POTT, when the tuning is unique *)
checkKeesIsCloseToPotopWhenUniqueConjecture[t_, isUnique_] := Module[{keesTuning, potopTuning},
  keesTuning = optimizeGeneratorsTuningMap[t, "originalTuningName" -> "Kees"];
  potopTuning = optimizeGeneratorsTuningMap[t, "originalTuningName" -> "POTOP"];
  
  If[
    isUnique,
    testClose[dummyTestFn, keesTuning, potopTuning],
    testNotClose[dummyTestFn, keesTuning, potopTuning]
  ];
];
accuracy = 1;
checkKeesIsCloseToPotopWhenUniqueConjecture[meantone, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[blackwood, False];
checkKeesIsCloseToPotopWhenUniqueConjecture[dicot, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[augmented, False];
checkKeesIsCloseToPotopWhenUniqueConjecture[mavila, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[porcupine, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[srutal, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[hanson, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[magic, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[negri, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[tetracot, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[meantone7, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[magic7, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[pajara, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[augene, False];
checkKeesIsCloseToPotopWhenUniqueConjecture[sensi, True];
accuracy = 3;

(* KE \[TildeTilde] POTE *)
checkKeIsCloseToPoteConjecture[t_] := Module[{keTuning, poteTuning},
  keTuning = optimizeGeneratorsTuningMap[t, "originalTuningName" -> "KE"];
  poteTuning = optimizeGeneratorsTuningMap[t, "originalTuningName" -> "POTE"];
  
  testClose[dummyTestFn, keTuning, poteTuning];
];
accuracy = 0; (* yeah, it's not really that close... *)
checkKeIsCloseToPoteConjecture[meantone];
(*checkKeIsCloseToPoteConjecture[blackwood]; KE = {240., 2788.}; POTE = {240., 2800.} *) (* TODO: finish filling these out. I accept that some of them just aren't close. as someone said on Facebook somewhere. some are closer than others. has to do with how much error/badness/etc there is *)
(*checkKeIsCloseToPoteConjecture[dicot];
checkKeIsCloseToPoteConjecture[augmented];
checkKeIsCloseToPoteConjecture[mavila];
checkKeIsCloseToPoteConjecture[porcupine];
checkKeIsCloseToPoteConjecture[srutal];
checkKeIsCloseToPoteConjecture[hanson];
checkKeIsCloseToPoteConjecture[magic];
checkKeIsCloseToPoteConjecture[negri];
checkKeIsCloseToPoteConjecture[tetracot];
checkKeIsCloseToPoteConjecture[meantone7];
checkKeIsCloseToPoteConjecture[magic7];
checkKeIsCloseToPoteConjecture[pajara];
checkKeIsCloseToPoteConjecture[augene];
checkKeIsCloseToPoteConjecture[sensi];*)
accuracy = 3;


(* interval basis *)
(* TODO: find and include more examples of this *)

t = {{{1, 1, 5}, {0, -1, -3}}, "co", {2, 7 / 5, 11}};
testClose[optimizeGeneratorsTuningMap, t, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "formalPrimes", {1200.4181, 617.7581}];
testClose[optimizeGeneratorsTuningMap, t, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "primes", {1200.0558, 616.4318}];

t = {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}};
testClose[optimizeGeneratorsTuningMap, t, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "formalPrimes", {1201.3969, 3796.8919, 5270.7809}];
testClose[optimizeGeneratorsTuningMap, t, "targetedIntervals" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "primes", {1201.3969, 3796.8919, 5267.2719}];


(* pure-octave stretch *)

(* TODO: test cover at all *)
(* TODO: test cover to error in the case that more than one generator row affects the first column *)
(* TODO: test cover to error if the subgroup doesn't include prime 2 *)
(* TODO: test cover to find prime 2 if it's not the first prime for some reason *)



(* ___ PRIVATE ___ *)



(* dualPower *)
test[dualPower, 1, \[Infinity]];
test[dualPower, 2, 2];
test[dualPower, \[Infinity], 1];

(* getPFromUnchangedIntervals *)
test[getPFromUnchangedIntervals, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{1, 0, 0}, {-2, 0, 1}}, {{1, 1, 0}, {0, 0, 0}, {0,FractionBox["1", "4"], 1}}];

(* getDiagonalEigenvalueMatrix *)
test[getDiagonalEigenvalueMatrix, {{1, 0, 0}, {-2, 0, 1}}, {{-4, 4, -1}}, {{1, 0, 0}, {0, 1, 0}, {0, 0, 0}}];

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
test[getComplexity, {1, 1, -1}, dummy5limitTemp, "copfr", 1, 3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, "copfr", 2, \[Sqrt]3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, "logProduct", 1, 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];

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
testCloseNotList[getDamage, meantone, {1201.7, 999697.564}, "originalTuningName" -> "TOP", 0.00141543]; (* TODO: figure out why this is not failing *)
testCloseNotList[getDamage, meantone, {1199.02, 695.601}, "originalTuningName" -> "least squares", 0.0000729989];
testCloseNotList[getDamage, meantone, {1200., 696.578}, "originalTuningName" -> "minimax", 0.004];
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
