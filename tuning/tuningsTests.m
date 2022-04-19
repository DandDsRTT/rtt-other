failures = 0;
passes = 0;

testClose[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AllTrue[MapThread[Abs[#1 - #2] < 0.001&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, 4]], 14, Red]];
    Print[ToString[SetAccuracy[actual, 4]]];
  ]
];


(* GENERATORS PREIMAGE TRANSVERSAL *)


(* getGpt *)
test[getGpt, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{{1, 0, 0}, {-1, 1, 0}}, "contra"}];
test[getGpt, {{{4, -4, 1}}, "contra"}, {{{1, 0, 0}, {0, 1, 0}}, "contra"}];


(* TUNING *)

(* some temperaments to check against *)

meantone = {{{1, 1, 0}, {0, 1, 4}}, "co"};
blackwood = {{{5, 8, 0}, {0, 0, 1}}, "co"};
dicot = {{ {1, 1, 2}, {0, 2, 1}}, "co"};
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

(* optimizeGtm, by individual tuning properties *)

testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted", {1202.387, 697.173}];

testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", {1202.387, 697.173}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", "complexityNormPower" -> 2, {1202.726, 697.258}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", {1201.695, 697.563}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.596, 697.530}];

testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", {1202.390, 697.174}];
testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", "complexityNormPower" -> 2, {1202.607, 696.741}];
testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", {1201.698, 697.563}];
testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.397, 697.049}];

testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "unstandardized", {1197.613, 694.787}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "unstandardized", "complexityNormPower" -> 2, {1197.437, 694.976}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", {1197.983, 694.712}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1198.160, 695.012}];


testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted", {1199.022, 695.601}];

testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", {1200.070, 696.005}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", "complexityNormPower" -> 2, {1200.742, 696.205}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", {1200.985, 696.904}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.127, 696.905}];

testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "unstandardized", {1198.396, 695.289}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "unstandardized", "complexityNormPower" -> 2, {1198.244, 695.294}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", {1198.085, 694.930}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1197.930, 694.911}];


(*testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted", {1200.000, 696.578}];*)
optimizeGtm[meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted"];

testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", {1195.699, 693.352}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "unstandardized", "complexityNormPower" -> 2, {1195.699, 693.352}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", {1200.000, 696.578}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1200.000, 696.578}];

testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "unstandardized", {1200.000, 696.578}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "unstandardized", "complexityNormPower" -> 2, {1200.000, 696.578}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", {1195.699, 693.352}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1195.699, 693.352}];


(* optimizeGtm, by "systematicTuningName" *)
(* TODO: you should probably instead just test that these map to the correct traits, or something *)
(* TODO: you should make some diagrams and actually visually check some of these non-unique ones for pajara *)

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-U", {600.000, 108.125}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MS", {596.496, 106.108}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MES", {598.230, 106.547}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-S", {598.444, 107.706}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-ES", {599.682, 108.372}];

testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-MS", {597.123, 103.304}];
testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-MES", {598.345, 106.693}];
testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-S", {598.451, 106.578}];
testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-ES", {598.859, 106.844}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MC", {601.517, 108.012}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MEC", {601.829, 108.324}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-C", {601.556, 108.012}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-EC", {600.318, 108.185}];


testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-U", {599.450, 107.15}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MS", {597.851, 106.643}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MES", {598.310, 106.798}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-S", {598.436, 106.672}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-ES", {598.762, 106.835}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MC", {601.653, 107.288}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MEC", {601.522, 107.178}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-C", {600.655, 107.426}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-EC", {600.263, 107.259}];


testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-U", {600.000, 106.843}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MS", {597.851, 106.643}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MES", {598.310, 106.798}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-S", {598.436, 106.672}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-ES", {598.762, 106.835}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MC", {601.397, 106.145}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MEC", {601.397, 106.145}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-C", {600.000, 106.843}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-EC", {600.000, 106.843}];


(* optimizeGtm, by "originalTuningName" *)
(* TODO: you should probably instead just test that these map to the correct traits, or something *)

testClose[optimizeGtm, pajara, "originalTuningName" -> "minimax", {600.000, 106.843}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "least squares", {599.450, 107.15}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "TOP", {598.451, 106.578}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "TIPTOP", {598.451, 106.578}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "TE", {598.859, 106.844}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "Frobenius", {598.345, 106.693}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "POTE", {600.000, 107.048}]; (* TODO: this is weird example for PO b/c not pure octave, I mean it's good to have this, but not as the first and only example *)
testClose[optimizeGtm, pajara, "originalTuningName" -> "POTOP", {600.000, 106.854}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "POTT", {600.000, 106.854}];


(* interval basis *)
(* TODO: rename options to traits *)
(* TODO: decide whether I really want to use camelCase for the user provided options, or sentence case maybe would be better*)

t = {{{1, 1, 5}, {0, -1, -3}}, "co", {2, 7 / 5, 11}};
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "formalPrimes", {1200.4181, 617.7581}];
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "primes", {1200.0558, 616.4318}];

t = {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}};
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "formalPrimes", {1201.3969, 3796.8919, 5270.7809}];
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "primes", {1201.3969, 3796.8919, 5267.2719}];


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

(* getPtm *)
test[getPtm, {{{12, 19, 28}}, "co", {2, 3, 5}}, {Log[2, 2], Log[2, 3], Log[2, 5]}];
test[getPtm, {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}}, {Log[2, 2], Log[2, 9], Log[2, 5], Log[2, 21]}];

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
test[getComplexity, {1, 1, -1}, {{{1, 2, 3}, {0, 5, 6}}, "co"}, "unstandardized", 1, 3];
test[getComplexity, {1, 1, -1}, {{{1, 2, 3}, {0, 5, 6}}, "co"}, "unstandardized", 2, \[Sqrt]3];
test[getComplexity, {1, 1, -1}, {{{1, 2, 3}, {0, 5, 6}}, "co"}, "standardized", 1, 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];

(* getDamage *)
test[getDamage, meantone, {1201.7, 697.564}, "originalTuningName" -> "TOP", 0.00141545];
test[getDamage, meantone, {1199.02, 695.601}, "originalTuningName" -> "least squares", 0.000072999];
test[getDamage, meantone, {1200., 696.578}, "originalTuningName" -> "minimax", 0.017923];
(* TODO: I'm not sure this handles pure-octave stretch and interval basis properly *)



Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
