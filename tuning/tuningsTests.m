failures = 0;
passes = 0;

testClose[fn_, args___, expectation_] := Module[{actual},
  actual = SetPrecision[Apply[fn, {args}], 5];
  
  If[
    actual === expectation,
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] != ``; actual result was: ``", fn, {args}, expectation, actual], 14, Red]]
  ]
];


(* GENERATORS PREIMAGE TRANSVERSAL *)


(* getGpt *)
test[getGpt, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{{1, 0, 0}, {-1, 1, 0}}, "contra"}];
test[getGpt, {{{4, -4, 1}}, "contra"}, {{{1, 0, 0}, {0, 1, 0}}, "contra"}];


(* TUNING *)

(* optimizeGtm, by individual tuning properties *)

t = {{{1, 1, 0}, {0, 1, 4}}, "co"}; (* meantone *)

testClose[optimizeGtm, t, {1200., 696.578}];

testClose[optimizeGtm, t, "weighted" -> True, "complexityWeighting" -> "F", {1202.39, 697.176}];
testClose[optimizeGtm, t, "weighted" -> True, "complexityWeighting" -> "F", "complexityPower" -> 2, {1202.728, 697.26}];
testClose[optimizeGtm, t, "weighted" -> True, {1201.7, 697.564}];
testClose[optimizeGtm, t, "weighted" -> True, "complexityPower" -> 2, {1201.6, 697.53}];

testClose[optimizeGtm, t, "weighted" -> True, "complexityWeighting" -> "F", "tim" -> {}, {1202.39, 697.176}];
testClose[optimizeGtm, t, "weighted" -> True, "complexityWeighting" -> "F", "complexityPower" -> 2, "tim" -> {}, {1202.61, 696.741}];
testClose[optimizeGtm, t, "weighted" -> True, "tim" -> {}, {1201.7, 697.564}];
testClose[optimizeGtm, t, "weighted" -> True, "complexityPower" -> 2, "tim" -> {}, {1201.4, 697.049}];

testClose[optimizeGtm, t, "weighted" -> True, "weightingDirection" -> "progressive", "complexityWeighting" -> "F", {1197.61, 694.786}];
testClose[optimizeGtm, t, "weighted" -> True, "weightingDirection" -> "progressive", "complexityWeighting" -> "F", "complexityPower" -> 2, {1197.43, 694.976}];
testClose[optimizeGtm, t, "weighted" -> True, "weightingDirection" -> "progressive", {1197.98, 694.711}];
testClose[optimizeGtm, t, "weighted" -> True, "weightingDirection" -> "progressive", "complexityPower" -> 2, {1198.16, 695.01}];


testClose[optimizeGtm, t, "meanPower" -> 2, {1199.02, 695.601}];

testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, "complexityWeighting" -> "F", {1200.07, 696.005}];
testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, "complexityWeighting" -> "F", "complexityPower" -> 2, {1200.74, 696.205}];
testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, {1200.98, 696.904}];
testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, "complexityPower" -> 2, {1201.13, 696.905}];

testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, "weightingDirection" -> "progressive", "complexityWeighting" -> "F", {1198.4, 695.289}];
testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, "weightingDirection" -> "progressive", "complexityWeighting" -> "F", "complexityPower" -> 2, {1198.24, 695.294}];
testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, "weightingDirection" -> "progressive", {1198.08, 694.93}];
testClose[optimizeGtm, t, "meanPower" -> 2, "weighted" -> True, "weightingDirection" -> "progressive", "complexityPower" -> 2, {1197.93, 694.911}];


testClose[optimizeGtm, t, "meanPower" -> 1, {1200., 696.578}];

testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, "complexityWeighting" -> "F", {1195.7, 693.352}];
testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, "complexityWeighting" -> "F", "complexityPower" -> 2, {1195.7, 693.352}];
testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, {1200., 696.578}];
testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, "complexityPower" -> 2, {1200., 696.578}];

testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, "weightingDirection" -> "progressive", "complexityWeighting" -> "F", {1200., 696.578}];
testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, "weightingDirection" -> "progressive", "complexityWeighting" -> "F", "complexityPower" -> 2, {1200., 696.578}];
testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, "weightingDirection" -> "progressive", {1195.7, 693.352}];
testClose[optimizeGtm, t, "meanPower" -> 1, "weighted" -> True, "weightingDirection" -> "progressive", "complexityPower" -> 2, {1195.7, 693.352}];


(* optimizeGtm, by "mean" and "damage" *)

t = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"}; (* pajara *)

testClose[optimizeGtm, t, "mean" -> "MAV", {600., 106.843}]; (*it seems, based on my https://docs.google.com/spreadsheets/d/1D6mE4MvbsbLqTDTYyagmlKZ_TenrqoN84VLDzsLfJnQ/edit#gid=1685147662, that 108.01 is actually more correct *)

testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "F1", {596.502, 106.767}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "F2", {598.233, 104.469}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "P1", {598.447, 107.09}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "P2", {599.682, 107.988}];

testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "F1", "tim" -> {}, {597.119, 103.293}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "F2", "tim" -> {}, {598.345, 106.693}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "P1", "tim" -> {}, {598.447, 106.567}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "P2", "tim" -> {}, {598.859, 106.844}];

testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "pF1", {601.897, 108.014}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "pF2", {601.99, 108.325}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "pP1", {601.553, 107.922}];
testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "pP2", {600.318, 108.159}];


testClose[optimizeGtm, t, "mean" -> "RMS", {599.45, 107.15}];

testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "F1", {597.851, 106.643}];
testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "F2", {598.31, 106.798}];
testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "P1", {598.436, 106.672}];
testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "P2", {598.762, 106.835}];

testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pF1", {601.653, 107.288}];
testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pF2", {601.522, 107.178}];
testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pP1", {600.655, 107.426}];
testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pP2", {600.263, 107.259}];


testClose[optimizeGtm, t, "mean" -> "AAV", {600., 106.843}];

testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "F1", {596.741, 105.214}];
testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "F2", {596.741, 105.214}];
testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "P1", {596.741, 105.214}];
testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "P2", {596.741, 105.214}];

testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pF1", {601.397, 106.145}];
testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pF2", {601.397, 106.145}];
testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pP1", {600., 106.843}];
testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pP2", {600., 106.843}];


(* optimizeGtm, by "tuning" *)

testClose[optimizeGtm, t, "tuning" -> "minimax", {600., 106.843}];

(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "F1", {596.502, 106.767}];*)
(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "F2", {598.233, 104.469}];*)
(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "P1", {598.447, 107.09}];*)
(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "P2", {599.682, 107.988}];*)

(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "F1", "tim" -> {}, {597.119, 103.293}];*)
testClose[optimizeGtm, t, "tuning" -> "Euclidean", {598.345, 106.693}];
testClose[optimizeGtm, t, "tuning" -> "Tenney", {598.447, 106.567}];
testClose[optimizeGtm, t, "tuning" -> "Breed", {598.859, 106.844}];

(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "pF1", {601.897, 108.014}];*)
(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "pF2", {601.99, 108.325}];*)
testClose[optimizeGtm, t, "tuning" -> "Partch", {601.553, 107.922}];
(*testClose[optimizeGtm, t, "mean" -> "MAV", "damage" -> "pP2", {600.318, 108.159}];*)


testClose[optimizeGtm, t, "tuning" -> "least squares", {599.45, 107.15}];

(*testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "F1", {597.851, 106.643}];*)
(*testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "F2", {598.31, 106.798}];*)
testClose[optimizeGtm, t, "tuning" -> "Tenney least squares", {598.436, 106.672}];
(*testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "P2", {598.762, 106.835}];*)

(*testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pF1", {601.653, 107.288}];*)
(*testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pF2", {601.522, 107.178}];*)
(*testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pP1", {600.655, 107.426}];*)
(*testClose[optimizeGtm, t, "mean" -> "RMS", "damage" -> "pP2", {600.263, 107.259}];*)


testClose[optimizeGtm, t, "tuning" -> "least absolutes", {600., 106.843}];

(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "F1", {596.741, 105.214}];*)
(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "F2", {596.741, 105.214}];*)
(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "P1", {596.741, 105.214}];*)
(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "P2", {596.741, 105.214}];*)

(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pF1", {601.397, 106.145}];*)
(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pF2", {601.397, 106.145}];*)
(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pP1", {600., 106.843}];*)
(*testClose[optimizeGtm, t, "mean" -> "AAV", "damage" -> "pP2", {600., 106.843}];*)




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
test[getComplexity, {1, 1, -1}, {{{1, 2, 3}, {0, 5, 6}}, "co"}, "F", 1, 3];
test[getComplexity, {1, 1, -1}, {{{1, 2, 3}, {0, 5, 6}}, "co"}, "F", 2, \[Sqrt]3];
test[getComplexity, {1, 1, -1}, {{{1, 2, 3}, {0, 5, 6}}, "co"}, "P", 1, 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];

(* getDamage *)
testClose[getDamage, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {1200., 696.578}, "tuning" -> "least absolutes", 0.0179233];
testClose[getDamage, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {1201.7, 697.564}, "tuning" -> "Tenney", 0.00141545];
testClose[getDamage, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {1199.02, 695.601}, "tuning" -> "least squares", 0.000072999];
testClose[getDamage, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {1200., 696.578}, "tuning" -> "minimax", 0.0044808];




Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
