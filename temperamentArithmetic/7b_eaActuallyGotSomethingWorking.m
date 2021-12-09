eaSum[w1_, w2_] := eaArithmetic[w1, w2, True];
eaDiff[w1_, w2_] := eaArithmetic[w1, w2, False];

eaArithmetic[w1input_, w2input_, isSum_] := Module[{w1, w2},
  w1 = eaCanonicalForm[w1input];
  w2 = If[eaGetV[w2input] != eaGetV[w1], eaDual[w2input], eaCanonicalForm[w2input]];

  If[
    eaGetR[w1] != eaGetR[w2] || eaGetD[w1] != eaGetD[w2],
    Error,
    If[
      isSum,
      eaCanonicalForm[{eaGetMinors[w1] + eaGetMinors[w2], eaGetGrade[w1], eaGetV[w1]}],
      eaCanonicalForm[{eaGetMinors[w1] - eaGetMinors[w2], eaGetGrade[w1], eaGetV[w1]}]
    ]
  ]
];




(* all examples tested from both sides of duality *)

f = 0;
p = 0;

(* monononcollinear multimaps *)
meantoneMultimap = {{1, 4, 4}, 2, "co"};
porcupineMultimap = {{3, 5, 1}, 2, "co"};
test2args[eaSum, meantoneMultimap, porcupineMultimap, {{4, 9, 5}, 2, "co"}];
test2args[eaDiff, meantoneMultimap, porcupineMultimap, {{2, 1, -3}, 2, "co"}];
meantoneMulticomma = {{4, -4, 1}, 1, "contra"};
porcupineMulticomma = {{1, -5, 3}, 1, "contra"};
test2args[eaSum, meantoneMulticomma, porcupineMulticomma, {{5, -9, 4}, 1, "contra"}];
test2args[eaDiff, meantoneMulticomma, porcupineMulticomma, {{-3, -1, 2}, 1, "contra"}];

(* monononcollinear multicommas *)
et7Multimap = {{7, 11, 16}, 1, "co"};
et5Multimap = {{5, 8, 12}, 1, "co"};
test2args[eaSum, et7Multimap, et5Multimap, {{12, 19, 28}, 1, "co"}];
test2args[eaDiff, et7Multimap, et5Multimap, {{2, 3, 4}, 1, "co"}];
et7Multicomma = {{16, -11, 7}, 2, "contra"};
et5Multicomma = {{12, -8, 5}, 2, "contra"};
test2args[eaSum, et7Multicomma, et5Multicomma, {{28, -19, 12}, 2, "contra"}];
test2args[eaDiff, et7Multicomma, et5Multicomma, {{4, -3, 2}, 2, "contra"}];

(* not monononcollinear - error! *)
septimalMeantoneMultimap = {{1, 4, 10, 4, 13, 12}, 2, "co"};
septimalBlackwoodMultimap = {{0, 5, 0, 8, 0, -14}, 2, "co"};
test2args[eaSum, septimalMeantoneMultimap, septimalBlackwoodMultimap, Error];
test2args[eaDiff, septimalMeantoneMultimap, septimalBlackwoodMultimap, Error];
septimalMeantoneMulticomma = eaDual[{{1, 4, 10, 4, 13, 12}, 2, "co"}];
septimalBlackwoodMulticomma = eaDual[{{0, 5, 0, 8, 0, -14}, 2, "co"}];
test2args[eaSum, septimalMeantoneMulticomma, septimalBlackwoodMulticomma, Error];
test2args[eaDiff, septimalMeantoneMulticomma, septimalBlackwoodMulticomma, Error];

(* monononcollinear - doubly collinear (multicommas) *)
et12Multimap = {{12, 19, 28, 34}, 1, "co"};
et19Multimap = {{19, 30, 44, 53}, 1, "co"};
test2args[eaSum, et12Multimap, et19Multimap, {{31, 49, 72, 87}, 1, "co"}];
test2args[eaDiff, et12Multimap, et19Multimap, {{7, 11, 16, 19}, 1, "co"}];
et12Multicomma = eaDual[et12Multimap];
et19Multicomma = eaDual[et19Multimap];
test2args[eaSum, et12Multicomma, et19Multicomma, {{-87, 72, -49, 31}, 3, "contra"}];
test2args[eaDiff, et12Multicomma, et19Multicomma, {{-19, 16, -11, 7}, 3, "contra"}];

(* examples with themselves *)
test2args[eaSum, meantoneMultimap, meantoneMultimap, {{1, 4, 4}, 2, "co"}];
test2args[eaDiff, meantoneMultimap, meantoneMultimap, {{0, 0, 0}, 2, "co"}];
test2args[eaSum, meantoneMulticomma, meantoneMulticomma, {{4, -4, 1}, 1, "contra"}];
test2args[eaDiff, meantoneMulticomma, meantoneMulticomma, {{0, 0, 0}, 1, "contra"}];
test2args[eaSum, et7Multimap, et7Multimap, {{7, 11, 16}, 1, "co"}];
test2args[eaDiff, et7Multimap, et7Multimap, {{0, 0, 0}, 1, "co"}];
test2args[eaSum, et7Multicomma, et7Multicomma, {{16, -11, 7}, 2, "contra"}];
test2args[eaDiff, et7Multicomma, et7Multicomma, {{0, 0, 0}, 2, "contra"}];

(* mismatched r & n but matching d *)
test2args[eaSum, et7Multimap, meantoneMultimap, Error];
test2args[eaDiff, et7Multimap, meantoneMultimap, Error];
test2args[eaSum, et7Multicomma, meantoneMulticomma, Error];
test2args[eaDiff, et7Multicomma, meantoneMulticomma, Error];

(* mismatched d but matching r or n *)
test2args[eaSum, et7Multimap, et12Multimap, Error];
test2args[eaDiff, et7Multimap, et12Multimap, Error];
test2args[eaSum, et7Multicomma, et12Multicomma, Error];
test2args[eaDiff, et7Multicomma, et12Multicomma, Error];

(* some basic examples *)
augmentedMultimap = {{3 , 0, -7}, 2, "co"};
diminishedMultimap = {{4, 4, -3}, 2, "co"};
tetracotMultimap = {{4, 9, 5}, 2, "co"};
dicotMultimap = {{2, 1, -3}, 2, "co"};
srutalMultimap = {{2, -4, -11}, 2, "co"};
test2args[eaSum, augmentedMultimap, diminishedMultimap, {{7, 4, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]] *)
test2args[eaDiff, augmentedMultimap, diminishedMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]] *)
test2args[eaSum, augmentedMultimap, tetracotMultimap, {{7, 9, -2}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]] *)
test2args[eaDiff, augmentedMultimap, tetracotMultimap, {{1, 9, 12}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]] *)
test2args[eaSum, augmentedMultimap, dicotMultimap, {{5, 1, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]] *)
test2args[eaDiff, augmentedMultimap, dicotMultimap, {{1, -1, -4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]] *)
test2args[eaSum, augmentedMultimap, srutalMultimap, {{5, -4, -18}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]] *)
test2args[eaDiff, augmentedMultimap, srutalMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]] *)
test2args[eaSum, diminishedMultimap, tetracotMultimap, {{8, 13, 2}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]] *)
test2args[eaDiff, diminishedMultimap, tetracotMultimap, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]] *)
test2args[eaSum, diminishedMultimap, dicotMultimap, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]] *)
test2args[eaDiff, diminishedMultimap, dicotMultimap, {{2, 3, 0}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]] *)
test2args[eaSum, diminishedMultimap, srutalMultimap, {{3, 0, -7}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test2args[eaDiff, diminishedMultimap, srutalMultimap, {{1, 4, 4}, 2, "co"}]; (*⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[eaSum, tetracotMultimap, dicotMultimap, {{3, 5, 1}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test2args[eaDiff, tetracotMultimap, dicotMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[eaSum, tetracotMultimap, srutalMultimap, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[eaDiff, tetracotMultimap, srutalMultimap, {{2, 13, 16}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[eaSum, dicotMultimap, srutalMultimap, {{4, -3, -14}, 2, "co"}]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[eaDiff, dicotMultimap, srutalMultimap, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of collinear, but not monononcollinear: d = 5, min-grade = 2, noncollinearity = 2 *)
w1 = {{0, 0, 0, 41, -27, 2, 41, -27, 2, 31}, 3, "co"};
w2 = {{48, 140, 46, 20, 10, 10, -250, -53, 85, 30}, 3, "co"};
test2args[eaSum, w1, w2, Error];
test2args[eaDiff, w1, w2, Error];

(* example of monononcollinear, but not collinear: d = 2, min-grade = 1, noncollinearity = 1 *)
w1 = {{2, 3}, 1, "contra"};
w2 = {{4, -7}, 1, "co"};
wSum = {{9, 7}, 1, "contra"};
wDiff = {{5, 1}, 1, "contra"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];

(* example demonstrating how it's important to canonicalize *)
w1 = {{-2, 4, -2}, 1, "co"};
w2 = {{7, 7, 0}, 1, "co"};
wSum = {{2, -1, 1}, 1, "co"};
wDiff = {{0, 3, -1}, 1, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
w1 = {{1, 4, 10, 4, 13, 12}, 2, "co"};
w2 = {{1 , 4, -9, 4, -17, -32}, 2, "co"};
wSum = {{2, 8, 1, 8, -4, -20}, 2, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaSum, eaDual[w1], w2, eaDual[wSum]];
test2args[eaSum, w1, eaDual[w2], wSum];
test2args[eaSum, eaDual[w1], eaDual[w2], eaDual[wSum]];

(* an example that used to fail for whatever reason, "some problem" *)
test2args[eaSum,{{18,-2,-1,14,-20,3},2,"co"} , {{6,-2,8,6,-15,-3},2,"co"}, {{24,-4,7,20,-35,0},2,"co"}];

(* another example that used to fail for whatever reason, "goddam failing mysteries" *)
test2args[eaSum, {{15,93,30,22,10,18},2,"co"}, {{32,44,-1,-56,-22,-32},2,"co"}, {{47,137,29,-34,-12,-14},2,"co"}];

(* another example that used to fail for whatever reason, "more stuff to sort out" *)
test2args[eaSum, {{5,16,15,-1,0,3},2,"contra"}, {{4,3,12,-1,0,3},2,"contra"}, {{9,19,27,-2,0,6},2,"contra"}];

(* EA only: example that motivated a further simplification and correction of the EA collinearity condition *)
test2args[eaSum, {{1, -5, -14, 9, 23, 11}, 2, "co"}, {{25, -1, 2, -18, -14, 2}, 2, "contra"}, Error];

(* LA only checks example that required the breadth-first search of linear combinations of multiple collinear vectors, but I think it's okay to check it here too *)
test2args[eaSum, {{3, 8, -4, -6}, 1, "co"}, {{9, 2, -4, 1}, 1 , "co"}, {{12, 10, -8, -5}, 1, "co"}];

(* LA only checks this non-min-grade-1 example, but I think it's okay to check it here too *)
septimalMeantoneW = {{1, 4, 10, 4, 13, 12}, 2, "co"};
flattoneW = {{1 , 4, -9, 4, -17, -32}, 2, "co"};
godzillaW = {{2, 8, 1, 8, -4, -20}, 2, "co"};
et19MwithIndependent7W = {{0, 0, 19, 0, 30, 44}, 2, "co"};
test2args[eaSum, septimalMeantoneW, flattoneW, godzillaW];
test2args[eaDiff, septimalMeantoneW, flattoneW, et19MwithIndependent7W];

(* LA only ensures the minors are consulted so that the sum and diff are identified correctly, but I think it's okay to check it here too *)
(* this also verifies that for the min-grade-1 case, I think *)
w1 = {{0, 1, -1, 0}, 3, "co"};
w2 = {{20, -144, 87, -59}, 3, "co"};
wSum = {{20, -143, 86, -59}, 3, "co"};
wDiff = {{20, -145, 88, -59}, 3, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];

(* LA only ensures intractability beyond the breadth-first search of linear combinations code the first way I wrote it, i.e. using my fancier style essentially using a Wolfram Solve[]... but let's check it here too *)
w1 = {{35, 5, 40, 10, 27, -71, 19, -41, -5, 42}, 2, "co"};
w2 = {{5, -40, 30, -60, 12, -15, 15, 48, 24, -90}, 2, "co"};
wSum = {{40, -35, 70, -50, 39, -86, 34, 7, 19, -48}, 2, "co"};
wDiff = {{30, 45, 10, 70, 15, -56, 4, -89, -29, 132}, 2, "co"};
test2args[eaSum, w1, w2, wSum];
test2args[eaDiff, w1, w2, wDiff];



Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];
