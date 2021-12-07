eaTemperamentSum[w1_, w2_] := eaTemperamentArithmetic[w1, w2, True];
eaTemperamentDifference[w1_, w2_] := eaTemperamentArithmetic[w1, w2, False];

eaTemperamentArithmetic[w1_, w2input_, isSum_] := Module[{w2},
  w2 = If[getV[w2input] != getV[w1], eaDual[w2input], w2input];

  If[
    eaGetR[w1] != eaGetR[w2] || eaGetD[w1] != eaGetD[w2],
    Error,
    If[
      isSum,
      eaCanonicalForm[{getMinors[w1] + getMinors[w2], getGrade[w1], getV[w1]}],
      eaCanonicalForm[{getMinors[w1] - getMinors[w2], getGrade[w1], getV[w1]}]
    ]
  ]
];




(* all examples tested from both sides of duality *)

f = 0;

(* now just a stray example I feel like keeping around*)
septimalMeantone = {{1, 4, 10, 4, 13, 12}, 2, "co"};
flattone = {{1 , 4, -9, 4, -17, -32}, 2, "co"};
godzilla = {{2, 8, 1, 8, -4, -20}, 2, "co"};
test2args[eaTemperamentSum, septimalMeantone, flattone, godzilla];

(* collinear multimaps *)
meantoneMultimap = {{1, 4, 4}, 2, "co"};
porcupineMultimap = {{3, 5, 1}, 2, "co"};
test2args[eaTemperamentSum, meantoneMultimap, porcupineMultimap, {{4, 9, 5}, 2, "co"}];
test2args[eaTemperamentDifference, meantoneMultimap, porcupineMultimap, {{2, 1, -3}, 2, "co"}];
meantoneMulticomma = {{4, -4, 1}, 1, "contra"};
porcupineMulticomma = {{1, -5, 3}, 1, "contra"};
test2args[eaTemperamentSum, meantoneMulticomma, porcupineMulticomma, {{5, -9, 4}, 1, "contra"}];
test2args[eaTemperamentDifference, meantoneMulticomma, porcupineMulticomma, {{-3, -1, 2}, 1, "contra"}];

(* collinear multicommas *)
et7Multimap = {{7, 11, 16}, 1, "co"};
et5Multimap = {{5, 8, 12}, 1, "co"};
test2args[eaTemperamentSum, et7Multimap, et5Multimap, {{12, 19, 28}, 1, "co"}];
test2args[eaTemperamentDifference, et7Multimap, et5Multimap, {{2, 3, 4}, 1, "co"}];
et7Multicomma = {{16, -11, 7}, 2, "contra"};
et5Multicomma = {{12, -8, 5}, 2, "contra"};
test2args[eaTemperamentSum, et7Multicomma, et5Multicomma, {{28, -19, 12}, 2, "contra"}];
test2args[eaTemperamentDifference, et7Multicomma, et5Multicomma, {{4, -3, 2}, 2, "contra"}];

(* noncollinear - error! *)
septimalMeantoneMultimap = {{1, 4, 10, 4, 13, 12}, 2, "co"};
septimalBlackwoodMultimap = {{0, 5, 0, 8, 0, -14}, 2, "co"};
test2args[eaTemperamentSum, septimalMeantoneMultimap, septimalBlackwoodMultimap, Error];
test2args[eaTemperamentDifference, septimalMeantoneMultimap, septimalBlackwoodMultimap, Error];
septimalMeantoneMulticomma = eaDual[{{1, 4, 10, 4, 13, 12}, 2, "co"}];
septimalBlackwoodMulticomma = eaDual[{{0, 5, 0, 8, 0, -14}, 2, "co"}];
test2args[eaTemperamentSum, septimalMeantoneMulticomma, septimalBlackwoodMulticomma, Error];
test2args[eaTemperamentDifference, septimalMeantoneMulticomma, septimalBlackwoodMulticomma, Error];

(* doubly collinear (multicommas) *)
et12Multimap = {{12, 19, 28, 34}, 1, "co"};
et19Multimap = {{19, 30, 44, 53}, 1, "co"};
test2args[eaTemperamentSum, et12Multimap, et19Multimap, {{31, 49, 72, 87}, 1, "co"}];
test2args[eaTemperamentDifference, et12Multimap, et19Multimap, {{7, 11, 16, 19}, 1, "co"}];
et12Multicomma = eaDual[et12Multimap];
et19Multicomma = eaDual[et19Multimap];
test2args[eaTemperamentSum, et12Multicomma, et19Multicomma, {{-87, 72, -49, 31}, 3, "contra"}];
test2args[eaTemperamentDifference, et12Multicomma, et19Multicomma, {{-19, 16, -11, 7}, 3, "contra"}];

(* examples with themselves *)
test2args[eaTemperamentSum, meantoneMultimap, meantoneMultimap, {{1, 4, 4}, 2, "co"}];
test2args[eaTemperamentDifference, meantoneMultimap, meantoneMultimap, {{0, 0, 0}, 2, "co"}];
test2args[eaTemperamentSum, meantoneMulticomma, meantoneMulticomma, {{4, -4, 1}, 1, "contra"}];
test2args[eaTemperamentDifference, meantoneMulticomma, meantoneMulticomma, {{0, 0, 0}, 1, "contra"}];
test2args[eaTemperamentSum, et7Multimap, et7Multimap, {{7, 11, 16}, 1, "co"}];
test2args[eaTemperamentDifference, et7Multimap, et7Multimap, {{0, 0, 0}, 1, "co"}];
test2args[eaTemperamentSum, et7Multicomma, et7Multicomma, {{16, -11, 7}, 2, "contra"}];
test2args[eaTemperamentDifference, et7Multicomma, et7Multicomma, {{0, 0, 0}, 2, "contra"}];

(* some basic examples *)
augmentedMultimap = {{3 , 0, -7}, 2, "co"};
diminishedMultimap = {{4, 4, -3}, 2, "co"};
tetracotMultimap = {{4, 9, 5}, 2, "co"};
dicotMultimap = {{2, 1, -3}, 2, "co"};
srutalMultimap = {{2, -4, -11}, 2, "co"};
test2args[eaTemperamentSum, augmentedMultimap, diminishedMultimap, {{7, 4, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]] *)
test2args[eaTemperamentDifference, augmentedMultimap, diminishedMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]] *)
test2args[eaTemperamentSum, augmentedMultimap, tetracotMultimap, {{7, 9, -2}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]] *)
test2args[eaTemperamentDifference, augmentedMultimap, tetracotMultimap, {{1, 9, 12}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]] *)
test2args[eaTemperamentSum, augmentedMultimap, dicotMultimap, {{5, 1, -10}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]] *)
test2args[eaTemperamentDifference, augmentedMultimap, dicotMultimap, {{1, -1, -4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]] *)
test2args[eaTemperamentSum, augmentedMultimap, srutalMultimap, {{5, -4, -18}, 2, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]] *)
test2args[eaTemperamentDifference, augmentedMultimap, srutalMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]] *)
test2args[eaTemperamentSum, diminishedMultimap, tetracotMultimap, {{8, 13, 2}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]] *)
test2args[eaTemperamentDifference, diminishedMultimap, tetracotMultimap, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]] *)
test2args[eaTemperamentSum, diminishedMultimap, dicotMultimap, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]] *)
test2args[eaTemperamentDifference, diminishedMultimap, dicotMultimap, {{2, 3, 0}, 2, "co"}]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]] *)
test2args[eaTemperamentSum, diminishedMultimap, srutalMultimap, {{3, 0, -7}, 2, "co"}]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test2args[eaTemperamentDifference, diminishedMultimap, srutalMultimap, {{1, 4, 4}, 2, "co"}]; (*⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[eaTemperamentSum, tetracotMultimap, dicotMultimap, {{3, 5, 1}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test2args[eaTemperamentDifference, tetracotMultimap, dicotMultimap, {{1, 4, 4}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[eaTemperamentSum, tetracotMultimap, srutalMultimap, {{6, 5, -6}, 2, "co"}]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[eaTemperamentDifference, tetracotMultimap, srutalMultimap, {{2, 13, 16}, 2, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[eaTemperamentSum, dicotMultimap, srutalMultimap, {{4, -3, -14}, 2, "co"}]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[eaTemperamentDifference, dicotMultimap, srutalMultimap, {{0, 5, 8}, 2, "co"}]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* TODO: EA version of this LA example that requires the breadth-first search of linear combinations of multiple collinear vectors *)
(*test2args[temperamentSum, {{{-3, -8, 4, 6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];*)

(* example that motivated a further simplification and correction of the EA collinearity condition *)
test2args[eaTemperamentSum, {{1, -5, -14, 9, 23, 11}, 2, "co"}, {{25, -1, 2, -18, -14, 2}, 2, "contra"}, Error];

(* example that motivated the possibility of inputs of different variances ... oh wait whoa crazy! here's a case where they are non-collinear, but mono-non-collinear, so it's still valid !!!! *)
test2args[eaTemperamentSum, {{2, 3}, 1, "contra"}, {{4, -7}, 1, "co"}, {{9, 7}, 1, "contra"}];

(* an example of a d=5 min-grade=2 noncolinearity=2 example that should error even though it's collinear *)
test2args[eaTemperamentSum, {{0, 0, 0, 41, -27, 2, 41, -27, 2, 31}, 3, "co"}, {{48, 140, 46, 20, 10, 10, -250, -53, 85, 30}, 3, "co"}, Error];


Print["TOTAL FAILURES: ", f];
