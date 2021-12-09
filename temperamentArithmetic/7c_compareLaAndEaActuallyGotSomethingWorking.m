randomVectors[d_, r_] := RandomInteger[{-9, 9}, {r, d}];

matrixToMultivectorWithPossibleError[a_] := If[a === Error, Error, matrixToMultivector[a]];

(*TODO: still rename difference to diff here and everywhere *)
match[sumByMultivectors_, sumByMatrices_, differenceByMultivectors_, differenceByMatrices_] := Module[{sumsMatch, differencesMatch},
  sumsMatch = sumByMultivectors == sumByMatrices ;
  differencesMatch =If[
    differenceByMatrices === Error,
    allZeros[eaGetMinors[differenceByMultivectors]],
    differenceByMultivectors == differenceByMatrices
  ];

  sumsMatch && differencesMatch
];

f = 0;
p = 0;

testTemperamentArithmetic[d_, r_, nonCollinearity_] := Module[
  {
    collinearity,
    sharedVectors,
    t1,
    t2,
    w1,
    w2,
    sumByMatrices,
    sumByMultivectors,
    differenceByMultivectors,
    differenceByMatrices
  },

  Do[
    collinearity = r - nonCollinearity;

    sharedVectors = randomVectors[d, collinearity];
    t1 = {Join[sharedVectors, randomVectors[d, nonCollinearity]], "co"};
    t2 = {Join[sharedVectors, randomVectors[d, nonCollinearity]], "co"};

    t1 = If[RandomInteger[] == 1, dual[t1], t1];
    t2 = If[RandomInteger[] == 1, dual[t2], t2];

    w1 = matrixToMultivector[t1];
    w2 = matrixToMultivector[t2];

    Print[ w1, " + ", w2, " = (OR ", t1, " + ", t2, " = )"];

    sumByMultivectors = eaTemperamentSum[w1, w2];
    sumByMatrices = matrixToMultivectorWithPossibleError[temperamentSum[t1, t2]];

    differenceByMultivectors = eaTemperamentDiff[w1, w2];
    differenceByMatrices = matrixToMultivectorWithPossibleError[temperamentDiff[t1, t2]];

    If[
      match[sumByMultivectors, sumByMatrices, differenceByMultivectors, differenceByMatrices],
      (*Print["match!"];*)
      p += 1,
      f += 1;
      (*Print[ w1, " + ", w2, " = (OR ", t1, " + ", t2, " = )"];*)
      Print[sumByMultivectors, " (by multivectors)"];
      Print[sumByMatrices, " (by matrices)"];
      Print[w1, " - ", w2, " = (OR ", t1, " - ", t2, " = )"];
      Print[differenceByMultivectors, " (by multivectors)"];
      Print[differenceByMatrices, " (by matrices)\n"];
    ],
    15
  ]
];

(* TODO: actually I think it would be better to test this by taking min-grade and sometimes flipping variance, so there wouldn't be e.g. [4,3,1] here because that'd already be covered by [4,1,1] *)


(*testTemperamentArithmetic[2, 1, 1];

testTemperamentArithmetic[3, 1, 1];
testTemperamentArithmetic[3, 2, 1];

testTemperamentArithmetic[4, 1, 1];
testTemperamentArithmetic[4, 2, 1]; *)
testTemperamentArithmetic[4, 3, 1];
(*testTemperamentArithmetic[4, 2, 2]; *)

(* testTemperamentArithmetic[5, 1, 1]; *)
(* testTemperamentArithmetic[5, 2, 1]; *)
(* testTemperamentArithmetic[5, 3, 1]; *)
(* testTemperamentArithmetic[5, 4, 1]; *)
(* testTemperamentArithmetic[5, 2, 2]; *)
(* testTemperamentArithmetic[5, 3, 2]; *)

(* testTemperamentArithmetic[6, 1, 1]; *)
(* testTemperamentArithmetic[6, 2, 1]; *)
(* testTemperamentArithmetic[6, 3, 1]; *)
(* testTemperamentArithmetic[6, 4, 1]; *)
(* testTemperamentArithmetic[6, 5, 1]; *)
(* testTemperamentArithmetic[6, 2, 2]; *)
(* testTemperamentArithmetic[6, 3, 2]; *)
(* testTemperamentArithmetic[6, 4, 2]; *)
(* testTemperamentArithmetic[6, 3, 3]; *)



Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];
