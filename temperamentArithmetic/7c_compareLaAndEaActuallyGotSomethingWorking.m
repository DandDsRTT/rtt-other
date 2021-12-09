randomVectors[d_, r_] := RandomInteger[{-9, 9}, {r, d}];

matrixToMultivectorWithPossibleError[a_] := If[a === Error, Error, matrixToMultivector[a]];

match[sumByMultivectors_, sumByMatrices_, diffByMultivectors_, diffByMatrices_] := Module[{sumsMatch, diffsMatch},
  sumsMatch = sumByMultivectors === sumByMatrices;
  diffsMatch = If[
    diffByMatrices === Error,
    If[
      diffByMultivectors === Error,
      True,
      allZeros[eaGetMinors[diffByMultivectors]]
    ],
    diffByMultivectors == diffByMatrices
  ];

  sumsMatch && diffsMatch
];

f = 0;
p = 0;

testTemperamentArithmetic[d_, r_, nonCollinearity_, testCount_] := Module[
  {
    collinearity,
    sharedVectors,
    t1,
    t2,
    w1,
    w2,
    sumByMatrices,
    sumByMultivectors,
    diffByMultivectors,
    diffByMatrices
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

    sumByMultivectors = eaTemperamentSum[w1, w2];
    sumByMatrices = matrixToMultivectorWithPossibleError[temperamentSum[t1, t2]];

    diffByMultivectors = eaTemperamentDiff[w1, w2];
    diffByMatrices = matrixToMultivectorWithPossibleError[temperamentDiff[t1, t2]];

    If[
      match[sumByMultivectors, sumByMatrices, diffByMultivectors, diffByMatrices],
      p += 1,
      f += 1;
      Print["failure: "];
      Print[w1, " + ", w2, " = (OR ", t1, " + ", t2, " = )"];
      Print[sumByMultivectors, " (by multivectors)"];
      Print[sumByMatrices, " (by matrices)"];
      Print[w1, " - ", w2, " = (OR ", t1, " - ", t2, " = )"];
      Print[diffByMultivectors, " (by multivectors)"];
      Print[diffByMatrices, " (by matrices)\n"];
    ],
    testCount
  ]
];



testTemperamentArithmetic[2, 1, 1, 32];

testTemperamentArithmetic[3, 1, 1, 16];
testTemperamentArithmetic[3, 2, 1, 16];

testTemperamentArithmetic[4, 1, 1, 8];
testTemperamentArithmetic[4, 2, 1, 8];
testTemperamentArithmetic[4, 3, 1, 8];
testTemperamentArithmetic[4, 2, 2, 8];

testTemperamentArithmetic[5, 1, 1, 4];
testTemperamentArithmetic[5, 2, 1, 4];
testTemperamentArithmetic[5, 3, 1, 4];
testTemperamentArithmetic[5, 4, 1, 4];
testTemperamentArithmetic[5, 2, 2, 4];
testTemperamentArithmetic[5, 3, 2, 4];

testTemperamentArithmetic[6, 1, 1, 2];
testTemperamentArithmetic[6, 2, 1, 2];
testTemperamentArithmetic[6, 3, 1, 2];
testTemperamentArithmetic[6, 4, 1, 2];
testTemperamentArithmetic[6, 5, 1, 2];
testTemperamentArithmetic[6, 2, 2, 2];
testTemperamentArithmetic[6, 3, 2, 2];
testTemperamentArithmetic[6, 4, 2, 2];
testTemperamentArithmetic[6, 3, 3, 2];



Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];
