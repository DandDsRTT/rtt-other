(*TODO: this is automated testing of thousands of random examples to make sure LA and EA agree; but I might want to design this so that it has a much higher likelihood of collinearity, to more efficiently/effectively test that, specifically actually generate the shared vector(s) first, then the unshared ones, and then assemble. you could draw from this:

randomVectors[d_, r_] :=RandomInteger[{-9,9},{r,d}];

Do[
sharedVectors = randomVectors[5,1];
t1 = {Join[sharedVectors, randomVectors[5,1]], "co"};
t2 = {Join[sharedVectors, randomVectors[5,1]], "co"};

 collinearVectors = dual[join[t1, t2]];
  collinearCovectors = dual[meet[t1, t2]];

Print[eaTemperamentSum[matrixToMultivector[t1],matrixToMultivector[t2]], " contrav. col.: ", Length[getA[collinearVectors]], " cov. col.: ", Length[getA[collinearCovectors]], " col. cov: ", getA[collinearCovectors], " col. v: ", getA[collinearVectors]] ,
100
]
*)

randomMatrixAndMultivector2[d_, r_] := Module[{v, a, t, w},
  v = If[RandomInteger[] == 1, "contra", "co"];
  a = RandomInteger[{-5, 5}, {If[v == "contra", d - r, r], d}];
  t = canonicalForm[{a, v}];
  w = matrixToMultivector[t];

  {t, w}
];

matrixToMultivector2[a_] := If[a === Error, Error, matrixToMultivector[a]];

f = 0;
Do[
  d = RandomInteger[{3, 5}];
  r = RandomInteger[{1, d - 1}];

  tAndW1 = randomMatrixAndMultivector2[d, r];
  t1 = First[tAndW1];
  w1 = Last[tAndW1];

  tAndW2 = randomMatrixAndMultivector2[d, r];
  t2 = First[tAndW2];
  w2 = Last[tAndW2];

  Print[w1, " + ", w2, " = "];
  (* Print[t1];
   Print[t2];*)
  sumByMultivectors = eaTemperamentSum[w1, w2];
  sumByMatrices = matrixToMultivector2[temperamentSum[t1, t2]];

  differenceByMultivectors = eaTemperamentDifference[w1, w2];
  differenceByMatrices = matrixToMultivector2[temperamentDifference[t1, t2]];

  If[
    Sort[{sumByMultivectors, differenceByMultivectors}] == Sort[{sumByMatrices, differenceByMatrices}],
    Print["match!"],
    f += 1;
    Print["BAD BAD BAD!!!!!!!!!!!!!!!!!"];
    Print[sumByMultivectors, " (by multivectors)"];
    Print[sumByMatrices, " (by matrices)"];
    Print[w1, " - ", w2, " = "];
    Print[differenceByMultivectors, " (by multivectors)"];
    Print[differenceByMatrices, " (by matrices)"];
  ],
  100
];

Print["TOTAL FAILURES: ", f];
