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

randomVectors[d_, r_] := RandomInteger[{-9, 9}, {r, d}];

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
  d = RandomInteger[{4, 4}];
  r = RandomInteger[{2, 2}];
  n = d - r;
  minGrade = Min[r, n];
  nonCollinearity = RandomInteger[{1, minGrade}];
  collinearityR = r - nonCollinearity;

  sharedVectors = randomVectors[d, collinearityR];
  t1 = canonicalForm[{Join[sharedVectors, randomVectors[d, nonCollinearity]], "co"}]; (*TODO: canonical forming for now just to avoid the bug I mentioned *)
  t2 = canonicalForm[{Join[sharedVectors, randomVectors[d, nonCollinearity]], "co"}];
  (*
    tAndW1 = randomMatrixAndMultivector2[d, r];
    t1 = First[tAndW1];
    w1 = Last[tAndW1];

    tAndW2 = randomMatrixAndMultivector2[d, r];
    t2 = First[tAndW2];
    w2 = Last[tAndW2];
  *)

  w1 = matrixToMultivector[t1];
  w2 = matrixToMultivector[t2];

  Print["\n", w1, " + ", w2, " = (OR ", t1, " + ", t2, " = )"];
  (* Print[t1];
   Print[t2];*)
  sumByMultivectors = eaTemperamentSum[w1, w2];
  sumByMatrices = matrixToMultivector2[temperamentSum[t1, t2]];

  differenceByMultivectors = eaTemperamentDifference[w1, w2];
  differenceByMatrices = matrixToMultivector2[temperamentDifference[t1, t2]];
  (*Print["what", sumByMultivectors, sumByMatrices];*)

  If[
    (* Sort[{sumByMultivectors, differenceByMultivectors}] == Sort[{sumByMatrices, differenceByMatrices}],*)
    sumByMultivectors == sumByMatrices && differenceByMultivectors == differenceByMatrices,
    Print["match!"],
    f += 1;
    (*Print["BAD BAD BAD!!!!!!!!!!!!!!!!!"];*)
    Print[sumByMultivectors, " (by multivectors)"];
    Print[sumByMatrices, " (by matrices)"];
    Print[w1, " - ", w2, " = (OR ", t1, " - ", t2, " = )"];
    Print[differenceByMultivectors, " (by multivectors)"];
    Print[differenceByMatrices, " (by matrices)\n"];
  ],
  50
];

(*TODO: eventually you should run a hundred, say, for each of the forms you document in that google sheet *)

Print["TOTAL FAILURES: ", f];
