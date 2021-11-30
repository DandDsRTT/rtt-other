getCollinearity[t1_, t2_] := Module[{collinearVectors, collinearCovectors},
  collinearVectors = dual[join[t1, t2]];
  collinearCovectors = dual[meet[t1, t2]];

  If[
    allZeros[getA[collinearCovectors]],
    If[
      allZeros[getA[collinearVectors]],
      Error,
      collinearVectors
    ],
    collinearCovectors
  ]
];

temperamentArithmetic[t1_, t2_, isSum_] := If[
  getR[t1] != getR[t2] || getD[t1] != getD[t2] ,
  Error,
  Module[{collinearity},
    collinearity = getCollinearity[t1, t2];

    If[
      collinearity === Error,
      Error,
      If[
        isContra[collinearity],
        validTemperamentArithmeticByC[t1, t2, isSum, getA[collinearity]],
        validTemperamentArithmeticByM[t1, t2, isSum, getA[collinearity]]
      ]
    ]
  ]
];

temperamentSum[t1_, t2_] := If[
  canonicalForm[t1] == canonicalForm[t2],
  t1,
  temperamentArithmetic[t1, t2, True]
];

temperamentDifference[t1_, t2_] := If[
  canonicalForm[t1] == canonicalForm[t2],
  Error,
  temperamentArithmetic[t1, t2, False]
];

isEnfactored[a_] := hnf[a] != hnf[colHermiteDefactor[a]];

(* TODO: definitely hope we can clean this up so that we don't need byM and byC versions of everything; like maybe just an anti-transpose can solve the problem *)

(* here's the comma basis version of everything *)

getCollinearVectorMultiplesForMaxMultiple[n_, maxMultiple_] := Module[{collinearVectorMultiples, i, collinearVectorMultiplesForMaxMultiple},
  collinearVectorMultiples = Table[0, n];
  collinearVectorMultiples[[1]] = maxMultiple;
  i = 2;
  collinearVectorMultiplesForMaxMultiple = {};

  While[
    i <= Length[collinearVectorMultiples],
    If[
      collinearVectorMultiples[[i]] < collinearVectorMultiples[[i - 1]],

      collinearVectorMultiplesForMaxMultiple = Join[collinearVectorMultiplesForMaxMultiple, {collinearVectorMultiples}];
      collinearVectorMultiples[[i]] = collinearVectorMultiples[[i]] + 1,

      i++;
      If[
        i > Length[collinearVectorMultiples] && collinearVectorMultiples[[1]] < maxMultiple,

        i = 2;
        collinearVectorMultiplesForMaxMultiple = Join[collinearVectorMultiplesForMaxMultiple, {collinearVectorMultiples}];
        collinearVectorMultiples[[1]] = collinearVectorMultiples[[1]] + 1;
      ]
    ];
  ];
  collinearVectorMultiplesForMaxMultiple = Join[collinearVectorMultiplesForMaxMultiple, {collinearVectorMultiples}];

  collinearVectorMultiplesForMaxMultiple
];

getCollinearVectorMultiplePermutationsForMaxMultiple[n_, maxMultiple_] := Flatten[Map[
  Permutations,
  getCollinearVectorMultiplesForMaxMultiple[n, maxMultiple]
], 1];

(* TODO: there's got to be a "reduce" like thing I could do instead here *)
getCollinearVectorLinearCombination[collinearVectors_, collinearVectorMultiplePermutation_] := Module[{collinearVectorLinearCombination, i},
  collinearVectorLinearCombination = Table[0, Length[First[collinearVectors]]];
  i = 1;

  Do[
    collinearVectorLinearCombination = collinearVectorLinearCombination + collinearVectorMultiplePermutationEntry * collinearVectors[[i]];
    i++,
    {collinearVectorMultiplePermutationEntry, collinearVectorMultiplePermutation}
  ];

  collinearVectorLinearCombination
];

defactorWhileLockingCollinearVectors[t_, n_, collinearVectors_] := Module[
  {
    c,
    originalTerminalVector,
    collinearVectorMultiplePermutations,
    i,
    maxMultiple,
    collinearVectorLinearCombination,
    candidateTerminalVector
  },

  c = Take[Join[collinearVectors, getC[t]], n];
  originalTerminalVector = c[[n]];
  collinearVectorMultiplePermutations = {};
  i = 1;
  maxMultiple = 1;

  While[
    isEnfactored[c],

    If[
      i <= Length[collinearVectorMultiplePermutations],

      collinearVectorLinearCombination = getCollinearVectorLinearCombination[collinearVectors, collinearVectorMultiplePermutations[[i]]];
      candidateTerminalVector = divideOutGcd[originalTerminalVector + collinearVectorLinearCombination];
      i++;
      c[[n]] = candidateTerminalVector,

      collinearVectorMultiplePermutations = getCollinearVectorMultiplePermutationsForMaxMultiple[Length[collinearVectors], maxMultiple];
      maxMultiple++;
      i = 1;
    ];
  ];

  c
];

(* TODO: still sad that I need Minors[] to figure this out... figure out if there's any other way *)
isNegativeOrientationOfTemperamentMatrixByC[c_, n_] := trailingEntry[First[Minors[c, n]]] < 0;

validTemperamentArithmeticByC[t1_, t2_, isSum_, collinearVectors_] := Module[{v, n, c1, c2},
  v = getV[t1];
  n = getN[t1];

  c1 = defactorWhileLockingCollinearVectors[t1, n, collinearVectors];
  c2 = defactorWhileLockingCollinearVectors[t2, n, collinearVectors];

  If[
    isNegativeOrientationOfTemperamentMatrixByC[c1, n],
    c1[[n]] = -c1[[n]]
  ];

  If[
    isSum,
    If[
      isNegativeOrientationOfTemperamentMatrixByC[c2, n],
      c2[[n]] = -c2[[n]]
    ],
    If[
      !isNegativeOrientationOfTemperamentMatrixByC[c2, n],
      c2[[n]] = -c2[[n]]
    ]
  ];

  If[
    isContra[t1],
    canonicalForm[{c1 + c2, "contra"}],
    dual[{c1 + c2, "contra"}]
  ]
];

(* here's the mapping version of everything *)

(* TODO: implement getCollinearCovectorMultiplesForMaxMultiple using breadth-first linear combination of multiple collinear vector pattern established for comma bases *)

(* TODO: implement getCollinearCovectorMultiplePermutationsForMaxMultiple using breadth-first linear combination of multiple collinear vector pattern established for comma bases *)

(* TODO: implement getCollinearCovectorLinearCombination using breadth-first linear combination of multiple collinear vector pattern established for comma bases *)

(* TODO: reimplement this using breadth-first linear combination of multiple collinear vector pattern established for comma bases *)
defactorWhileLockingCollinearCovectors[t_, r_, collinearCovectors_] := Module[{m},
  m = Take[Join[collinearCovectors, getM[t]], r];

  While[
    isEnfactored[m],
    m[[r]] = divideOutGcd[m[[r]] + First[collinearCovectors]]
  ];

  m
];

isNegativeOrientationOfTemperamentMatrixByM[m_, r_] := leadingEntry[First[Minors[m, r]]] < 0;

validTemperamentArithmeticByM[t1_, t2_, isSum_, collinearCovectors_] := Module[{v, r, m1, m2},
  v = getV[t1];
  r = getR[t1];

  m1 = defactorWhileLockingCollinearCovectors[t1, r, collinearCovectors];
  m2 = defactorWhileLockingCollinearCovectors[t2, r, collinearCovectors];

  If[
    isNegativeOrientationOfTemperamentMatrixByM[m1, r],
    m1[[r]] = -m1[[r]]
  ];

  If[
    isSum,
    If[
      isNegativeOrientationOfTemperamentMatrixByM[m2, r],
      m2[[r]] = -m2[[r]]
    ],
    If[
      !isNegativeOrientationOfTemperamentMatrixByM[m2, r],
      m2[[r]] = -m2[[r]]
    ]
  ];

  If[
    isContra[t1],
    dual[{m1 + m2, "co"}],
    canonicalForm[{m1 + m2, "co"}]
  ]
];




(* most examples tested from both sides of duality *)

f = 0;

(* collinear mappings*)
meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
test2args[temperamentSum, meantoneM, porcupineM, {{{1, 1, 1}, {0, 4, 9}}, "co"}];
test2args[temperamentDifference, meantoneM, porcupineM, {{{1, 1, 2}, {0, 2, 1}}, "co"}];
meantoneC = {{{4, -4, 1}}, "contra"};
porcupineC = {{{1, -5, 3}}, "contra"};
test2args[temperamentSum, meantoneC, porcupineC, {{{5, -9, 4}}, "contra"}];
test2args[temperamentDifference, meantoneC, porcupineC, {{{-3, -1, 2}}, "contra"}];

(* collinear comma bases *)
et7M = {{{7, 11, 16}}, "co"};
et5M = {{{5, 8, 12}}, "co"};
test2args[temperamentSum, et7M, et5M, {{{12, 19, 28}}, "co"}];
test2args[temperamentDifference, et7M, et5M, {{{2, 3, 4}}, "co"}];
et7C = dual[et7M];
et5C = dual[et5M];
test2args[temperamentSum, et7C, et5C, {{{-19, 12, 0}, {-15, 8, 1}}, "contra"}];
test2args[temperamentDifference, et7C, et5C, {{{-3, 2, 0}, {-2, 0, 1}}, "contra"}];

(* noncollinear - error! *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "co"};
test2args[temperamentSum, septimalMeantoneM, septimalBlackwoodM, Error];
test2args[temperamentDifference, septimalMeantoneM, septimalBlackwoodM, Error];
septimalMeantoneC = dual[septimalMeantoneM];
septimalBlackwoodC = dual[septimalBlackwoodM];
test2args[temperamentSum, septimalMeantoneC, septimalBlackwoodC, Error];
test2args[temperamentDifference, septimalMeantoneC, septimalBlackwoodC, Error];

(* doubly collinear (comma bases) *)
et12M = {{{12, 19, 28, 34}}, "co"}; (* dual[{{{4, -4, 1, 0}, {-5, 2, 2, -1}, {-10, -1, 5, 0}}, "contra"}] *)
et19M = {{{19, 30, 44, 53}}, "co"}; (* dual[{{{4, -4, 1, 0}, {-5, 2, 2, -1}, {6, -2, 0, -1}}, "contra"}] *)
test2args[temperamentSum, et12M, et19M, {{{31, 49, 72, 87}}, "co"}];
test2args[temperamentDifference, et12M, et19M, {{{7, 11, 16, 19}}, "co"}];
et12C = dual[et12M];
et19C = dual[et19M];
test2args[temperamentSum, et12C, et19C, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "contra"}];
test2args[temperamentDifference, et12C, et19C, {{{-11, 7, 0, 0}, {-7, 3, 1, 0}, {-9, 4, 0, 1}}, "contra"}];

(* examples with themselves *)
test2args[temperamentSum, meantoneM, meantoneM, meantoneM];
test2args[temperamentDifference, meantoneM, meantoneM, Error];
test2args[temperamentSum, meantoneC, meantoneC, meantoneC];
test2args[temperamentDifference, meantoneC, meantoneC, Error];
test2args[temperamentSum, et7M, et7M, et7M];
test2args[temperamentDifference, et7M, et7M, Error];
test2args[temperamentSum, et7C, et7C, et7C];
test2args[temperamentDifference, et7C, et7C, Error];

(* mismatched r & n but matching d *)
test2args[temperamentSum, et7M, meantoneM, Error];
test2args[temperamentDifference, et7M, meantoneM, Error];
test2args[temperamentSum, et7C, meantoneC, Error];
test2args[temperamentDifference, et7C, meantoneC, Error];

(* mismatched d but matching r or n *)
test2args[temperamentSum, et7M, et12M, Error];
test2args[temperamentDifference, et7M, et12M, Error];
test2args[temperamentSum, et7C, et12C, Error];
test2args[temperamentDifference, et7C, et12C, Error];

(* some basic examples *)
augmentedM = {{{3, 0, 7}, {0, 1, 0}}, "co"}; (* ⟨⟨3 0 -7]] *)
diminishedM = {{{4, 0, 3}, {0, 1, 1}}, "co"}; (* ⟨⟨4 4 -3]] *)
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "co"}; (* ⟨⟨4 9 5]] *)
dicotM = {{{1, 1, 2}, {0, 2, 1}}, "co"}; (* ⟨⟨2 1 -3]] *)
srutalM = {{{2, 0, 11}, {0, 1, -2}}, "co"}; (* ⟨⟨2 -4 -11]] *)
test2args[temperamentSum, augmentedM, diminishedM, {{{1, 1, 2}, {0, 7, 4}}, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]]*)
test2args[temperamentDifference, augmentedM, diminishedM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ];(* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]]*)
test2args[temperamentSum, augmentedM, tetracotM, {{{1, 6, 8}, {0, 7, 9}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]]*)
test2args[temperamentDifference, augmentedM, tetracotM, {{{1, 0, -12}, {0, 1, 9}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]]*)
test2args[temperamentSum, augmentedM, dicotM, {{{1, 0, 2}, {0, 5, 1}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]]*)
test2args[temperamentDifference, augmentedM, dicotM, {{{1, 0, 4}, {0, 1, -1}}, "co"} ] ;(* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]]*)
test2args[temperamentSum, augmentedM, srutalM, {{{1, 2, 2}, {0, 5, -4}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]]*)
test2args[temperamentDifference, augmentedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]]*)
test2args[temperamentSum, diminishedM, tetracotM, {{{1, 2, 3}, {0, 8, 13}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]]*)
test2args[temperamentDifference, diminishedM, tetracotM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]]*)
test2args[temperamentSum, diminishedM, dicotM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ];(* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]]*)
test2args[temperamentDifference, diminishedM, dicotM, {{{1, 0, 0}, {0, 2, 3}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]]*)
test2args[temperamentSum, diminishedM, srutalM, {{{3, 0, 7}, {0, 1, 0}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] → ⟨⟨3 0 -7]] *)
test2args[temperamentDifference, diminishedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] → ⟨⟨1 4 4]] *)
test2args[temperamentSum, tetracotM, dicotM, {{{1, 2, 3}, {0, 3, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] → ⟨⟨3 5 1]] *)
test2args[temperamentDifference, tetracotM, dicotM, {{{1, 0, -4}, {0, 1, 4}}, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] → ⟨⟨1 4 4]] *)
test2args[temperamentSum, tetracotM, srutalM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[temperamentDifference, tetracotM, srutalM, {{{1, 0, -8}, {0, 2, 13}}, "co"} ];  (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[temperamentSum, dicotM, srutalM, {{{1, 2, 2}, {0, 4, -3}}, "co"} ]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[temperamentDifference, dicotM, srutalM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example that requires the breadth-first search of linear combinations of multiple collinear vectors *)
test2args[temperamentSum, {{{-3, -8, 4, 6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];




Print["TOTAL FAILURES: ", f];
