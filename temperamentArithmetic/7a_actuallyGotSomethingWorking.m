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
      validTemperamentArithmetic[t1, t2, isSum, collinearity]
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

getCollinearVectorMultiplesForMaxMultiple[grade_, maxMultiple_] := Module[{collinearVectorMultiples, i, collinearVectorMultiplesForMaxMultiple},
  collinearVectorMultiples = Table[0, grade];
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

getCollinearVectorMultiplePermutationsForMaxMultiple[grade_, maxMultiple_] := Flatten[Map[
  Permutations,
  getCollinearVectorMultiplesForMaxMultiple[grade, maxMultiple]
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

defactorWhileLockingCollinearVectors[t_, collinearity_] := Module[
  {
    tContra,
    collinearityContra,
    grade,
    collinearVectors,
    a,
    originalTerminalVector,
    collinearVectorMultiplePermutations,
    i,
    maxMultiple,
    collinearVectorLinearCombination,
    candidateTerminalVectorBeforeDefactoring,
    candidateTerminalVector
  },

  tContra = isContra[t];
  collinearityContra = isContra[collinearity];
  grade = If[
    tContra == collinearityContra,
    If[
      tContra,
      getN[t],
      getR[t]
    ],
    If[
      tContra,
      getR[t],
      getN[t]
    ]
  ];
  collinearVectors = getA[collinearity];
  a = Take[Join[collinearVectors, If[collinearityContra, getC[t], getM[t]]], grade];
  originalTerminalVector = a[[grade]];
  collinearVectorMultiplePermutations = {};
  i = 1;
  maxMultiple = 1;

  While[
    isEnfactored[a],

    If[
      i <= Length[collinearVectorMultiplePermutations],

      collinearVectorLinearCombination = getCollinearVectorLinearCombination[collinearVectors, collinearVectorMultiplePermutations[[i]]];

      candidateTerminalVectorBeforeDefactoring = originalTerminalVector + collinearVectorLinearCombination;
      candidateTerminalVector = divideOutGcd[candidateTerminalVectorBeforeDefactoring];
      If[
        candidateTerminalVectorBeforeDefactoring != candidateTerminalVector,

        originalTerminalVector = candidateTerminalVector;
        collinearVectorMultiplePermutations = {};
        maxMultiple = 1;
        i = 1,

        i++;
      ];
      a[[grade]] = candidateTerminalVector,

      collinearVectorMultiplePermutations = getCollinearVectorMultiplePermutationsForMaxMultiple[Length[collinearVectors], maxMultiple];
      maxMultiple++;
      i = 1;
    ];
  ];

  a
];

(* TODO: still sad that I need Minors[] to figure this out... figure out if there's any other way *)
isNegativeOrientationOfTemperamentMatrix[t_] := Module[{contra, grade, minors, normalizingEntry},
  contra = isContra[t];
  grade = If[contra, getN[t], getR[t]];
  minors = First[Minors[getA[t], grade]];
  normalizingEntry = If[contra, trailingEntry[minors], leadingEntry[minors]]; (* TODO: variable functions? *)

  normalizingEntry < 0
];

validTemperamentArithmetic[t1_, t2_, isSum_, collinearity_] := Module[{tContra, collinearityContra, contra, grade, a1, a2},
  (* contra = isContra[t1];
   grade = If[contra, getN[t1], getR[t1]];*)

  tContra = isContra[t1];
  collinearityContra = isContra[collinearity];
  grade = If[
    tContra == collinearityContra,
    If[
      tContra,
      getN[t1],
      getR[t1]
    ],
    If[
      tContra,
      getR[t1],
      getN[t1]
    ]
  ];

  a1 = defactorWhileLockingCollinearVectors[t1, collinearity];
  a2 = defactorWhileLockingCollinearVectors[t2, collinearity];

  (*Print["is it arleayd rworng here????", a1,a2,t1,t2,getA[collinearity]];*)

  If[
    isNegativeOrientationOfTemperamentMatrix[{a1, getV[collinearity]}],
    a1[[grade]] = -a1[[grade]]
  ];

  If[
    isSum,
    If[
      isNegativeOrientationOfTemperamentMatrix[{a2, getV[collinearity]}],
      a2[[grade]] = -a2[[grade]]
    ],
    If[
      !isNegativeOrientationOfTemperamentMatrix[{a2, getV[collinearity]}],
      a2[[grade]] = -a2[[grade]]
    ]
  ];

  (* Print["what the f", a1, a2, contra, grade, a1 + a2];*)
  (*Print["aw cmon! getV[t1]: ", getV[t1], " getV[collinearity]: ", getV[collinearity], " a1+a2: ", a1+a2, " a1: ", a1, " a2: ", a2, "and the collinearity is: ", collinearity];*)

  If[
    getV[t1] == getV[collinearity],
    canonicalForm[{a1 + a2, getV[collinearity]}] ,
    dual[{a1 + a2, getV[collinearity]}]
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
test2args[temperamentSum, diminishedM, srutalM, {{{3, 0, 7}, {0, 1, 0}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test2args[temperamentDifference, diminishedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[temperamentSum, tetracotM, dicotM, {{{1, 2, 3}, {0, 3, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test2args[temperamentDifference, tetracotM, dicotM, {{{1, 0, -4}, {0, 1, 4}}, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[temperamentSum, tetracotM, srutalM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[temperamentDifference, tetracotM, srutalM, {{{1, 0, -8}, {0, 2, 13}}, "co"} ];  (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[temperamentSum, dicotM, srutalM, {{{1, 2, 2}, {0, 4, -3}}, "co"} ]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[temperamentDifference, dicotM, srutalM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example that requires the breadth-first search of linear combinations of multiple collinear vectors *)
test2args[temperamentSum, {{{-3, -8, 4, 6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];

(* example that was intractable unless I defactored piecemeal *)
test2args[temperamentSum, {{{-97, 73, 45, 16}}, "contra"}, {{{-1, 8, 9, 3}}, "contra"}, {{{-98, 81, 54, 19}}, "contra"}];




Print["TOTAL FAILURES: ", f];
