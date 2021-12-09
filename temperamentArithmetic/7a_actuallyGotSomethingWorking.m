(* TODO: sort these functions in a more logical order *)

getCollinearity[collinearityT_] := Length[getA[collinearityT]];

isCollinear[collinearityT_] := getCollinearity[collinearityT] > 0;

isMonononcollinear[collinearityT_, t_] := If[
  isContra[collinearityT],
  getCollinearity[collinearityT] === getN[t] - 1,
  getCollinearity[collinearityT] === getR[t] - 1
];

getCollinearityT[t1_, t2_] := Module[{collinearityM, collinearityC},
  collinearityM = dual[join[t1, t2]];
  collinearityC = dual[meet[t1, t2]];

  collinearityM[[1]] = removeAllZeroRows[collinearityM[[1]]];
  collinearityC[[1]] = removeAllZeroRows[collinearityC[[1]]];

  If[
    isMonononcollinear[collinearityC, t1] && isCollinear[collinearityC],
    collinearityC,
    If[
      isMonononcollinear[collinearityM, t1],
      collinearityM,
      Error
    ]
  ]
];

tShapesDoNotMatch[t1_, t2_] := getR[t1] != getR[t2] || getD[t1] != getD[t2];

variancesMatch[t1_, t2_] := getV[t1] == getV[t2];

arithmetic[t1_, t2_, isSum_] := If[
  tShapesDoNotMatch[t1, t2],
  Error,
  Module[{collinearityT, tSumAndDiff},
    collinearityT = getCollinearityT[t1, t2];
    tSumAndDiff = If[
      collinearityT === Error,
      Error,
      getSumAndDiff[t1, t2, collinearityT]
    ];

    If[
      tSumAndDiff === Error,
      Error,
      chooseCorrectlyBetweenSumAndDiff[t1, t2, isSum, tSumAndDiff]
    ]
  ]
];

getSumMinorsChecker[t1_, t2_] := Module[{t2sameVariance},
  t2sameVariance = If[getV[t1] != getV[t2], dual[t2], t2];

  divideOutGcd[getMinors[t1] + getMinors[t2sameVariance]]
];

chooseCorrectlyBetweenSumAndDiff[t1_, t2_, isSum_, tSumAndDiff_] := Module[
  {
    tSum,
    tDiff,
    tSumMinors,
    tSumMinorsChecker,
    tSumMinorsMatch
  },

  tSum = First[tSumAndDiff];
  tDiff = Last[tSumAndDiff];
  tSumMinors = getMinors[tSum];
  tSumMinorsChecker = getSumMinorsChecker[t1, t2];
  tSumMinorsMatch = tSumMinors == tSumMinorsChecker;

  If[
    isSum,
    If[tSumMinorsMatch, tSum, tDiff],
    If[tSumMinorsMatch, tDiff, tSum]
  ]
];

getMinors[t_] := Module[{contra, grade, minors, entryFn, normalizingEntry},
  contra = isContra[t];
  grade = If[contra, getN[t], getR[t]];
  minors = divideOutGcd[First[Minors[getA[t], grade]]];
  entryFn = If[contra, trailingEntry, leadingEntry];
  normalizingEntry = entryFn[minors];

  If[normalizingEntry < 0, -minors, minors]
];

getGradeMatchingCollinearity[t_, collinearityT_] := If[
  variancesMatch[t, collinearityT],
  If[isContra[t], getN[t], getR[t]],
  If[isContra[t], getR[t], getN[t]]
];

getSumAndDiff[t1_, t2_, collinearityT_] := Module[
  {
    grade,
    collinearUnvariancedVectors,
    a1,
    a2,
    tSum,
    tDiff,
    a1noncollinearVector,
    a2noncollinearVector,
    noncollinearVectorSum,
    noncollinearVectorDiff
  },

  grade = getGradeMatchingCollinearity[t1, collinearityT];
  collinearUnvariancedVectors = getA[collinearityT];

  a1 = defactorWhileLockingCollinearUnvariancedVectors[t1, collinearityT];
  a2 = defactorWhileLockingCollinearUnvariancedVectors[t2, collinearityT];

  a1noncollinearVector = Last[a1];
  a2noncollinearVector = Last[a2];
  noncollinearVectorSum = a1noncollinearVector + a2noncollinearVector;
  noncollinearVectorDiff = a1noncollinearVector - a2noncollinearVector;

  tSum = {Join[collinearUnvariancedVectors, {noncollinearVectorSum}], getV[collinearityT]};
  tSum = If[variancesMatch[t1, collinearityT], canonicalForm[tSum], dual[tSum]];

  tDiff = {Join[collinearUnvariancedVectors, {noncollinearVectorDiff}], getV[collinearityT]};
  tDiff = If[variancesMatch[t1, collinearityT], canonicalForm[tDiff], dual[tDiff]];

  {tSum, tDiff}
];

sum[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalForm[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];

  If[
    t1 == t2,
    t1,
    arithmetic[t1, t2, True]
  ]
];

diff[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalForm[t1input];
  t2 = If[variancesMatch[t1input, t2input], canonicalForm[t2input], dual[t2input]];

  If[
    t1 == t2,
    Error,
    arithmetic[t1, t2, False]
  ]
];

getCollinearUnvariancedVectorLinearCombination[collinearUnvariancedVectors_, collinearVectorMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {collinearUnvariancedVectors, collinearVectorMultiplePermutation}
]];

getEnfactoredDetA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getEnfactoring[a_] := Det[getEnfactoredDetA[a]];

getInitialLockedCollinearUnvariancedVectorsFormOfA[t_, collinearityT_, grade_, collinearUnvariancedVectors_] := Module[
  {
    potentiallyNoncollinearUnvariancedVectors,
    lockedCollinearUnvariancedVectorsFormOfA
  },

  potentiallyNoncollinearUnvariancedVectors = If[isContra[collinearityT], getC[t], getM[t]];
  lockedCollinearUnvariancedVectorsFormOfA = collinearUnvariancedVectors;

  Do[
    candidate = hnf[Join[collinearUnvariancedVectors, {potentiallyNoncollinearVector}]];
    If[
      Length[lockedCollinearUnvariancedVectorsFormOfA] < grade && MatrixRank[candidate] > Length[collinearUnvariancedVectors],
      lockedCollinearUnvariancedVectorsFormOfA = Join[lockedCollinearUnvariancedVectorsFormOfA, {potentiallyNoncollinearVector}]
    ],
    {potentiallyNoncollinearVector, potentiallyNoncollinearUnvariancedVectors}
  ];

  Take[lockedCollinearUnvariancedVectorsFormOfA, grade]
];

defactorWhileLockingAtLeastOneCollinearUnvariancedVector[t_, collinearityT_, grade_, collinearUnvariancedVectors_, lockedCollinearUnvariancedVectorsFormOfAInput_] := Module[
  {
    lockedCollinearUnvariancedVectorsFormOfA,
    d,
    collinearity,
    enfactoring,
    multiples,
    equations,
    answer,
    result
  },

  lockedCollinearUnvariancedVectorsFormOfA = lockedCollinearUnvariancedVectorsFormOfAInput;
  d = getD[t];
  collinearity = getCollinearity[collinearityT];
  enfactoring = getEnfactoring[lockedCollinearUnvariancedVectorsFormOfA];
  multiples = Table[Subscript[x, i], {i, collinearity}];
  equations = Map[
    Function[
      dIndex,
      Mod[lockedCollinearUnvariancedVectorsFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * collinearUnvariancedVectors[[multiplesIndex]][[dIndex]]],
        Range[collinearity]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];

  lockedCollinearUnvariancedVectorsFormOfA[[grade]] = divideOutGcd[lockedCollinearUnvariancedVectorsFormOfA[[grade]] + getCollinearUnvariancedVectorLinearCombination[collinearUnvariancedVectors, result]];

  lockedCollinearUnvariancedVectorsFormOfA
];

defactorWhileLockingCollinearUnvariancedVectors[t_, collinearityT_] := Module[
  {
    grade,
    collinearUnvariancedVectors,
    lockedCollinearUnvariancedVectorsFormOfA
  },

  grade = getGradeMatchingCollinearity[t, collinearityT];
  collinearUnvariancedVectors = getA[collinearityT];
  lockedCollinearUnvariancedVectorsFormOfA = getInitialLockedCollinearUnvariancedVectorsFormOfA[t, collinearityT, grade, collinearUnvariancedVectors];

  If[
    isCollinear[collinearityT],
    defactorWhileLockingAtLeastOneCollinearUnvariancedVector[t, collinearityT, grade, collinearUnvariancedVectors, lockedCollinearUnvariancedVectorsFormOfA],
    lockedCollinearUnvariancedVectorsFormOfA
  ]
];




(* most examples tested from both sides of duality *)

f = 0;
p = 0;

(* collinear mappings*)
meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
test2args[sum, meantoneM, porcupineM, {{{1, 1, 1}, {0, 4, 9}}, "co"}];
test2args[diff, meantoneM, porcupineM, {{{1, 1, 2}, {0, 2, 1}}, "co"}];
meantoneC = {{{4, -4, 1}}, "contra"};
porcupineC = {{{1, -5, 3}}, "contra"};
test2args[sum, meantoneC, porcupineC, {{{5, -9, 4}}, "contra"}];
test2args[diff, meantoneC, porcupineC, {{{-3, -1, 2}}, "contra"}];

(* collinear comma bases *)
et7M = {{{7, 11, 16}}, "co"};
et5M = {{{5, 8, 12}}, "co"};
test2args[sum, et7M, et5M, {{{12, 19, 28}}, "co"}];
test2args[diff, et7M, et5M, {{{2, 3, 4}}, "co"}];
et7C = dual[et7M];
et5C = dual[et5M];
test2args[sum, et7C, et5C, {{{-19, 12, 0}, {-15, 8, 1}}, "contra"}];
test2args[diff, et7C, et5C, {{{-3, 2, 0}, {-2, 0, 1}}, "contra"}];

(* noncollinear - error! *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "co"};
test2args[sum, septimalMeantoneM, septimalBlackwoodM, Error];
test2args[diff, septimalMeantoneM, septimalBlackwoodM, Error];
septimalMeantoneC = dual[septimalMeantoneM];
septimalBlackwoodC = dual[septimalBlackwoodM];
test2args[sum, septimalMeantoneC, septimalBlackwoodC, Error];
test2args[diff, septimalMeantoneC, septimalBlackwoodC, Error];

(* doubly collinear (comma bases) *)
et12M = {{{12, 19, 28, 34}}, "co"};
et19M = {{{19, 30, 44, 53}}, "co"};
test2args[sum, et12M, et19M, {{{31, 49, 72, 87}}, "co"}];
test2args[diff, et12M, et19M, {{{7, 11, 16, 19}}, "co"}];
et12C = dual[et12M];
et19C = dual[et19M];
test2args[sum, et12C, et19C, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "contra"}];
test2args[diff, et12C, et19C, {{{-11, 7, 0, 0}, {-7, 3, 1, 0}, {-9, 4, 0, 1}}, "contra"}];

(* examples with themselves *)
test2args[sum, meantoneM, meantoneM, meantoneM];
test2args[diff, meantoneM, meantoneM, Error];
test2args[sum, meantoneC, meantoneC, meantoneC];
test2args[diff, meantoneC, meantoneC, Error];
test2args[sum, et7M, et7M, et7M];
test2args[diff, et7M, et7M, Error];
test2args[sum, et7C, et7C, et7C];
test2args[diff, et7C, et7C, Error];

(* mismatched r & n but matching d *)
test2args[sum, et7M, meantoneM, Error];
test2args[diff, et7M, meantoneM, Error];
test2args[sum, et7C, meantoneC, Error];
test2args[diff, et7C, meantoneC, Error];

(* mismatched d but matching r or n *)
test2args[sum, et7M, et12M, Error];
test2args[diff, et7M, et12M, Error];
test2args[sum, et7C, et12C, Error];
test2args[diff, et7C, et12C, Error];

(* some basic examples *)
augmentedM = {{{3, 0, 7}, {0, 1, 0}}, "co"}; (* ⟨⟨3 0 -7]] *)
diminishedM = {{{4, 0, 3}, {0, 1, 1}}, "co"}; (* ⟨⟨4 4 -3]] *)
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "co"}; (* ⟨⟨4 9 5]] *)
dicotM = {{{1, 1, 2}, {0, 2, 1}}, "co"}; (* ⟨⟨2 1 -3]] *)
srutalM = {{{2, 0, 11}, {0, 1, -2}}, "co"}; (* ⟨⟨2 -4 -11]] *)
test2args[sum, augmentedM, diminishedM, {{{1, 1, 2}, {0, 7, 4}}, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]]*)
test2args[diff, augmentedM, diminishedM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ];(* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]]*)
test2args[sum, augmentedM, tetracotM, {{{1, 6, 8}, {0, 7, 9}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]]*)
test2args[diff, augmentedM, tetracotM, {{{1, 0, -12}, {0, 1, 9}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]]*)
test2args[sum, augmentedM, dicotM, {{{1, 0, 2}, {0, 5, 1}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]]*)
test2args[diff, augmentedM, dicotM, {{{1, 0, 4}, {0, 1, -1}}, "co"} ] ;(* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]]*)
test2args[sum, augmentedM, srutalM, {{{1, 2, 2}, {0, 5, -4}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]]*)
test2args[diff, augmentedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]]*)
test2args[sum, diminishedM, tetracotM, {{{1, 2, 3}, {0, 8, 13}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]]*)
test2args[diff, diminishedM, tetracotM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]]*)
test2args[sum, diminishedM, dicotM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ];(* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]]*)
test2args[diff, diminishedM, dicotM, {{{1, 0, 0}, {0, 2, 3}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]]*)
test2args[sum, diminishedM, srutalM, {{{3, 0, 7}, {0, 1, 0}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test2args[diff, diminishedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[sum, tetracotM, dicotM, {{{1, 2, 3}, {0, 3, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test2args[diff, tetracotM, dicotM, {{{1, 0, -4}, {0, 1, 4}}, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[sum, tetracotM, srutalM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[diff, tetracotM, srutalM, {{{1, 0, -8}, {0, 2, 13}}, "co"} ];  (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[sum, dicotM, srutalM, {{{1, 2, 2}, {0, 4, -3}}, "co"} ]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[diff, dicotM, srutalM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of collinear, but not monononcollinear: d = 5, min-grade = 2, noncollinearity = 2 *)
t1 = {{{1, 1, 0, 30, -19}, {0, 0, 1, 6, -4}, {0, 0, 0, 41, -27}}, "co"};
t2 = {{{2, 0, 19, 45, 16}, {0, 1, 19, 55, 18}, {0, 0, 24, 70, 23}}, "co"};
test2args[sum, t1, t2, Error];
test2args[diff, t1, t2, Error];

(* example of monononcollinear, but not collinear: d = 2, min-grade = 1, noncollinearity = 1 *)
t1 = {{{2, 3}}, "contra"};
t2 = {{{4, -7}}, "co"};
tSum = {{{9, 7}}, "contra"};
tDiff = {{{5, 1}}, "contra"};
test2args[sum, t1, t2, tSum];
test2args[diff, t1, t2, tDiff];

(* example demonstrating how it's important to canonicalize *)
t1 = {{{-2, 4, -2}}, "co"};
t2 = {{{7, 7, 0}}, "co"};
tSum = {{{2, -1, 1}}, "co"};
tDiff = {{{0, 3, -1}}, "co"};
test2args[sum, t1, t2, tSum];
test2args[diff, t1, t2, tDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
t1 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
t2 = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
tSum = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
test2args[sum, t1, t2, tSum];
test2args[sum, dual[t1], t2, dual[tSum]];
test2args[sum, t1, dual[t2], tSum];
test2args[sum, dual[t1], dual[t2], dual[tSum]];

(* LA only: example that required the breadth-first search of linear combinations of multiple collinear vectors *)
test2args[sum, {{{3, 8, -4, -6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];

(* LA only: example that was intractable unless I defactored piecemeal *)
test2args[sum, {{{-97, 73, 45, 16}}, "contra"}, {{{-1, 8, 9, 3}}, "contra"}, {{{-98, 81, 54, 19}}, "contra"}];

(* LA only: example that motivated the existence of the special min-grade-1 path... which no longer exists, but I'll keep this around anyway *)
test2args[sum, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{7, 4, 3}}, "contra"}];
test2args[diff, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{-3, -4, 3}}, "contra"}];

(* LA only: non-min-grade-1 *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
flattoneM = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
godzillaM = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
et19MwithIndependent7 = {{{19, 30, 44, 0}, {0, 0, 0, 1}}, "co"};
test2args[sum, septimalMeantoneM, flattoneM, godzillaM];
test2args[diff, septimalMeantoneM, flattoneM, et19MwithIndependent7];

(* LA only: ensure the minors are consulted so that the sum and diff are identified correctly *)
t1 = {{{0, 1, 4}}, "co"};
t2 = {{{5, -6, -2}}, "co"};
tSum = {{{5, -5, 2}}, "co"};
tDiff = {{{5, -7, -6}}, "co"};
test2args[sum, t1, t2, tSum];
test2args[diff, t1, t2, tDiff];

(* LA only: an example that makes sure that even if the input matrices explicitly share the vector, it still works *)
t1 = {{{-3, 2, 0, 0}, {-2, 0, 0, 1}}, "contra"};
t2 = {{{-3, 2, 0, 0}, {-4, 1, 1, 0}}, "contra"};
test2args[sum, t1, t2, {{{-3, 2, 0, 0}, {-6, 1, 1, 1}}, "contra"}];
test2args[diff, t1, t2, {{{-3, 2, 0, 0}, {-1, 1, -1, 1}}, "contra"}];

(* LA only: an example that was intractable with the breadth-first search of linear combinations code the first way I wrote it, but is tractable using my fancier style essentially using a Wolfram Solve[]*)
t1 = {{{5, -1, -4, 9, -3}, {0, -7, -1, -8, -2}}, "co"};
t2 = {{{5, -1, -4, 9, -3}, {-5, 2, -4, -3, -9}}, "co"};
test2args[sum, t1, t2, {{{5, 7, -11, 23, -13}, {0, 8, -7, 14, -10}}, "co"}];
test2args[diff, t1, t2, {{{5, 5, 5, 11, 11}, {0, 6, 9, 2, 14}}, "co"}];

(* LA only: example where the first vectors of the input were not actually noncollinear with the collinear vectors, things would fail, so now we actually test each one to ensure it's noncollinear before adding it into the initial matrix to be defactored *)
test2args[sum, {{{-17, -55, 24, 34}}, "contra"}, {{{-1, -7, 0, 2}}, "contra"}, {{{-9, -31, 12, 18}}, "contra"}];



Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];
