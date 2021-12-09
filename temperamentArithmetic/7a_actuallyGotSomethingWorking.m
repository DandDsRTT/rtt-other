(* TODO: sort these functions in a more logical order *)

(* TODO: actually shouldn't "temperament" be abbreviated to "T" in the fn names? or maybe actually it should just be "sum" and "diff"... t should be assumed? *)

getCollinearity[collinearityVariancedMatrix_] := Length[getA[collinearityVariancedMatrix]];

isCollinear[collinearityVariancedMatrix_] := getCollinearity[collinearityVariancedMatrix] > 0;

isMonononcollinear[collinearityVariancedMatrix_, t_] := If[
  isContra[collinearityVariancedMatrix],
  getCollinearity[collinearityVariancedMatrix] === getN[t] - 1,
  getCollinearity[collinearityVariancedMatrix] === getR[t] - 1
];

getCollinearityVariancedMatrix[t1_, t2_] := Module[{collinearityCovariantMatrix, collinearityContravariantMatrix},
  collinearityCovariantMatrix = dual[join[t1, t2]];
  collinearityContravariantMatrix = dual[meet[t1, t2]];

  collinearityCovariantMatrix[[1]] = removeAllZeroRows[collinearityCovariantMatrix[[1]]];
  collinearityContravariantMatrix[[1]] = removeAllZeroRows[collinearityContravariantMatrix[[1]]];

  If[
    isMonononcollinear[collinearityContravariantMatrix, t1] && isCollinear[collinearityContravariantMatrix],
    collinearityContravariantMatrix,
    If[
      isMonononcollinear[collinearityCovariantMatrix, t1],
      collinearityCovariantMatrix,
      Error
    ]
  ]
];

temperamentSizesDoNotMatch[t1_, t2_] := getR[t1] != getR[t2] || getD[t1] != getD[t2];

temperamentArithmetic[t1_, t2_, isSum_] := If[
  temperamentSizesDoNotMatch[t1, t2],
  Error,
  Module[{collinearityVariancedMatrix, tSumAndDiff},
    collinearityVariancedMatrix = getCollinearityVariancedMatrix[t1, t2];
    tSumAndDiff = If[
      collinearityVariancedMatrix === Error,
      Error,
      getSumAndDiff[t1, t2, collinearityVariancedMatrix]
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

getGradeMatchingCollinearity[t_, collinearityVariancedMatrix_] := Module[{tContra, collinearityContra},
  tContra = isContra[t];
  collinearityContra = isContra[collinearityVariancedMatrix];

  If[
    tContra == collinearityContra,
    If[tContra, getN[t], getR[t]],
    If[tContra, getR[t], getN[t]]
  ]
];

getSumAndDiff[t1_, t2_, collinearityVariancedMatrix_] := Module[
  {
    grade,
    collinearVectors,
    a1,
    a2,
    tSum,
    tDiff,
    a1noncollinearVector,
    a2noncollinearVector,
    noncollinearVectorSum,
    noncollinearVectorDiff,
    v,
    collinearityV
  },

  grade = getGradeMatchingCollinearity[t1, collinearityVariancedMatrix];
  collinearVectors = getA[collinearityVariancedMatrix];

  a1 = defactorWhileLockingCollinearVectors[t1, collinearityVariancedMatrix];
  a2 = defactorWhileLockingCollinearVectors[t2, collinearityVariancedMatrix];

  a1noncollinearVector = Last[a1];
  a2noncollinearVector = Last[a2];
  noncollinearVectorSum = a1noncollinearVector + a2noncollinearVector;
  noncollinearVectorDiff = a1noncollinearVector - a2noncollinearVector;

  v = getV[t1];
  collinearityV = getV[collinearityVariancedMatrix];

  tSum = {Join[collinearVectors, {noncollinearVectorSum}], collinearityV};
  tSum = If[v == collinearityV, canonicalForm[tSum], dual[tSum]];

  tDiff = {Join[collinearVectors, {noncollinearVectorDiff}], collinearityV};
  tDiff = If[v == collinearityV, canonicalForm[tDiff], dual[tDiff]];

  {tSum, tDiff}
];

temperamentSum[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalForm[t1input];
  t2 = canonicalForm[t2input];

  If[
    t1 == t2,
    t1,
    temperamentArithmetic[t1, t2, True]
  ]
];

temperamentDiff[t1input_, t2input_] := Module[{t1, t2},
  t1 = canonicalForm[t1input];
  t2 = canonicalForm[t2input];

  If[
    t1 == t2,
    {{Table[0, getD[t1]]}, getV[t1]},
    temperamentArithmetic[t1, t2, False]
  ]
];


getCollinearVectorLinearCombination[collinearVectors_, collinearVectorMultiplePermutation_] := Total[MapThread[
  #1 * #2&,
  {collinearVectors, collinearVectorMultiplePermutation}
]];

getEnfactoredDetA[a_] := Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]];

getEnfactoring[a_] := Det[getEnfactoredDetA[a]];

filterCollinearVectorCopies[noncollinearVectors_, collinearVectors_] := Module[{filtered},
  filtered = Select[noncollinearVectors, !MemberQ[collinearVectors, #]&];

  Select[filtered, !MemberQ[collinearVectors, -#]&]
];

getInitialLockedCollinearVectorsFormOfA[t_, collinearityVariancedMatrix_, grade_, collinearVectors_] := Module[
  {
    noncollinearVectors,
    lockedCollinearVectorsFormOfA
  },

  noncollinearVectors = If[isContra[collinearityVariancedMatrix], getC[t], getM[t]];
  noncollinearVectors = filterCollinearVectorCopies[noncollinearVectors, collinearVectors];
  lockedCollinearVectorsFormOfA = Join[collinearVectors, noncollinearVectors];

  (* TODO: maybe it would be even more secure and not even required this throw if you could do something like
  multiply each vector in the latter thing if necessary by consecutive prime numbers
  to ensure no enfactoring or something but also ensure no matches *)
  If[
    Length[lockedCollinearVectorsFormOfA] < grade,
    Throw["were not enough vectors to perform temperament arithmetic"],
    Take[lockedCollinearVectorsFormOfA, grade]
  ]
];

defactorWhileLockingAtLeastOneCollinearVector[t_, collinearityVariancedMatrix_, grade_, collinearVectors_, lockedCollinearVectorsFormOfAInput_] := Module[
  {
    lockedCollinearVectorsFormOfA,
    d,
    collinearity,
    enfactoring,
    multiples,
    equations,
    answer,
    result,
  },

  lockedCollinearVectorsFormOfA = lockedCollinearVectorsFormOfAInput;
  d = getD[t];
  collinearity = getCollinearity[collinearityVariancedMatrix];
  enfactoring = getEnfactoring[lockedCollinearVectorsFormOfA];
  multiples = Table[Subscript[x, i], {i, collinearity}];
  equations = Map[
    Function[
      dIndex,
      Mod[lockedCollinearVectorsFormOfA[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * collinearVectors[[multiplesIndex]][[dIndex]]],
        Range[collinearity]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];
  answer = FindInstance[equations, multiples, Integers];
  result = Values[Association[answer]];

  lockedCollinearVectorsFormOfA[[grade]] = divideOutGcd[lockedCollinearVectorsFormOfA[[grade]] + getCollinearVectorLinearCombination[collinearVectors, result]];

  lockedCollinearVectorsFormOfA
];

defactorWhileLockingCollinearVectors[t_, collinearityVariancedMatrix_] := Module[
  {
    grade,
    collinearVectors,
    lockedCollinearVectorsFormOfA
  },

  grade = getGradeMatchingCollinearity[t, collinearityVariancedMatrix];
  collinearVectors = getA[collinearityVariancedMatrix];
  lockedCollinearVectorsFormOfA = getInitialLockedCollinearVectorsFormOfA[t, collinearityVariancedMatrix, grade, collinearVectors];

  If[
    isCollinear[collinearityVariancedMatrix],
    defactorWhileLockingAtLeastOneCollinearVector[t, collinearityVariancedMatrix, grade, collinearVectors, lockedCollinearVectorsFormOfA],
    lockedCollinearVectorsFormOfA
  ]
];




(* most examples tested from both sides of duality *)

f = 0;
p = 0;

(* collinear mappings*)
meantoneM = {{{1, 0, -4}, {0, 1, 4}}, "co"};
porcupineM = {{{1, 2, 3}, {0, 3, 5}}, "co"};
test2args[temperamentSum, meantoneM, porcupineM, {{{1, 1, 1}, {0, 4, 9}}, "co"}];
test2args[temperamentDiff, meantoneM, porcupineM, {{{1, 1, 2}, {0, 2, 1}}, "co"}];
meantoneC = {{{4, -4, 1}}, "contra"};
porcupineC = {{{1, -5, 3}}, "contra"};
test2args[temperamentSum, meantoneC, porcupineC, {{{5, -9, 4}}, "contra"}];
test2args[temperamentDiff, meantoneC, porcupineC, {{{-3, -1, 2}}, "contra"}];

(* collinear comma bases *)
et7M = {{{7, 11, 16}}, "co"};
et5M = {{{5, 8, 12}}, "co"};
test2args[temperamentSum, et7M, et5M, {{{12, 19, 28}}, "co"}];
test2args[temperamentDiff, et7M, et5M, {{{2, 3, 4}}, "co"}];
et7C = dual[et7M];
et5C = dual[et5M];
test2args[temperamentSum, et7C, et5C, {{{-19, 12, 0}, {-15, 8, 1}}, "contra"}];
test2args[temperamentDiff, et7C, et5C, {{{-3, 2, 0}, {-2, 0, 1}}, "contra"}];

(* noncollinear - error! *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "co"};
test2args[temperamentSum, septimalMeantoneM, septimalBlackwoodM, Error];
test2args[temperamentDiff, septimalMeantoneM, septimalBlackwoodM, Error];
septimalMeantoneC = dual[septimalMeantoneM];
septimalBlackwoodC = dual[septimalBlackwoodM];
test2args[temperamentSum, septimalMeantoneC, septimalBlackwoodC, Error];
test2args[temperamentDiff, septimalMeantoneC, septimalBlackwoodC, Error];

(* doubly collinear (comma bases) *)
et12M = {{{12, 19, 28, 34}}, "co"};
et19M = {{{19, 30, 44, 53}}, "co"};
test2args[temperamentSum, et12M, et19M, {{{31, 49, 72, 87}}, "co"}];
test2args[temperamentDiff, et12M, et19M, {{{7, 11, 16, 19}}, "co"}];
et12C = dual[et12M];
et19C = dual[et19M];
test2args[temperamentSum, et12C, et19C, {{{-49, 31, 0, 0}, {-45, 27, 1, 0}, {-36, 21, 0, 1}}, "contra"}];
test2args[temperamentDiff, et12C, et19C, {{{-11, 7, 0, 0}, {-7, 3, 1, 0}, {-9, 4, 0, 1}}, "contra"}];

(* examples with themselves *)
test2args[temperamentSum, meantoneM, meantoneM, meantoneM];
test2args[temperamentDiff, meantoneM, meantoneM, {{{0, 0, 0}}, "co"}];
test2args[temperamentSum, meantoneC, meantoneC, meantoneC];
test2args[temperamentDiff, meantoneC, meantoneC, {{{0, 0, 0}}, "contra"}];
test2args[temperamentSum, et7M, et7M, et7M];
test2args[temperamentDiff, et7M, et7M, {{{0, 0, 0}}, "co"}];
test2args[temperamentSum, et7C, et7C, et7C];
test2args[temperamentDiff, et7C, et7C, {{{0, 0, 0}}, "contra"}];

(* mismatched r & n but matching d *)
test2args[temperamentSum, et7M, meantoneM, Error];
test2args[temperamentDiff, et7M, meantoneM, Error];
test2args[temperamentSum, et7C, meantoneC, Error];
test2args[temperamentDiff, et7C, meantoneC, Error];

(* mismatched d but matching r or n *)
test2args[temperamentSum, et7M, et12M, Error];
test2args[temperamentDiff, et7M, et12M, Error];
test2args[temperamentSum, et7C, et12C, Error];
test2args[temperamentDiff, et7C, et12C, Error];

(* some basic examples *)
augmentedM = {{{3, 0, 7}, {0, 1, 0}}, "co"}; (* ⟨⟨3 0 -7]] *)
diminishedM = {{{4, 0, 3}, {0, 1, 1}}, "co"}; (* ⟨⟨4 4 -3]] *)
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "co"}; (* ⟨⟨4 9 5]] *)
dicotM = {{{1, 1, 2}, {0, 2, 1}}, "co"}; (* ⟨⟨2 1 -3]] *)
srutalM = {{{2, 0, 11}, {0, 1, -2}}, "co"}; (* ⟨⟨2 -4 -11]] *)
test2args[temperamentSum, augmentedM, diminishedM, {{{1, 1, 2}, {0, 7, 4}}, "co"}]; (* ⟨⟨3 0 -7]] + ⟨⟨4 4 -3]] = ⟨⟨7 4 -10]]*)
test2args[temperamentDiff, augmentedM, diminishedM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ];(* ⟨⟨3 0 -7]] - ⟨⟨4 4 -3]] = ⟨⟨1 4 4]]*)
test2args[temperamentSum, augmentedM, tetracotM, {{{1, 6, 8}, {0, 7, 9}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨4 9 5]] = ⟨⟨7 9 -2]]*)
test2args[temperamentDiff, augmentedM, tetracotM, {{{1, 0, -12}, {0, 1, 9}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨4 9 5]] = ⟨⟨1 9 12]]*)
test2args[temperamentSum, augmentedM, dicotM, {{{1, 0, 2}, {0, 5, 1}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 1 -3]] = ⟨⟨5 1 -10]]*)
test2args[temperamentDiff, augmentedM, dicotM, {{{1, 0, 4}, {0, 1, -1}}, "co"} ] ;(* ⟨⟨3 0 -7]] - ⟨⟨2 1 -3]] = ⟨⟨1 -1 -4]]*)
test2args[temperamentSum, augmentedM, srutalM, {{{1, 2, 2}, {0, 5, -4}}, "co"} ] ;(* ⟨⟨3 0 -7]] + ⟨⟨2 -4 -11]] = ⟨⟨5 -4 -18]]*)
test2args[temperamentDiff, augmentedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨3 0 -7]] - ⟨⟨2 -4 -11]] = ⟨⟨1 4 4]]*)
test2args[temperamentSum, diminishedM, tetracotM, {{{1, 2, 3}, {0, 8, 13}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨4 9 5]] = ⟨⟨8 13 2]]*)
test2args[temperamentDiff, diminishedM, tetracotM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨4 9 5]] = ⟨⟨0 5 8]]*)
test2args[temperamentSum, diminishedM, dicotM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ];(* ⟨⟨4 4 -3]] + ⟨⟨2 1 -3]] = ⟨⟨6 5 -6]]*)
test2args[temperamentDiff, diminishedM, dicotM, {{{1, 0, 0}, {0, 2, 3}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 1 -3]] = ⟨⟨2 3 0]]*)
test2args[temperamentSum, diminishedM, srutalM, {{{3, 0, 7}, {0, 1, 0}}, "co"} ]; (* ⟨⟨4 4 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨6 0 -14]] \[RightArrow] ⟨⟨3 0 -7]] *)
test2args[temperamentDiff, diminishedM, srutalM, {{{1, 0, -4}, {0, 1, 4}}, "co"} ]; (* ⟨⟨4 4 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[temperamentSum, tetracotM, dicotM, {{{1, 2, 3}, {0, 3, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 1 -3]] = ⟨⟨6 10 2]] \[RightArrow] ⟨⟨3 5 1]] *)
test2args[temperamentDiff, tetracotM, dicotM, {{{1, 0, -4}, {0, 1, 4}}, "co"}]; (* ⟨⟨4 9 5]] - ⟨⟨2 1 -3]] = ⟨⟨2 8 8]] \[RightArrow] ⟨⟨1 4 4]] *)
test2args[temperamentSum, tetracotM, srutalM, {{{1, 0, 1}, {0, 6, 5}}, "co"} ]; (* ⟨⟨4 9 5]] + ⟨⟨2 -4 -11]] = ⟨⟨6 5 -6]] *)
test2args[temperamentDiff, tetracotM, srutalM, {{{1, 0, -8}, {0, 2, 13}}, "co"} ];  (* ⟨⟨4 9 5]] - ⟨⟨2 -4 -11]] = ⟨⟨2 13 16]] *)
test2args[temperamentSum, dicotM, srutalM, {{{1, 2, 2}, {0, 4, -3}}, "co"} ]; (* ⟨⟨2 1 -3]] + ⟨⟨2 -4 -11]] = ⟨⟨4 -3 -14]] *)
test2args[temperamentDiff, dicotM, srutalM, {{{5, 8, 0}, {0, 0, 1}}, "co"} ]; (* ⟨⟨2 1 -3]] - ⟨⟨2 -4 -11]] = ⟨⟨0 5 8]] *)

(* example of collinear, but not monononcollinear: d = 5, min-grade = 2, noncollinearity = 2 *)
t1 = {{{1, 1, 0, 30, -19}, {0, 0, 1, 6, -4}, {0, 0, 0, 41, -27}}, "co"};
t2 = {{{2, 0, 19, 45, 16}, {0, 1, 19, 55, 18}, {0, 0, 24, 70, 23}}, "co"};
test2args[temperamentSum, t1, t2, Error];
test2args[temperamentDiff, t1, t2, Error];

(* example of monononcollinear, but not collinear: d = 2, min-grade = 1, noncollinearity = 1 *)
t1 = {{{2, 3}}, "contra"};
t2 = {{{4, -7}}, "co"};
tSum = {{{9, 7}}, "contra"};
tDiff = {{{5, 1}}, "contra"};
test2args[temperamentSum, t1, t2, tSum];
test2args[temperamentDiff, t1, t2, tDiff];

(* example demonstrating how it's important to canonicalize *)
t1 = {{{-2, 4, -2}}, "co"};
t2 = {{{7, 7, 0}}, "co"};
tSum = {{{2, -1, 1}}, "co"};
tDiff = {{{0, 3, -1}}, "co"};
test2args[temperamentSum, t1, t2, tSum];
test2args[temperamentDiff, t1, t2, tDiff];

(* example demonstrating how mixed variance inputs are accepted, but the first variance matches the output *)
t1 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
t2 = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
tSum = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
test2args[temperamentSum, t1, t2, tSum];
test2args[temperamentSum, dual[t1], t2, dual[tSum]];
test2args[temperamentSum, t1, dual[t2], tSum];
test2args[temperamentSum, dual[t1], dual[t2], dual[tSum]];

(* LA only: example that required the breadth-first search of linear combinations of multiple collinear vectors *)
test2args[temperamentSum, {{{3, 8, -4, -6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];

(* LA only: example that was intractable unless I defactored piecemeal *)
test2args[temperamentSum, {{{-97, 73, 45, 16}}, "contra"}, {{{-1, 8, 9, 3}}, "contra"}, {{{-98, 81, 54, 19}}, "contra"}];

(* LA only: example that motivated the existence of the special min-grade-1 path... which no longer exists, but I'll keep this around anyway *)
test2args[temperamentSum, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{7, 4, 3}}, "contra"}];
test2args[temperamentDiff, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{-3, -4, 3}}, "contra"}];

(* LA only: non-min-grade-1 *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
flattoneM = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
godzillaM = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
et19MwithIndependent7 = {{{19, 30, 44, 0}, {0, 0, 0, 1}}, "co"};
test2args[temperamentSum, septimalMeantoneM, flattoneM, godzillaM];
test2args[temperamentDiff, septimalMeantoneM, flattoneM, et19MwithIndependent7];

(* LA only: ensure the minors are consulted so that the sum and difference are identified correctly *)
t1 = {{{0, 1, 4}}, "co"};
t2 = {{{5, -6, -2}}, "co"};
tSum = {{{5, -5, 2}}, "co"};
tDiff = {{{5, -7, -6}}, "co"};
test2args[temperamentSum, t1, t2, tSum];
test2args[temperamentDiff, t1, t2, tDiff];

(* LA only: an example that makes sure that even if the input matrices explicitly share the vector, it still works *)
t1 = {{{-3, 2, 0, 0}, {-2, 0, 0, 1}}, "contra"};
t2 = {{{-3, 2, 0, 0}, {-4, 1, 1, 0}}, "contra"};
test2args[temperamentSum, t1, t2, {{{-3, 2, 0, 0}, {-6, 1, 1, 1}}, "contra"}];
test2args[temperamentDiff, t1, t2, {{{-3, 2, 0, 0}, {-1, 1, -1, 1}}, "contra"}];

(* LA only: an example that was intractable with the breadth-first search of linear combinations code the first way I wrote it, but is tractable using my fancier style essentially using a Wolfram Solve[]*)
t1 = {{{5, -1, -4, 9, -3}, {0, -7, -1, -8, -2}}, "co"};
t2 = {{{5, -1, -4, 9, -3}, {-5, 2, -4, -3, -9}}, "co"};
test2args[temperamentSum, t1, t2, {{{5, 7, -11, 23, -13}, {0, 8, -7, 14, -10}}, "co"}];
test2args[temperamentDiff, t1, t2, {{{5, 5, 5, 11, 11}, {0, 6, 9, 2, 14}}, "co"}];



Print["TOTAL FAILURES: ", f];
Print["TOTAL PASSES: ", p];
