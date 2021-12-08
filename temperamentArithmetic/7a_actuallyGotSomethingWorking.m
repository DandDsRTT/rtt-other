getCollinearityNumber[collinearity_] := Length[removeAllZeroRows[getA[collinearity]]];

isMonononcollinear[collinearity_, t_] := If[
  isContra[collinearity],
  getCollinearityNumber[collinearity] === getN[t] - 1,
  getCollinearityNumber[collinearity] === getR[t] - 1
];

(* TODO: argh, "collinearity" is really the number, I would say. this is like a collinearity matrix, so collinearityA, well except it's actually a T, which is confusing. it's a collinearityVariancedMatrix *)
getCollinearity[t1_, t2_] := Module[{covariantCollinearity, contravariantCollinearity},
  covariantCollinearity = dual[join[t1, t2]];
  contravariantCollinearity = dual[meet[t1, t2]];

  If[
    isMonononcollinear[contravariantCollinearity, t1],
    contravariantCollinearity,
    If[
      isMonononcollinear[covariantCollinearity, t1],
      covariantCollinearity,
      Error
    ]
  ]
];

temperamentArithmetic[t1_, t2_, isSum_] := If[
  getR[t1] != getR[t2] || getD[t1] != getD[t2] ,
  Error,
  Module[{collinearity, t1plus2andT1minus2},
    collinearity = getCollinearity[t1, t2];
    t1plus2andT1minus2 = If[
      collinearity === Error,
      Error,
      If[
        getR[t1] == 1 || getN[t1] == 1,
        validMinGradeOneTemperamentArithmetic[t1, t2],
        validTemperamentArithmetic[t1, t2, collinearity]
      ]
    ];

    If[
      t1plus2andT1minus2 === Error,
      Error,
      correctOrientation[t1, t2, isSum, t1plus2andT1minus2]
    ]
  ]
];

correctOrientation[t1_, t2_, isSum_, t1plus2andT1minus2_] := Module[
  {
    t1plus2,
    t1minus2,
    t1plus2minors,
    t1minors,
    t2minors,
    doubleCheckT1plus2minors,
    minorsMatchAsSum
  },

  t1plus2 = First[t1plus2andT1minus2];
  t1minus2 = Last[t1plus2andT1minus2];

  t1plus2minors = laGetMinors[t1plus2];
  t1minors = laGetMinors[t1];
  t2minors = laGetMinors[t2];
  doubleCheckT1plus2minors = divideOutGcd[t1minors + t2minors];
  minorsMatchAsSum = t1plus2minors == doubleCheckT1plus2minors;

  If[
    isSum,
    If[
      minorsMatchAsSum,
      t1plus2,
      t1minus2
    ],
    If[
      minorsMatchAsSum,
      t1minus2,
      t1plus2
    ]
  ]
];

laGetMinors[t_] := Module[{contra, grade, minors, normalizingEntry},
  contra = isContra[t];
  grade = If[contra, getN[t], getR[t]];
  minors = divideOutGcd[First[Minors[getA[t], grade]]];
  (* TODO: variable functions? then I could rewrite this to call the variable function on minors always *)
  normalizingEntry = If[contra, trailingEntry[minors], leadingEntry[minors]];

  If[
    normalizingEntry < 0,
    -minors,
    minors
  ]
];

getGradeMatchingCollinearity[t_, collinearity_] := Module[{tContra, collinearityContra},
  tContra = isContra[t];
  collinearityContra = isContra[collinearity];

  If[
    tContra == collinearityContra,
    If[ tContra, getN[t], getR[t] ],
    If[ tContra, getR[t], getN[t] ]
  ]
];

validTemperamentArithmetic[t1_, t2_, collinearity_] := Module[
  {
    grade,
    a1,
    a2,
    t1plus2,
    t1minus2,
    a1final,
    a2final
  },

  grade = getGradeMatchingCollinearity[t1, collinearity];

  a1 = defactorWhileLockingCollinearVectors[t1, collinearity];
  a2 = defactorWhileLockingCollinearVectors[t2, collinearity];

  a1final = Last[a1];
  a2final = Last[a2];

  t1plus2 = If[
    getV[t1] == getV[collinearity],
    canonicalForm[{Join[getA[collinearity], {a1final + a2final}], getV[collinearity]}],
    dual[{Join[getA[collinearity], { a1final + a2final}], getV[collinearity]}]
  ];
  t1minus2 = If[
    getV[t1] == getV[collinearity],
    canonicalForm[{Join[getA[collinearity], { a1final - a2final}], getV[collinearity]}],
    dual[{Join[getA[collinearity], {a1final - a2final}], getV[collinearity]}]
  ];

  {t1plus2, t1minus2}
];

validMinGradeOneTemperamentArithmetic[t1_, t2_] := Module[{t1plus2, t1minus2},
  t1plus2 = If[
    getR[t1] == 1,
    {getM[t1] + getM[t2], "co"},
    {getC[t1] + getC[t2], "contra"}
  ];
  t1minus2 = If[
    getR[t1] == 1,
    {getM[t1] - getM[t2], "co"},
    {getC[t1] - getC[t2], "contra"}
  ];

  t1plus2 = If[
    getV[t1plus2] == getV[t1],
    canonicalForm[t1plus2],
    dual[t1plus2]
  ];
  t1minus2 = If[
    getV[t1minus2] == getV[t1],
    canonicalForm[t1minus2],
    dual[t1minus2]
  ];

  {t1plus2, t1minus2}
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

(* TODO: closely related to pernetSteinDefactor; perhaps clean this up *)
getEnfactoring[a_] := Det[Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]]];

filterCollinearVectorCopies[otherVectors_, collinearVectors_] := Module[{filtered},
  filtered = Select[otherVectors, !MemberQ[collinearVectors, #]&];

  Select[filtered, !MemberQ[collinearVectors, -#]&]
];

defactorWhileLockingCollinearVectors[t_, collinearity_] := Module[
  {
    grade,
    collinearVectors,
    otherVectors,
    joinedVectors,
    a,
    d,
    multiples,
    equations,
    enfactoring,
    multiplesCount,
    result
  },

  grade = getGradeMatchingCollinearity[t, collinearity];
  collinearVectors = getA[collinearity];
  otherVectors = If[isContra[collinearity], getC[t], getM[t]];
  otherVectors = filterCollinearVectorCopies[otherVectors, collinearVectors];
  joinedVectors = Join[collinearVectors, otherVectors];

  (* TODO: maybe it would be even more secure and not even required this throw if you could do something like
  multiply each vector in the latter thing if necessary by consecutive prime numbers
  to ensure no enfactoring or something but also ensure no matches *)
  If[Length[joinedVectors] < grade, Throw["were not enough vectors to perform temperament arithmetic"]];
  a = Take[joinedVectors, grade];

  d = getD[t];
  multiplesCount = Length[collinearVectors];
  enfactoring = getEnfactoring[a];
  multiples = Table[Subscript[x, i], {i, multiplesCount}];
  equations = Map[
    Function[
      dIndex,
      Mod[a[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * collinearVectors[[multiplesIndex]][[dIndex]]],
        Range[multiplesCount]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];

  result = Values[Association[FindInstance[equations, multiples, Integers]]];

  a[[grade]] = a[[grade]] + getCollinearVectorLinearCombination[a, result];
  a[[grade]] = divideOutGcd[a[[grade]]];

  a
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
et12M = {{{12, 19, 28, 34}}, "co"};
et19M = {{{19, 30, 44, 53}}, "co"};
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

(* example of collinear, but not monononcollinear: d = 5, min-grade = 2, noncollinearity = 2 *)
t1 = {{{1, 1, 0, 30, -19}, {0, 0, 1, 6, -4}, {0, 0, 0, 41, -27}}, "co"};
t2 = {{{2, 0, 19, 45, 16}, {0, 1, 19, 55, 18}, {0, 0, 24, 70, 23}}, "co"};
test2args[temperamentSum, t1, t2, Error];
test2args[temperamentDifference, t1, t2, Error];

(* example of monononcollinear, but not collinear: d = 2, min-grade = 1, noncollinearity = 1 *)
t1 = {{{2, 3}}, "contra"};
t2 = {{{4, -7}}, "co"};
t1plus2 = {{{9, 7}}, "contra"};
t1minus2 = {{{5, 1}}, "contra"};
test2args[temperamentSum, t1, t2, t1plus2];
test2args[temperamentDifference, t1, t2, t1minus2];

(* LA only: example that required the breadth-first search of linear combinations of multiple collinear vectors *)
test2args[temperamentSum, {{{3, 8, - 4, - 6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];

(* LA only: example that was intractable unless I defactored piecemeal *)
test2args[temperamentSum, {{{-97, 73, 45, 16}}, "contra"}, {{{-1, 8, 9, 3}}, "contra"}, {{{-98, 81, 54, 19}}, "contra"}];

(* LA only: example that motivated the existence of the special min-grade-1 path *)
test2args[temperamentSum, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{7, 4, 3}}, "contra"}];
test2args[temperamentDifference, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{-3, -4, 3}}, "contra"}];

(* LA only: an example that actually exercises the non-min-grade-1 code! *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
flattoneM = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
godzillaM = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
et19MwithIndependent7 = {{{19, 30, 44, 0}, {0, 0, 0, 1}}, "co"};
test2args[temperamentSum, septimalMeantoneM, flattoneM, godzillaM];
test2args[temperamentDifference, septimalMeantoneM, flattoneM, et19MwithIndependent7];

(* LA only: ensure the minors are consulted so that the sum and difference are identified correctly *)
(* this verifies that for the min-grade-1 case *)
t1 = {{{-25, 28, 0}, {-18, 20, 1}}, "contra"};
t2 = {{{-5, 4, 0}, {4, 1, 5}}, "contra"};
t1plus2 = {{{-25, 24, 0}, {-17, 17, 2}}, "contra"};
t1minus2 = {{{0, 1, 0}, {-25, 0, 8}}, "contra"};
test2args[temperamentSum, t1, t2, t1plus2];
test2args[temperamentDifference, t1, t2, t1minus2];
(* this also verifies that for the min-grade-1 case, I think *)
t1 = {{{1, 0, 0, 0}, {0, 1, -1, 0}, {0, 0, 0, 1}}, "co"};
t2 = {{{1, 3, 0, -16}, {0, 4, 3, -39}, {0, 0, 5, -36}}, "co"};
t1plus2 = {{{1, 0, 7, -53}, {0, 1, 18, -133}, {0, 0, 20, -143}}, "co"};
t1minus2 = {{{1, 2, 1, -19}, {0, 5, 0, -22}, {0, 0, 4, -29}}, "co"};
test2args[temperamentSum, t1, t2, t1plus2];
test2args[temperamentDifference, t1, t2, t1minus2];
(* TODO: verify that these actually fail when you remove the orientation correction code, then find a non-min-grade-1 case... I feel like I did this before but stuff got lost in the mix *)

(* LA only: an example that makes sure that even if the input matrices explicitly share the vector, it still works *)
t1 = {{{-3, 2, 0, 0}, {-2, 0, 0, 1}}, "contra"};
t2 = {{{-3, 2, 0, 0}, {-4, 1, 1, 0}}, "contra"};
test2args[temperamentSum, t1, t2, {{{-3, 2, 0, 0}, {-6, 1, 1, 1}}, "contra"}];
test2args[temperamentDifference, t1, t2, {{{-3, 2, 0, 0}, {-1, 1, -1, 1}}, "contra"}];

(* LA only: an example that was intractable with the breadth-first search of linear combinations code the first way I wrote it, but is tractable using my fancier style essentially using a Wolfram Solve[]*)
t1 = {{{5, -1, -4, 9, -3}, {0, -7, -1, -8, -2}}, "co"};
t2 = {{{5, -1, -4, 9, -3}, {-5, 2, -4, -3, -9}}, "co"};
test2args[temperamentSum, t1, t2, {{{5, 7, -11, 23, -13}, {0, 8, -7, 14, -10}}, "co"}];
test2args[temperamentDifference, t1, t2, {{{5, 5, 5, 11, 11}, {0, 6, 9, 2, 14}}, "co"}]; (*TODO: test multivector these *)



Print["TOTAL FAILURES: ", f];
