getCollinearity[t1_, t2_] := Module[{collinearVectors, collinearCovectors},
  collinearVectors = dual[join[t1, t2]];
  collinearCovectors = dual[meet[t1, t2]];

  (*Print[collinearVectors, collinearCovectors]; *)

  If[
    allZeros[getA[collinearCovectors]] || Length[getA[collinearCovectors]] < Length[getM[t1]] - 1,
    If[
      allZeros[getA[collinearVectors]] || Length[getA[collinearVectors]] < Length[getC[t1]] - 1,
      Error,
      collinearVectors
    ],
    collinearCovectors
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

correctOrientation[t1_, t2_, isSum_, t1plus2andT1minus2_] := Module[{t1plus2, t1minus2, t1plus2minors, t1minors, t2minors, doubleCheckT1plus2minors, minorsMatchAsSum},
  t1plus2 = First[t1plus2andT1minus2];
  t1minus2 = Last[t1plus2andT1minus2];

  t1plus2minors = minorsThing[t1plus2];
  t1minors = minorsThing[t1];
  t2minors = minorsThing[t2];
  doubleCheckT1plus2minors = divideOutGcd[t1minors + t2minors];
  minorsMatchAsSum = t1plus2minors == doubleCheckT1plus2minors;

  (*  Print["t1plus2andT1minus2: ", t1plus2andT1minus2," minorsMatchAsSum: ", minorsMatchAsSum, " isSum: ", isSum, " t1plus2minors: ", t1plus2minors,  " t1minors: ", t1minors," t2minors: ", t2minors, " doubleCheckT1plus2minors: ", doubleCheckT1plus2minors];*)

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

(* TODO: rename and i think we have a lto of repetationve contra and grade claculations that should be cleaned up *)
minorsThing[t_] := Module[{contra, grade, minors, normalizingEntry},
  contra = isContra[t];
  grade = If[contra, getN[t], getR[t]];
  minors = divideOutGcd[First[Minors[getA[t], grade]]];
  normalizingEntry = If[contra, trailingEntry[minors], leadingEntry[minors]];

  If[
    normalizingEntry < 0,
    -minors,
    minors
  ]
];

validTemperamentArithmetic[t1_, t2_, collinearity_] := Module[{tContra, collinearityContra, contra, grade, a1, a2, t1plus2, t1minus2, a1final, a2final},
  (*Print["hello non min grade 1"];*)
  (* Print["hello", t1, t2];*)
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

  (*Print["is there really any collinearity????",  collinearity];*)

  a1 = defactorWhileLockingCollinearVectors[t1, collinearity];
  a2 = defactorWhileLockingCollinearVectors[t2, collinearity];

  (*Print["is it arleayd rworng here????", a1,a2, isNegativeOrientationOfTemperamentMatrix[{a1, getV[collinearity]}], isNegativeOrientationOfTemperamentMatrix[{a2, getV[collinearity]}],isNegativeOrientationOfTemperamentMatrix[t1], isNegativeOrientationOfTemperamentMatrix[t2] ];*)
  (* Print["i haven't been albe to understand what these deubg messages are telling me so far. this should be fairly straightforward, though. this is a temperament sum so first, isSum: ", isSum, " and second, what are the orientations of the two results? the first result is negative?: ", isNegativeOrientationOfTemperamentMatrix[{a1, getV[collinearity]}], " the second says is negatve? ", isNegativeOrientationOfTemperamentMatrix[{a2, getV[collinearity]}]];*)

  (*  If[
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
    ]; *)

  (* Print["after correction", a1, a2, a1+a2];*)
  (* Print["what the f", a1, a2, contra, grade, a1 + a2];*)
  (*Print["aw cmon! getV[t1]: ", getV[t1], " getV[collinearity]: ", getV[collinearity], " a1+a2: ", a1+a2, " a1: ", a1, " a2: ", a2, "and the collinearity is: ", collinearity,  " a1-a2: ", a1-a2];*)

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
  (* Print["hello min grade 1", t1, t2];*)
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

temperamentArithmeticOld[t1_, t2_, isSum_] := If[
  getR[t1] != getR[t2] || getD[t1] != getD[t2] ,
  Error,
  Module[{collinearity},
    collinearity = getCollinearity[t1, t2];

    If[
      collinearity === Error,
      Error,
      If[
        getR[t1] == 1 || getN[t1] == 1,
        validMinGradeOneTemperamentArithmeticOld[t1, t2, isSum],
        validTemperamentArithmeticOld[t1, t2, isSum, collinearity]
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

isEnfactored[a_] := hnf[a] != hnf[colHermiteDefactor[a]]; (* TODO: or how about getEnfactoring > 1? might that be faster actually? *)

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

(* TODO: closely related to pernetSteinDefactor; perhaps clean this up *)
getEnfactoring[a_] := Det[Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]]];

filterCollinearVectorCopies[otherVectors_, collinearVectors_] := Module[{filtered},
  filtered = Select[otherVectors, !MemberQ[collinearVectors, #]&];

  Select[filtered, !MemberQ[collinearVectors, -#]&]
];

defactorWhileLockingCollinearVectors[t_, collinearity_] := Module[{tContra, collinearityContra, grade, collinearVectors, a, d, multiples, equations, enfactoring, multiplesCount, result, answer, otherVectors, joinedVectors},
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
  otherVectors = If[collinearityContra, getC[t], getM[t]];
  otherVectors = filterCollinearVectorCopies[otherVectors, collinearVectors];
  joinedVectors = Join[collinearVectors, otherVectors];

  If[Length[joinedVectors] < grade, Throw["were not enough vectors to perform temperament arithmetic"]]; (* TODO: okay so this is the fix of the bug below at least I think so, but maybe it would be even more secure and not even required this thorw if you could do something like multipley each vector in the latter thing if necessary by consecutive prime mnumers to ensure no enfactoring or something but also ensure no matches *)
  a = Take[joinedVectors, grade]; (*TODO: okay there's actually a bug where if the shared vector explicitly apperas in the inputs then when it forms the `a` to be defactored it has duplicate vectors rather than a full representation of the matrix so you need to make it smarter here to maker surue the stuff you join to the collinearVectors here doesn't include exact copies or negatives of the collinear vecotstr and if so try to fix it so that it doesn'*)

  d = getD[t];
  multiplesCount = Length[collinearVectors] ;(* Length[a] - 1 BAPPLES*);
  enfactoring = getEnfactoring[a];
  multiples = Table[Subscript[x, i], {i, multiplesCount}];
  equations = Map[
    Function[
      dIndex,
      Mod[a[[grade]][[dIndex]] + Total[Map[
        Function[multiplesIndex, multiples[[multiplesIndex]] * collinearVectors[[multiplesIndex]][[dIndex]]],
        (*Function[multiplesIndex, multiples[[multiplesIndex]]* a[[multiplesIndex]][[dIndex]]],BAPPLES*)
        Range[multiplesCount]
      ]], enfactoring] == 0
    ],
    Range[d]
  ];

  (*Print["a: ", a," enfactoring: ",  enfactoring, "uuh", collinearVectors, collinearityContra,getC[t],  getM[t] , grade];*)

  (*Print["equations", equations,multiples,"WWWWW", a, "WHAT", a[[grade]],"WHATWHATWHAT", a[[grade]][[1]], "mulitples count", multiplesCount];*)

  result = FindInstance[equations, multiples, Integers];

  answer = Values[Association[result]];
  a[[grade]] = a[[grade]] + getCollinearVectorLinearCombination[a, answer];
  (*Print[a[[grade]]];*)
  a[[grade]] = divideOutGcd[a[[grade]]];
  (*Print[a[[grade]]];*)

  (*Print["enfactoring of whole thing: ", getEnfactoring[a]];*)

  a
];

(* TODO: still sad that I need Minors[] to figure this out... figure out if there's any other way *)
isNegativeOrientationOfTemperamentMatrix[t_] := Module[{contra, grade, minors, normalizingEntry},
  contra = isContra[t];
  grade = If[contra, getN[t], getR[t]];
  minors = First[Minors[getA[t], grade]];
  normalizingEntry = If[contra, trailingEntry[minors], leadingEntry[minors]]; (* TODO: variable functions? *)

  (*Print["need to audit this. t: ", t," contra: " ,contra," minors: ", minors, " grade: ", grade, "normalizing entry: ", normalizingEntry];  *)
  normalizingEntry < 0
];

validTemperamentArithmeticOld[t1_, t2_, isSum_, collinearity_] := Module[{tContra, collinearityContra, contra, grade, a1, a2},
  (*Print["hello old non min grade 1"];*)
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

  (*Print["is it arleayd rworng here????", a1,a2, isNegativeOrientationOfTemperamentMatrix[{a1, getV[collinearity]}], isNegativeOrientationOfTemperamentMatrix[{a2, getV[collinearity]}],isNegativeOrientationOfTemperamentMatrix[t1], isNegativeOrientationOfTemperamentMatrix[t2] ];*)
  (* Print["i haven't been albe to understand what these deubg messages are telling me so far. this should be fairly straightforward, though. this is a temperament sum so first, isSum: ", isSum, " and second, what are the orientations of the two results? the first result is negative?: ", isNegativeOrientationOfTemperamentMatrix[{a1, getV[collinearity]}], " the second says is negatve? ", isNegativeOrientationOfTemperamentMatrix[{a2, getV[collinearity]}]];*)

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

  (* Print["after correction", a1, a2, a1+a2];*)
  (* Print["what the f", a1, a2, contra, grade, a1 + a2];*)
  (* Print["aw cmon! getV[t1]: ", getV[t1], " getV[collinearity]: ", getV[collinearity], " a1+a2: ", a1+a2, " a1: ", a1,Minors[a1,2], " a2: ", a2,Minors[a2,2], "and the collinearity is: ", collinearity];*)

  If[
    getV[t1] == getV[collinearity],
    canonicalForm[{a1 + a2, getV[collinearity]}] ,
    dual[{a1 + a2, getV[collinearity]}]
  ]
];

validMinGradeOneTemperamentArithmeticOld[t1_, t2_, isSum_] := Module[{thing},
  (*Print["hello old min grade 1"];*)
  thing = If[
    isSum,
    If[
      getR[t1] == 1,
      {getM[t1] + getM[t2], "co"},
      {getC[t1] + getC[t2], "contra"},
    ],
    If[
      getR[t1] == 1,
      {getM[t1] - getM[t2], "co"},
      {getC[t1] - getC[t2], "contra"},
    ]
  ];

  If[
    getV[thing] == getV[t1],
    canonicalForm[thing],
    dual[thing]
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
test2args[temperamentSum, {{{3, 8, - 4, - 6}}, "co"}, {{{9, 2, -4, 1}}, "co"}, {{{12, 10, -8, -5}}, "co"}];

(* example that was intractable unless I defactored piecemeal *)
test2args[temperamentSum, {{{-97, 73, 45, 16}}, "contra"}, {{{-1, 8, 9, 3}}, "contra"}, {{{-98, 81, 54, 19}}, "contra"}];

(* example that illuminated how sometimes the matrix approach can't pick which is the sum and which is the difference correctly, although it actually is able to in this case because this is a min-grade-1 temperament; I just needed to rework the code to take a specialized simpler approach in that case which doesn't trigger the problem whereby if the collineairty is on the other side of duality and also the EA dual of the multimap is not equal to the multicomma (before normalizing, that is) or vice versa, then you can't determine (without bringing in significant material from the EA part of the library re: the sign change patterns for the EA dual, which would compromise the entire point of this exercise in accomplishing temperament arithmetic using only LA) what the correct negativity orientation for the summed matrices should be *)
test2args[temperamentSum, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{7, 4, 3}}, "contra"}];
test2args[temperamentDifference, {{{2, 0, 3}}, "contra"}, {{{5, 4, 0}}, "contra"}, {{{-3, -4, 3}}, "contra"}];

(* an example that actually exercises the non-min-grade-1 code! *)
septimalMeantoneM = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
flattoneM = {{{1, 0, -4, 17}, {0, 1, 4, -9}}, "co"};
godzillaM = {{{1, 0, -4, 2}, {0, 2, 8, 1}}, "co"};
test2args[temperamentSum, septimalMeantoneM, flattoneM, godzillaM];
(*septimalMeantoneW = matrixToMultivector[septimalMeantoneM]
flattoneW = matrixToMultivector[flattoneM]
eaTemperamentSum[septimalMeantoneW, flattoneW]*)


(*test2args[temperamentDifference, septimalMeantoneM, flattoneM, {{{19, 30, 44, 0}, {0, 0, 0, 1}}, "co"}];*)

(*TODO: SEE BAPPLES need an example that demonstrates how it's necessary to include all the vectors, not just the collinear ones *)

(*TODO: need an example that was intractable with the breadth-first search of linear combinations code the first way I wrote it, but is tractable using my fancier style essentially using a Wolfram Solve[] *)

(* an example of a d=5 min-grade=2 noncolinearity=2 example that should error even though it's collinear *)
test2args[temperamentSum, {{{1, 1, 0, 30, -19}, {0, 0, 1, 6, -4}, {0, 0, 0, 41, -27}}, "co"}, {{{2, 0, 19, 45, 16}, {0, 1, 19, 55, 18}, {0, 0, 24, 70, 23}}, "co"}, Error];

(* an example that fails without using the minors to check your work... well except unforutnatley this is min-grade 1 so it doesn't use the code that needs to be tested and revised by this*)
(*t1 ={{{-25,28,0},{-18,20,1}},"contra"}; (*multivectorToMatrix[ {{4,-25,28},2,"contra"} ]*)t2 = {{{-5,4,0},{4,1,5}},"contra"}; (* multivectorToMatrix[{{-21,-25,20},2,"contra"}] *)t1plus2 = {{{-25,24,0},{-17,17,2}},"contra"}; (* multivectorToMatrix[ {{-17,-50,48},2,"contra"} ]*)t1minus2 ={{{0,1,0},{-25,0,8}},"contra"}; (* multivectorToMatrix[{{25,0,8},2,"contra"}] *)test2args[temperamentSum, t1, t2, t1plus2];test2args[temperamentDifference,t1, t2, t1minus2];*)

(* wth is even happening right now ... okay maybe actually this is some crazy example of somethig that causes the FindInstance[] strategy to fail, like it's accpeting a 0 where it never should or something liek that... t1plus2: {{{0,6,0,-2},{0,-6,0,2}},co} t1plus2minors: {0,6,0,-2} t1minors + t2minors: {9,19,27,-2,0,6} t1minors: {5,16,15,-1,0,3} t2minors: {4,3,12,-1,0,3} a1: {{0,3,0,-1},{0,-3,0,1}} a2: {{0,3,0,-1},{0,-3,0,1}} *)
(* w1 = {{5,16,15,-1,0,3},2,"contra"} ;
t1 = multivectorToMatrix[w1]
w2 = {{4,3,12,-1,0,3},2,"contra"} ;
t2  = multivectorToMatrix[w2]
temperamentSum[t1,t2]
eaTemperamentSum[w1,w2] *)

(* okay, let me try again to come up with an example that fails without using the minors to check your work... okay this is testing it in the min grade on  case, but you'll need to add an example for non min grade one too *)
t1 = {{{1, 0, 0, 0}, {0, 1, -1, 0}, {0, 0, 0, 1}}, "co"}; (*multivectorToMatrix[{{0,1,-1,0},3,"co"} ]*)
t2 = {{{1, 3, 0, -16}, {0, 4, 3, -39}, {0, 0, 5, -36}}, "co"}; (* multivectorToMatrix[{{20,-144,87,-59},3,"co"} ] *)
expectedSum = {{{1, 0, 7, -53}, {0, 1, 18, -133}, {0, 0, 20, -143}}, "co"}; (*multivectorToMatrix[{{20,-143,86,-59},3,"co"}]*)
expectedDifference = {{{1, 2, 1, -19}, {0, 5, 0, -22}, {0, 0, 4, -29}}, "co"}; (*multivectorToMatrix[{{20,-145,88,-59},3,"co"}]*)
test2args[temperamentSum, t1, t2, expectedSum];
test2args[temperamentDifference, t1, t2, expectedDifference];

(*okay, what the hell is this now then...? I was just trying to find the aforementioned example for non min grade one but which verifies the need for this new way of checking things with the minors as double-check ... this might be an example but I'm not sure. nope, it's not failing the old way. so I need to keep looking for an example that will actualy test-cover that.*)
(*t1 = {{{1,9,9,19},{0,18,18,49}},"co"}; (*canonicalForm[{{{-3,-9,-9,-8},{5,9,9,-3}},"co"}]*)
t2={{{1,9,9,19},{0,18,18,49}},"co"}; (*canonicalForm[{{{-3,-9,-9,-8},{8,9,-6,-8}},"co"} ] *)
test2args[temperamentSum, t1, t2, {{{1,9,9,19},{0,18,18,49}},"co"}];*)
(* and i think this one was just because of the bug, that I'm trying to fix now
t1 = {{{3,2,8,2},{0,5,31,10}},"co"} ;
t2 = {{{1,22,32,0},{0,32,44,-1}},"co"};
test2args[temperamentSum, t1, t2, {}];*)
t1 = {{{-3, 2, 0, 0}, {-2, 0, 0, 1}}, "contra"};
t2 = {{{-3, 2, 0, 0}, {-4, 1, 1, 0}}, "contra"};
test2args[temperamentSum, t1, t2, {{{-3, 2, 0, 0}, {-6, 1, 1, 1}}, "contra"}];
test2args[temperamentDifference, t1, t2, {{{-3, 2, 0, 0}, {-1, 1, -1, 1}}, "contra"}];

Print["TOTAL FAILURES: ", f];
