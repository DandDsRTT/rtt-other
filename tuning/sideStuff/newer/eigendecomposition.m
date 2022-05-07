getProjectionAFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[
  {commaEigenvectors, eigenvectors, diagonalEigenvalueA},
  
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueA = getDiagonalEigenvalueA[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueA.Inverse[eigenvectors]]
];

getDiagonalEigenvalueA[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];

(* getProjectionAFromUnchangedIntervals *)
test[getProjectionAFromUnchangedIntervals, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{1, 0, 0}, {-2, 0, 1}}, {{1, 1, 0}, {0, 0, 0}, {0,FractionBox["1", "4"], 1}}];

(* getDiagonalEigenvalueA *)
test[getDiagonalEigenvalueA, {{1, 0, 0}, {-2, 0, 1}}, {{-4, 4, -1}}, {{1, 0, 0}, {0, 1, 0}, {0, 0, 0}}];
