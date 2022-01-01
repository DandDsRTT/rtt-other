snf[a_] := Part[SmithDecomposition[a], 2];

rref[a_] := RowReduce[a];

irref[a_] := Map[multByLcd, rref[a]];

rightReducingMatrix[a_] := Last[SmithDecomposition[a]];
smithDefactor[a_] := Take[Inverse[rightReducingMatrix[a]], MatrixRank[a]];

pernetSteinDefactor[m_] := Inverse[getGreatestFactorA[m]].m;



(* TESTS *)

(* snf *)
test[snf, {{1, 1, 0, -3}, {0, 3, 12, 30}}, {{1, 0, 0, 0}, {0, 3, 0, 0}}];

(* rref *)
test[rref, {{7, 11, 16}, {22, 35, 51}}, {{1, 0, -1 / 3}, {0, 1, 5 / 3}}];

(* irref *)
test[irref, {{7, 11, 16}, {22, 35, 51}}, {{3, 0, -1}, {0, 3, 5}}];

(* rightReducingMatrix *)
test[rightReducingMatrix, {{6, 5, -4}, {4, -4, 1}}, {{1, -2, 1}, {1, 1, 2}, {2, 1, 4}}];

(* smithDefactor *)
test[smithDefactor, {{6, 5, -4}, {4, -4, 1}}, {{2, 9, -5}, {0, 2, -1}}];
