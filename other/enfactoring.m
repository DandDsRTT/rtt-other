(* closely related to pernetSteinDefactor *)
getEnfactoring[a_] := Det[Transpose[Take[hnf[Transpose[a]], MatrixRank[a]]]];
