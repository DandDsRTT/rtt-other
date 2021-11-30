(* closely related to pernetSteinDefactor *)
getEnfactoring[m_] := Det[Transpose[Take[hnf[Transpose[m]], MatrixRank[m]]]];
