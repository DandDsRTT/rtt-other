removeAllZeroRows[a_] := Select[a, FreeQ[#, {0 ..}] &];

Do[
  a = Table[Table[RandomInteger[{-2,2}],3],3];
  rank1 = Length[removeAllZeroRows[Last[HermiteDecomposition[a]]]];
  rank2 = Length[removeAllZeroRows[Last[HermiteDecomposition[Transpose[a]]]]];
  Print[rank1, rank2, rank1 == rank2],
  100
];
