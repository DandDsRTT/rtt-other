X = Transpose[{{0, 1, 0}, {0, -2, 1}}](*9/7 and 245/243, in the 2.9/7.5/3 subgroup *)
S = Transpose[{{1 , 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}] (*transformation from 2.9/7.5/3 -> 2.3.5.7 *)

S.X


X = Transpose[{{4, -4, 1}}]
S = Transpose[{{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}] (* transformation from 2.3.5 -> 2.3.5.7 *)
S.X


X = Transpose[{{-6, 1, 1}}]
S = Transpose[{{1, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 0, 1}}]

S.X
