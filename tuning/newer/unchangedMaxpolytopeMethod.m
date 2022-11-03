M = {{12, 19, 28}};
j = 1200 * Log2[{{2, 3, 5}}];
K = {{1, 1, 0}};
T = IdentityMatrix[3];
W = IdentityMatrix[3];

LinearSolve[K.Transpose[M.T.W], K.Transpose[j.T.W]];
N[First[Transpose[%]]]

M = {{1, 1, 1, 2}, {0, 2, 3, 2}, {0, 0, 2, 1}};
j = 1200 * Log2[{{2, 3, 5, 7}}];
K = {{0, 1, 1, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, -1, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, -1, 0, 0, 0, 0}};
T = Transpose[{{1, 1, -1, 0}, {0, 0, -1, 1}, {3, 0, -1, 0}, {0, 2, -1, 0}, {-1, -1, 0, 1}, {2, -1, 0, 0}, {-1, 1, 0, 0}, {3, 0, 0, -1}, {0, 2, 0, -1}, {-3, 2, 0, 0}}];
W = DiagonalMatrix[Log2[{30, 35, 40, 45, 42, 12, 6, 56, 63, 72}]];

LinearSolve[K.Transpose[M.T.W], K.Transpose[j.T.W]];
N[First[Transpose[%]]]

M = {{1, 1, 1, 2}, {0, 2, 3, 2}, {0, 0, 2, 1}};
j = 1200 * Log2[{{2, 3, 5, 7}}];
K = {{0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1}}; (* we've replaced the last row with an equation that enforces the unchanged interval, and extended the length of each other row by 1 *)
T = Transpose[{{1, 1, -1, 0}, {0, 0, -1, 1}, {3, 0, -1, 0}, {0, 2, -1, 0}, {-1, -1, 0, 1}, {2, -1, 0, 0}, {-1, 1, 0, 0}, {3, 0, 0, -1}, {0, 2, 0, -1}, {-3, 2, 0, 0}}];
W = (*IdentityMatrix[11];*) DiagonalMatrix[Log2[{30, 35, 40, 45, 42, 12, 6, 56, 63, 72, 2}]]; (*the newly added final entry has to be 2 for the Log2 part to send it to 1; the point is, though: don't weight it... or does it matter? ah ha, I guess anything as long as it's not 0, haha, because it'll scale it on both sides equally*)
U = Transpose[{{0, -1, 1, 0}}]; (* here's the unchanged interval *)
T = JoinHorizontally[T, U]; (* so we add the unchanged interval to the end of this *)

"K"
    K // N // MatrixForm
    "Transpose[M.T.W]"
    Transpose[M.T.W] // N // MatrixForm
    "K.Transpose[M.T.W] (A)"
    K.Transpose[M.T.W] // N // MatrixForm
    "Transpose[j.T.W]"
    Transpose[j.T.W] // N // MatrixForm
    "K.Transpose[j.T.W] (b)"
    K.Transpose[j.T.W] // N // MatrixForm
    
    LinearSolve[K.Transpose[M.T.W], K.Transpose[j.T.W]];
N[First[Transpose[%]]]
(* and voila! that makes the unchanged interval unchanged. does it make damage tie with the other two things? that shoul be damage to 7/5, 9.5, and 7/6, where 7/6 has opposite damage *)

g = {1200.0014384954493`, 350.9092022300114`, 266.72475538471787`}
    (g.M.Transpose[Part[Transpose[T], 1]] - j.Transpose[Part[Transpose[T], 1]]) * Part[Part[W, 1], 1]
    (g.M.Transpose[Part[Transpose[T], 2]] - j.Transpose[Part[Transpose[T], 2]]) * Part[Part[W, 2], 2]
    (g.M.Transpose[Part[Transpose[T], 3]] - j.Transpose[Part[Transpose[T], 3]]) * Part[Part[W, 3], 3]
    (g.M.Transpose[Part[Transpose[T], 4]] - j.Transpose[Part[Transpose[T], 4]]) * Part[Part[W, 4], 4]
    (g.M.Transpose[Part[Transpose[T], 5]] - j.Transpose[Part[Transpose[T], 5]]) * Part[Part[W, 5], 5]
    (g.M.Transpose[Part[Transpose[T], 6]] - j.Transpose[Part[Transpose[T], 6]]) * Part[Part[W, 6], 6]
    (g.M.Transpose[Part[Transpose[T], 7]] - j.Transpose[Part[Transpose[T], 7]]) * Part[Part[W, 7], 7]
    (g.M.Transpose[Part[Transpose[T], 8]] - j.Transpose[Part[Transpose[T], 8]]) * Part[Part[W, 8], 8]
    (g.M.Transpose[Part[Transpose[T], 9]] - j.Transpose[Part[Transpose[T], 9]]) * Part[Part[W, 9], 9]
    (g.M.Transpose[Part[Transpose[T], 10]] - j.Transpose[Part[Transpose[T], 10]]) * Part[Part[W, 10], 10]
    (g.M.Transpose[Part[Transpose[T], 11]] - j.Transpose[Part[Transpose[T], 11]]) * Part[Part[W, 11], 11]
    (*(g.M.Transpose[Part[Transpose[T],12]] - j.Transpose[Part[Transpose[T],12]])*Part[Part[W,12],12]*)
    (* blah blah blah, yes, it worked!!! *)
    
    T // MatrixForm
    
    
    {{1, 1, 1, 2}, {0, 2, 3, 2}, {0, 0, 2, 1}}.Transpose[{0, -1, 1, 0}]
    350.909 + 2 * 266.725
    
    2 * 266.725
