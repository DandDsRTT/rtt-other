{{4.907, 5.129, 10.644, 5.492, 0, 3.585, 0, 5.807, 0, -6.170 }, {-4.907, -5.129, -15.966, 5.492, 0, -7.170, 5.170, -11.615, 11.955, 24.680}, {-9.814, -5.129, -10.644, -10.984, 5.392, 0, 0, -5.807, -5.977, 0}}.Transpose[{{0, 1, 1, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, -1, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, -1, 0, 0, 0, 0}}] // MatrixForm

Unprotect[C];
M = {{1, 1, 1, 2}, {0, 2, 3, 2}, {0, 0, 2, 1}};
T = Transpose[{{1, 1, -1, 0}, {0, 0, -1, 1}, {3, 0, -1, 0}, {0, 2, -1, 0}, {-1, -1, 0, 1}, {2, -1, 0, 0}, {-1, 1, 0, 0}, {3, 0, 0, -1}, {0, 2, 0, -1}, {-3, 2, 0, 0}}];
C = DiagonalMatrix[Log2[{30, 35, 40, 45, 42, 12, 6, 56, 63, 72}]];
K = Transpose[{{0, 1, 1, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, -1, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, -1, 0, 0, 0, 0}}];


Log[35] / Log[2] // N
Log2[35] // N
Log2[45] - Log2[35] // N
Log2[9 / 7] // N

M.T.C // MatrixForm

M.T.C.K // MatrixForm
M.T.C.K // N // MatrixForm

Log2[56000] // N
Log2[9 / 7] // N
Log2[35 / 12] // N
35 * 40^3
Log2[144 / 35] // N
Log2[45^2 / 35] // N
Log2[35]

T.C // MatrixForm

T.C.K // MatrixForm
T.C.K // N // MatrixForm

Simplify[Inverse[M.T.C.K]] // MatrixForm
Simplify[Inverse[M.T.C.K]] * Denominator[First[First[Simplify[Inverse[M.T.C.K]] ]]] // MatrixForm

Numerator[First[First[Simplify[Inverse[M.T.C.K]] ]]]
% // N

Log[35] * Log[45] // N



Log[35] / Log[2] // N
Log2[35] // N

Numerator[First[First[Simplify[Inverse[M.T.C.K]] ]]]
% // N
Denominator[First[First[Simplify[Inverse[M.T.C.K]] ]]]
% // N

Log[35]Log[45] // N
Log2[35]Log2[45]Log[2]Log[2] // N


Log2[E] // N

fsk = Log[2];
fsk * Log2[2](3 * fsk * Log2[35] * fsk * Log2[45] - 2 * fsk * Log2[12] * fsk * Log2[405 / 7]) // N
fsk * fsk * fsk * (3Log2[35]Log2[45] - 2Log2[12]Log2[405 / 7]) // N

fsk * fsk * fsk * (Log2[9 / 8]Log2[12]Log2[35] + Log2[40]Log2[45]Log2[20736 / 35]) // N

(3Log2[35]Log2[45] - 2Log2[12]Log2[405 / 7]) / (Log2[9 / 8]Log2[12]Log2[35] + Log2[40]Log2[45]Log2[20736 / 35]) // N



Log[35]Log[45] // N
Log[35 * 45] // N

35 * 40 * 40

thing = Simplify[Inverse[M.T.C.K]];
denom = Denominator[First[First[thing ]]];
wereallywant = Simplify[Inverse[M.T.C.K]] * Denominator[First[First[Simplify[Inverse[M.T.C.K]] ]]];
result = Simplify[T.C.K.wereallywant];

Dimensions[result]
result // MatrixForm
result / denom // N // MatrixForm

Part[Part[result], 4, 3]

2^10.1559 * 3^1.83119 * 5^3.66237 * 7^-7.32475

2^Part[Part[result / denom], 1, 1] * 3^Part[Part[result / denom], 2, 1] * 5^Part[Part[result / denom], 3, 1] * 7^Part[Part[result / denom], 4, 1] // N

Log2[%] * 1200 // N
    
    
    
    Unprotect[C];
M = {{1, 1, 1, 2}, {0, 2, 3, 2}, {0, 0, 2, 1}};
T = Transpose[{{1, 1, -1, 0}, {0, 0, -1, 1}, {3, 0, -1, 0}, {0, 2, -1, 0}, {-1, -1, 0, 1}, {2, -1, 0, 0}, {-1, 1, 0, 0}, {3, 0, 0, -1}, {0, 2, 0, -1}, {-3, 2, 0, 0}, {0, -1, 1, 0}}];
C = DiagonalMatrix[Log2[{30, 35, 40, 45, 42, 12, 6, 56, 63, 72, 2}]];
K = Transpose[{{0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1}}];

M.T.C // MatrixForm
    
    M.T.C.K // MatrixForm
    
    T.C.K // MatrixForm
    T.C.K // N // MatrixForm
    
    T.C.K.Inverse[M.T.C.K] * Det[M.T.C.K] // N // MatrixForm
    
    T.C.K.Inverse[M.T.C.K] // N // MatrixForm
    
    T.C.K // N // MatrixForm
    
    Inverse[M.T.C.K] * Det[M.T.C.K] // N // MatrixForm
    Det[M.T.C.K] // N
    
    j = 1200 * Log2[{2, 3, 5, 7}];
j.T.C.K.Inverse[M.T.C.K] // N // MatrixForm
    
    M = {{1, 2, 3}, {0, -3, -5}};
Linv = IdentityMatrix[3];(*Inverse[DiagonalMatrix[{2,3,5}]];*)
K = Transpose[{{1, 1, 0}, {1, 0, 1}}];
M.Linv.K // MatrixForm
    Linv.K // MatrixForm
    Simplify[Det[M.Linv.K]]
    Inverse[M.Linv.K] *% // MatrixForm
    Linv.K.Inverse[M.Linv.K] // N // MatrixForm
    
    2^0.489542 * 3^2.55229 * 5^-1.53137
    Log2[%] * 1200
    
    Inverse[M.Linv.K] // MatrixForm
    
    Linv.K.Inverse[M.Linv.K] // MatrixForm
    
    3^5 * 5^-3
    (*% // N*)
    Log2[%] * 1200 // N
    
    MSp = {{1 / Log2[2], 2 / Log2[3], 3 / Log2[5], 0}, {0, -3 / Log2[3], -5 / Log2[5], 0}, {1, 1, 1, -1}};
K = Transpose[{{1, 1, 0, 0}, {1, 0, 1, 0}, {1, 0, 0, 1}}];
Sp = Inverse[DiagonalMatrix[Log2[{2, 3, 5, 2}]]];
j = 1200 * {{Log2[2], Log2[3], Log2[5], 0}};

MSp.K // MatrixForm // N
    Sp.K // MatrixForm
    
    G = Sp.K.Inverse[MSp.K]
    
    j.G // N
    
    
    Sp // MatrixForm
    
    Inverse[MSp.K] // MatrixForm // N
    
    G // MatrixForm // N
    
    j.G // N
    
    MSp = {{1 / Log2[2], 2 / Log2[3], 3 / Log2[5], 0, 1}, {0, -3 / Log2[3], -5 / Log2[5], 0, 0}, {1, 1, 1, -1, 0}};
K = Transpose[{{1, 1, 0, 0, 0}, {1, 0, 1, 0, 0}, {0, 0, 0, 0, 1}}];
Sp = Inverse[DiagonalMatrix[{Log2[2], Log2[3], Log2[5], 1, 1}]];
j = 1200 * {{Log2[2], Log2[3], Log2[5], 0, 1}};
G = Sp.K.Inverse[MSp.K];

Sp // MatrixForm
    
    MSp.K // MatrixForm // N
    
    Sp.K // MatrixForm
    
    Inverse[MSp.K] // MatrixForm // N
    
    G // MatrixForm // N
    
    j.G // MatrixForm // N

