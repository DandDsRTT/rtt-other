Unprotect[C];
M = {{1, 1, 0}, {0, 1, 4}};
W = DiagonalMatrix[Map[1 / #&, j]];(*IdentityMatrix[3];*)
j = Log2[{2, 3, 5}];
V = (*Transpose[{{-2,0,1}}];*)(*  Transpose[{{-1,1,0}}];  *) Transpose[{{1, 0, 0}}];
"A: "
A = Transpose[M.W]
"C: "
C = Transpose[M.V]
"b: "
b = Transpose[j.W]
"d: "
d = Transpose[j.V]

(* seems like there's way too much unnecessary transposition. but let's wait to correct for that until after I've got a working solution, and can walk back to the variables Dave and I would use *)

(* this is Sintel's explanation of the constrained case https://en.xen.wiki/w/User:Sintel/Generator_optimization#Constraints but w/o the C and d parts i.e. w/o the actual constraints, just close to the normal pseudoinverse method for optimization *)
1200 * N[Inverse[Transpose[A].A].Transpose[A].b]
(* and this is it confirmed to be the pseudoinverse *)
1200 * N[PseudoInverse[A].b]

(* and this is changing it to our preferred variables, orientations, and factorings *)
bold1 = {{1, 1, 1}};
L = DiagonalMatrix[j];
T = IdentityMatrix[3];
N[First[1200 * bold1.L].W.T.PseudoInverse[M.T.W]]

(* defining some helper functions and checking that they work as expected *)

x = {{1, 1, 1}, {2, 2, 2}}
y = {{3, 3, 3}}
Join[x, y]

z = {{1}, {2}}
JoinHorizontally[a1_, a2_] := Transpose[Join[Transpose[a1], Transpose[a2]]]
JoinHorizontally[x, z]

ZeroMatrix[n_] := ConstantArray[0, {n, n}]
ZeroMatrix[2]

(*this is Sintel's explanation of the constrained case again, but this time in full, with the constraints *)

toBeInverted = Join[
  JoinHorizontally[
    Transpose[A].A,
    Transpose[C]
  ],
  JoinHorizontally[
    C,
    ZeroMatrix[1]
  ]
];
"toBeInverted: "
toBeInverted // MatrixForm

otherMatrix = Transpose[Join[
  Transpose[A].b,
  d
]];
"otherMatrix: "
otherMatrix // MatrixForm

1200 * N[Take[Inverse[toBeInverted].otherMatrix, 2]]

(* checking it worked; these two should be the same *)
First[1200 * bold1.L.V] == %.M.V

(* and this is changing the full, constrained case over to our preferred variables, orientations, and factorings *)

U = V;

toBeInverted = Join[
  JoinHorizontally[
    M.T.W.Transpose[M.T.W],
    M.U
  ],
  JoinHorizontally[
    Transpose[M.U],
    ZeroMatrix[1]
  ]
];
"toBeInverted: "
toBeInverted // MatrixForm

otherMatrix = 1200 * bold1.L.JoinHorizontally[
  T.W.Transpose[M.T.W],
  U
]
"otherMatrix: "
otherMatrix // MatrixForm

N[Take[First[otherMatrix.Inverse[toBeInverted]], 2]]

(* checking it worked; these two should be the same *)
First[1200 * bold1.L.V] == %.M.V

(* so here it is again w/o constraint *)
N[First[1200 * bold1.L].W.T.PseudoInverse[M.T.W]]
(* and w/ *)
N[Take[First[otherMatrix.Inverse[toBeInverted]], 2]]

(* and now we'll try to make them as similar as possible *)
N[Take[First[1200 * bold1.L].
    JoinHorizontally[
      T.W.Transpose[M.T.W],
      {}
    ].Inverse[Join[
  JoinHorizontally[
    M.T.W.Transpose[M.T.W],
    {}
  ],
  JoinHorizontally[
    {},
    {}
  ]
]], 2]]
(* and w/ *)
N[Take[First[1200 * bold1.L].
    JoinHorizontally[
      T.W.Transpose[M.T.W],
      U
    ].Inverse[Join[
  JoinHorizontally[
    M.T.W.Transpose[M.T.W],
    M.U
  ],
  JoinHorizontally[
    Transpose[M.U],
    ZeroMatrix[1]
  ]
]], 2]]

(* so if the formula is G = TW(MTW)⁺ = TW(MTW)ᵀ(MTW(MTW)ᵀ)⁻¹, then this just makes it = TW[(MTW)ᵀ|CstuffHonly]([MTW(MTW)ᵀ|CstuffHandV])⁻¹ *)

(* and now we'll try to rework that into terms of 𝐺, not 𝒈 *)
Take[N[First[1200 * bold1.L]].
    JoinHorizontally[
      T.W.Transpose[M.T.W],
      {}
    ].Inverse[Join[
  JoinHorizontally[
    M.T.W.Transpose[M.T.W],
    {}
  ],
  JoinHorizontally[
    {},
    {}
  ]
]], 2]
(* and w/ *)
Take[N[First[1200 * bold1.L]].
    JoinHorizontally[
      T.W.Transpose[M.T.W],
      U
    ].Inverse[Join[
  JoinHorizontally[
    M.T.W.Transpose[M.T.W],
    M.U
  ],
  JoinHorizontally[
    Transpose[M.U],
    ZeroMatrix[1]
  ]
]], 2]

debug = True;
format = "display";
optimizeGeneratorTuningMap["[⟨1 1 0] ⟨0 1 4]⟩", {"optimizationPower" -> 2, "targetIntervals" -> "primes" , "damageWeightSlope" -> "simplicityWeight"}]
optimizeGeneratorTuningMap["[⟨1 1 0] ⟨0 1 4]⟩", {"optimizationPower" -> 2, "targetIntervals" -> "primes" , "damageWeightSlope" -> "simplicityWeight", "unchangedIntervals" -> "octave"}]
optimizeGeneratorTuningMap["[⟨12 19 28]⟩", {"optimizationPower" -> 2, "targetIntervals" -> "primes" , "damageWeightSlope" -> "simplicityWeight", "unchangedIntervals" -> "octave"}]
debug = False;

(* okay crap well... actually the code does do it in terms of 𝒋... *)

N[Take[First[
  1200 * bold1.L.
      JoinHorizontally[
        T.W.Transpose[M.T.W],
        {}
      ].Inverse[Join[
    JoinHorizontally[
      M.T.W.Transpose[M.T.W],
      {}
    ],
    JoinHorizontally[
      {},
      {}
    ]
  ]]], 2]]
(* and w/ *)
N[Take[First[
  1200 * bold1.L.
      JoinHorizontally[
        T.W.Transpose[M.T.W],
        U
      ].Inverse[Join[
    JoinHorizontally[
      M.T.W.Transpose[M.T.W],
      M.U
    ],
    JoinHorizontally[
      Transpose[M.U],
      ZeroMatrix[1]
    ]
  ]]], 2]]
