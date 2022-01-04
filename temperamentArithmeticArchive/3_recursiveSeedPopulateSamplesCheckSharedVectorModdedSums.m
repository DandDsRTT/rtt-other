depth = 3;

shared = { 7, 11, 16};
a = {5, 8, 12}; (* [4 -4 1> *)
b = {15, 24, 35}; (* [1 -5 3> *)
trueW = {{4,9,5}};(* [5 -9 4> *) (*meantone + porcupine = tetracot *)

(*shared = {12,19,28};
a = {7, 11, 16}; (* <<1 4 4]] [4 -4 1> *)
b = {18, 29, 42}; (* <<3 0 -7]] [7 0 -3> *)
(*trueW = {{2,- 4, -11}};*) (* <<2 -4 -11]] [11 -4 -2> *) (*no this is if we subtract them*)
trueW = {{4, 4, -3}}; (* [3 4 -4⟩*)(* meantone + augmented = diminished *)*)

(*shared  = {7,11,16};
a = {17,27,40}; (* [4 -4 1> *)
b = {10, 16, 23}; (* [-3 -1 2> *)
trueW = {{3 , 5, 1}};  (* [1 -5 3> *)*) (*meantone + dicot = porcupine *)

hnf[a_] := Last[HermiteDecomposition[a]];
extractGcf[l_] := l / getGcf[l];

populateSamples[v_] := Module[{samples},
  samples = {v, shared};
  Do[
    next = {};
    For[i=1,i < Length[samples],i++,next = Join[next, {samples[[i]], samples[[i]] + samples[[i+1]] }]];
    next = Join[next,{ Last[samples]}];
    samples = next,
    depth
  ];
  samples
];

aSamples = populateSamples[a];
bSamples = populateSamples[b];
pairs = Tuples[{aSamples, bSamples}];

modRow[l_] := l - Floor[First[l] / First[shared]]*shared;

modCounts = Association[];

Do[
  s =extractGcf[First[pair]] + extractGcf[Last[pair]];
  (*s = extractGcf[Last[pair]] - extractGcf[First[pair]]; (* b - a *)*)

  mod = modRow[s];

  If[
    KeyExistsQ[modCounts, mod],
    modCounts[mod] = modCounts[mod] + 1,
    modCounts[mod] = 1
  ];

  h = hnf[{s, shared}];
  w = Minors[h, 2] ;

  (*If[First[pair]=={31,49,72},Print["w: ", w, " pair: ", pair]];*)
  Print["hnf: ", h, " w: ",w, " pair: ", pair, " s: ", s, " mod: ", mod, " t1: ", TensorWedge[shared, extractGcf[First[pair]]] // MatrixForm, " t2: ", TensorWedge[shared, extractGcf[Last[pair]]] // MatrixForm];

  If[
    w == trueW,
    Print["###########match: ", mod],
    (*Print["hnf: ", h, " w: ",w, " pair: ", pair, " s: ", s, " mod: ", mod]*)
  ],
  {pair, pairs}
];

Print[modCounts]
