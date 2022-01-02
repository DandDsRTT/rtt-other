failures = 0;
passes = 0;

getPrimes[count_] := Map[Prime, Range[count]];
test[getPrimes, 5, {2, 3, 5, 7, 11}];

rationalToI[rational_] := Module[{factorization, greatestPrime, count, primes, result, currentPrimeIndex},
  factorization = FactorInteger[rational];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  result = Table[0, count];
  currentPrimeIndex = 1;
  
  Do[
    While[
      primes[[currentPrimeIndex]] < First[factorizationEntry],
      currentPrimeIndex = currentPrimeIndex + 1
    ];
    result[[currentPrimeIndex]] = Last[factorizationEntry],
    {factorizationEntry, factorization}
  ];
  
  result
];
test[rationalToI, 22 / 5, {1, 0, -1, 0, 1}];

iToRational[v_] := Module[{rational, primeIndex},
  rational = 1;
  primeIndex = 1;
  Do[
    rational = rational * Prime[primeIndex]^vEntry;
    primeIndex = primeIndex + 1,
    {vEntry, v}
  ];
  
  rational
];
test[iToRational, {1, 0, -1, 0, 1}, 22 / 5];

getDforB[l_] := PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, l]]]]];
test[getDforB, {2, 9, 7}, 4];

padD[a_, d_] := Map[PadRight[#, d]&, a];
test2args[padD, {{1, 2, 3}, {4, 5, 6}}, 5, {{1, 2, 3, 0, 0}, {4, 5, 6, 0, 0}}];

normalB[b_] := Map[iToRational, removeAllZeroRows[hnf[padD[Map[rationalToI, b], getDforB[b]]]]];
test[normalB, {2, 7, 9}, {2, 9, 7}];

(* TODO: DRY up with normalB *)
canonicalB[b_] := Map[iToRational, removeAllZeroRows[hnf[colHermiteDefactor[padD[Map[rationalToI, b], getDforB[b]]]]]];
test[canonicalB, {2, 7, 9}, {2, 3, 7}];

(*their union-like thing, a superset or equal set to both of them; if doing comma-merge, would be what we want *)
bSumset[bSequence___] := Module[{d, factorizedBSequence},
  d = getDofB[Apply[Join, { bSequence}]];
  factorizedBSequence = Map[padD[Map[rationalToI, #], d]&, {bSequence}];
  
  canonicalB[Map[iToRational, Flatten[factorizedBSequence, 1]]]
];
test2args[bSumset, {2, 3, 5}, {2, 9, 5}, {2, 3, 5}];
test2args[bSumset, {2, 3, 5}, {2, 9, 7}, {2, 3, 5, 7}];
test3args[bSumset, {2, 3, 5}, {2, 9, 7}, {2, 5 / 7, 11}, {2, 3, 5, 7, 11}];

(*TODO: is there no way to do this, like, with duals and merging? because this seems pretty overwrought *)
(*their intersection; if doing map-merge, would be what we want; we only care about mapping stuff relevant to both of the input t's commas *)
shareRoot[rational1_, rational2_] := Module[{gcd},
  gcd = GCD[rational1, rational2];
  
  If[
    gcd == 1,
    False,
    IntegerQ[Log[gcd, rational1]] && IntegerQ[Log[gcd, rational2]]
  ]
];
matchFound[b1entry_, b2_] := Module[{result},
  result = Null;
  Do[
    If[
      shareRoot[b1entry, b2entry],
      result = LCM[b1entry, b2entry]
    ],
    {b2entry, b2}
  ];
  
  result
];
bIntersectionTwoBs[b1_, b2_] := Module[{result},
  result = {};
  
  Do[
    maybeMatch = matchFound[b1entry, b2];
    If[
      maybeMatch === Null,
      "",
      result = Join[result, {maybeMatch}]
    ],
    {b1entry, b1}
  ];
  
  normalB[result]
];
bIntersection[bSequence___] := Module[{result, i},
  result = First[{bSequence}];
  
  Do[
    result = bIntersectionTwoBs[result, b],
    {b, Drop[{bSequence}, 1]}
  ];
  
  result
];
test2args[bIntersection, {2, 3, 5}, {2, 9, 5}, {2, 9, 5}];
test2args[bIntersection, {2, 9 / 7, 5 / 3}, {2, 9, 5}, {2}];
test3args[bIntersection, {2, 3, 5, 7}, {2, 3, 5}, {2, 5, 7}, {2, 5}];

isSubspaceOf[b_, otherB_] := bSumset[b, otherB] == otherB;
test[isSubspaceOf, {2, 9, 5}, {2, 3, 5}, True];
test[isSubspaceOf, {2, 3, 5}, {2, 3, 5, 7}, True];
test[isSubspaceOf, {2, 3, 5}, {2, 9, 5}, False];
test[isSubspaceOf, {2, 3, 5, 7}, {2, 3, 5}, False];

getStandardPrimeLimitB[t_] := getPrimes[getD[t]];
test[getStandardPrimeLimitB, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {2, 3, 5}];

isStandardPrimeLimitB[b_] := normalB[b] == getPrimes[Length[b]];
test[isStandardPrimeLimitB, {2, 3, 5, 7, 11}, True];
test[isStandardPrimeLimitB, {2, 3, 7, 5, 11}, True];
test[isStandardPrimeLimitB, {2, 3, 5, 9, 11}, False];

getB[t_] := If[
  Length[t] == 3,
  Part[t, 3],
  getStandardPrimeLimitB[t]
];
test[getB, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {2, 3, 5}];
test[getB, {{{11, 35, 31}}, "co", {2, 9, 7}}, {2, 9, 7}];

(*TODO: DRY this up with repletesOkay2, fix their names *)
depletesOkay2[baseV_, candidateVToSubtract_] := Module[{ifWeSubtracted, okay, index},
  ifWeSubtracted = baseV - candidateVToSubtract;
  
  okay = True;
  index = 1;
  Do[
    If[
      Abs[ifWeSubtracted[[index]]] > Abs[baseVEntry],
      okay = False
    ];
    index = index + 1, (* TODO: i feel like i use this technique elsewhere ein the code too and int reflects my lack of command of Wolfram Language and I should figure out what 's the canonical way to do this in the language *)
    {baseVEntry, baseV}
  ];
  
  okay
];
test[depletesOkay2, {1, 0, 0}, {1, 0, 0}, True];
test[depletesOkay2, {2, 0, 0}, {1, 0, 0}, True];
test[depletesOkay2, {1, 1, 0}, {1, 0, 0}, True];
test[depletesOkay2, {1, 1, 0}, {1, 1, 0}, True];
test[depletesOkay2, {2, 1, 0}, {1, 1, 0}, True];
test[depletesOkay2, {1, 1, 0}, {1, 2, 0}, False];
test[depletesOkay2, {1, 0, 0}, {0, 0, 1}, False];

repletesOkay2[baseV_, candidateVToAdd_] := Module[{ifWeAdded, okay, index},
  ifWeAdded = baseV + candidateVToAdd;
  
  okay = True;
  index = 1;
  Do[
    If[
      Abs[ifWeAdded[[index]]] > Abs[baseVEntry],
      okay = False
    ];
    index = index + 1,
    {baseVEntry, baseV}
  ];
  
  okay
];

(* express the target formal primes in terms of the initial formal primes*)
getRforM[initialB_, targetB_] := Module[{d, result, row, thing, depletingTargetBEntry},
  d = getDofB[Join[initialB, targetB]];
  factorizedTargetB = padD[Map[rationalToI, targetB], d];
  factorizedInitialB = padD[ Map[rationalToI, initialB], d];
  
  result = {};
  
  Do[
    row = {};
    depletingTargetBEntry = targetBEntry;
    Do[
      thing = 0;
      
      While[
        depletesOkay2[depletingTargetBEntry, initialBEntry],
        thing = thing + 1;
        depletingTargetBEntry = depletingTargetBEntry - initialBEntry
      ];
      
      While[
        repletesOkay2[depletingTargetBEntry, initialBEntry],
        thing = thing - 1;
        depletingTargetBEntry = depletingTargetBEntry + initialBEntry
      ];
      
      row = Join[row, {thing}],
      {initialBEntry, factorizedInitialB}
    ];
    result = Join[result, {row}],
    {targetBEntry, factorizedTargetB}
  ];
  
  Transpose[result]
];
test2args[getRforM, {2, 3, 5, 7}, {2, 3, 5}, Transpose[{{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}]];
test2args[getRforM, {2, 3, 7}, {2, 9, 7}, Transpose[{{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}]];
test2args[getRforM, {2, 3, 5, 7}, {2, 9 / 7, 5 / 3}, Transpose[{{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}]];

getRforC[initialB_, targetB_] := getRforM[targetB, initialB]; (* yes, just swapping initial and target, that's all! *)
test2args[getRforC, {2, 3, 5}, {2, 3, 5, 7}, Transpose[{{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}]];
test2args[getRforC, {2, 9, 7}, {2, 3, 7}, Transpose[{{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}]];
test2args[getRforC, {2, 9 / 7, 5 / 3}, {2, 3, 5, 7}, Transpose[{{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}]];

canonicalFormWithB[t_] := Module[{b, canonicalT},
  b = getB[t];
  canonicalT = canonicalForm[t];
  
  If[
    isStandardPrimeLimitB[b],
    canonicalT,
    Join[canonicalT, {b}]
  ]
];
test[canonicalFormWithB, {{{24, 38, 56}}, "co", {2, 3, 5}}, {{{12, 19, 28}}, "co"}];
test[canonicalFormWithB, {{{22, 70, 62}}, "co", {2, 9, 7}}, {{{11, 35, 31}}, "co", {2, 9, 7}}];

changeBforM[m_, targetB_] := If[
  isSubspaceOf[getB[m], targetB],
  Error,
  canonicalFormWithB[{getA[m].getRforM[getB[m], targetB], "co", targetB}]
];
test2args[changeBforM, {{{12, 19, 28}}, "co"}, {2, 3, 5, 7}, Error];
t = {{{22, 35, 51, 76}}, "co", {2, 3, 5, 11}};
targetB = {2, 9, 11};
expectedT = {{{11, 35, 38}}, "co", {2, 9, 11}};
test2args[changeBforM, t, targetB, expectedT];

changeBforC[c_, targetB_] := If[
  isSubspaceOf[getB[c], targetB],
  canonicalFormWithB[{Transpose[getRforC[getB[c], targetB].Transpose[getA[c]]], "contra", targetB}],
  Error
];
test2args[changeBforC, {{{4, -4, 1}}, "contra"}, {2, 9, 7}, Error];
t = {{{0, 1, 0}, {0, -2, 1}}, "contra", {2, 9 / 7, 5 / 3}};
targetB = {2, 3, 5, 7};
expectedT = {{{0, -1, 1, 0}, {0, -2, 0, 1}}, "contra"}; (*{{{0,2,0,-1},{0,-5,1,2}},"contra"}, before canonicalization *)
test2args[changeBforC, t, targetB, expectedT];

mapMergeWithB[tSequence___] := Module[{bSequence, intersectedB, tSequenceWithIntersectedB},
  bSequence = Map[getB, {tSequence}];
  intersectedB = Apply[bIntersection, bSequence];
  tSequenceWithIntersectedB = Map[changeBforM[#, intersectedB]&, { tSequence}];
  
  canonicalFormWithB[{Apply[Join, Map[getM, tSequenceWithIntersectedB]], "co", intersectedB}]
];
t1 = {{{22, 35, 51, 76}}, "co", {2, 3, 5, 11}};
t2 = {{{17, 54, 48, 59}}, "co", {2, 9, 7, 11}};
expectedT = {{{1, 0, 13}, {0, 1, -3}}, "co", {2, 9, 11}};(* {{{22,70,76},{17,54,59}},"co",{2,9,11}}; before canonicalization *)
test2args[mapMergeWithB, t1, t2, expectedT];

commaMergeWithB[tSequence___] := Module[{bSequence, mergedB, tSequenceWithMergedB},
  bSequence = Map[getB, {tSequence}];
  mergedB = Apply[bSumset, bSequence];
  tSequenceWithMergedB = Map[changeBforC[#, mergedB]&, { tSequence}];
  
  canonicalFormWithB[{Apply[Join, Map[getC, tSequenceWithMergedB]], "contra", mergedB}]
];
t1 = {{{4, -4, 1}}, "contra"};
t2 = {{{6, -1, -1}}, "contra", {2, 9, 7}};
expectedT = {{{4, -4, 1, 0}, {-6, 2, 0, 1}}, "contra"};
test2args[commaMergeWithB, t1, t2, expectedT];

Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
