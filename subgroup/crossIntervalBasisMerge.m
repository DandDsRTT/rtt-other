failures = 0;
passes = 0;

getPrimes[count_] := Map[Prime, Range[count]];
test[getPrimes, 5, {2, 3, 5, 7, 11}];

rationalToI[rational_] := Module[{factorization, greatestPrime, count, primes, i, currentPrimeIndex},
  factorization = FactorInteger[rational];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  i = Table[0, count];
  currentPrimeIndex = 1;
  
  Do[
    While[
      primes[[currentPrimeIndex]] < First[factorizationEntry],
      currentPrimeIndex += 1
    ];
    i[[currentPrimeIndex]] = Last[factorizationEntry],
    {factorizationEntry, factorization}
  ];
  
  i
];
test[rationalToI, 22 / 5, {1, 0, -1, 0, 1}];

iToRational[v_] := Module[{rational, primeIndex},
  rational = 1;
  primeIndex = 1;
  Do[
    rational = rational * Prime[primeIndex]^vEntry; (* TODO: var name *)
    primeIndex += 1,
    {vEntry, v}
  ];
  
  rational
];
test[iToRational, {1, 0, -1, 0, 1}, 22 / 5];

getDforB[l_] := PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, l]]]]];
test[getDforB, {2, 9, 7}, 4];

padD[a_, d_] := Map[PadRight[#, d]&, a];
test[padD, {{1, 2, 3}, {4, 5, 6}}, 5, {{1, 2, 3, 0, 0}, {4, 5, 6, 0, 0}}];

super[rational_] := If[rational < 1, Denominator[rational] / Numerator[rational], rational];
test[super, 5 / 3, 5 / 3];
test[super, 3 / 5, 5 / 3];

canonicalB[b_] := Map[super, Map[iToRational, antiTranspose[removeAllZeroRows[hnf[antiTranspose[padD[Map[rationalToI, b], getDforB[b]]]]]]]];
(* order by prime-limit*)
test[canonicalB, {2, 7, 9}, {2, 9, 7}];
test[canonicalB, {2, 9 / 7, 5}, {2, 5, 9 / 7}];
test[canonicalB, {2, 9 / 7, 5 / 3}, {2, 5 / 3, 9 / 7}];

(* consolidate redundancies *)
test[canonicalB, {2, 3, 9}, {2, 3}];
test[canonicalB, {2, 3, 15}, {2, 3, 5}];
test[canonicalB, {2, 3, 5 / 3}, {2, 3, 5}];

(* tricky stuff *)
test[canonicalB, {2, 5 / 3, 7 / 5}, {2, 5 / 3, 7 / 3}];

(* all the subgroups on the wiki page if they are canonical according to this *)
test[canonicalB, {2, 3, 7}, {2, 3, 7}];
test[canonicalB, {2, 5, 7}, {2, 5, 7}];
test[canonicalB, {2, 3, 7 / 5}, {2, 3, 7 / 5}];
test[canonicalB, {2, 5 / 3, 7}, {2, 5 / 3, 7}];
test[canonicalB, {2, 5, 7 / 3}, {2, 5, 7 / 3}];
test[canonicalB, {2, 5 / 3, 7 / 3}, {2, 5 / 3, 7 / 3}];
test[canonicalB, {2, 27 / 25, 7 / 3}, {2, 27 / 25, 7 / 3}];
test[canonicalB, {2, 9 / 5, 9 / 7}, {2, 9 / 5, 9 / 7}];
test[canonicalB, {2, 3, 11}, {2, 3, 11}];
test[canonicalB, {2, 5, 11}, {2, 5, 11}];
test[canonicalB, {2, 7, 11}, {2, 7, 11}];
test[canonicalB, {2, 3, 5, 11}, {2, 3, 5, 11}];
test[canonicalB, {2, 3, 7, 11}, {2, 3, 7, 11}];
test[canonicalB, {2, 5, 7, 11}, {2, 5, 7, 11}];
test[canonicalB, {2, 5 / 3, 7 / 3, 11 / 3}, {2, 5 / 3, 7 / 3, 11 / 3}];
test[canonicalB, {2, 3, 13}, {2, 3, 13}];
test[canonicalB, {2, 3, 5, 13}, {2, 3, 5, 13}];
test[canonicalB, {2, 3, 7, 13}, {2, 3, 7, 13}];
test[canonicalB, {2, 5, 7, 13}, {2, 5, 7, 13}];
test[canonicalB, {2, 5, 7, 11, 13}, {2, 5, 7, 11, 13}];
test[canonicalB, {2, 3, 13 / 5}, {2, 3, 13 / 5}];
test[canonicalB, {2, 3, 11 / 5, 13 / 5}, {2, 3, 11 / 5, 13 / 5}];
test[canonicalB, {2, 3, 11 / 7, 13 / 7}, {2, 3, 11 / 7, 13 / 7}];
test[canonicalB, {2, 7 / 5, 11 / 5, 13 / 5}, {2, 7 / 5, 11 / 5, 13 / 5}];

(*their union-like thing, a superset or equal set to both of them; if doing comma-merge, would be what we want *)
bMerge[bl___] := Module[{concatedB, factorizedConcatedB},
  concatedB = Apply[Join, {bl}];
  factorizedConcatedB = padD[Map[rationalToI, concatedB], getDforB[concatedB]];
  
  canonicalB[Map[iToRational, factorizedConcatedB]]
];

(* returns the supergroup, when one is a subgroup of the other *)
test[bMerge, {2, 3, 5}, {2, 9, 5}, {2, 3, 5}];

(* basically works *)
test[bMerge, {2, 3, 5}, {2, 9, 7}, {2, 3, 5, 7}];

(* can handle more than two interval bases at once *)
test[bMerge, {2, 3, 5}, {2, 9, 7}, {2, 5 / 7, 11}, {2, 3, 5, 7, 11}];
test[bMerge, {4}, {16}, {4}];
test[bMerge, {25 / 9}, {5 / 3}, {5 / 3}];

(*their intersection; if doing map-merge, would be what we want; we only care about mapping stuff relevant to both of the input t's commas *)
rationalsShareRoot[rational1_, rational2_] := Module[{gcf},
  gcf = getGcf[{rational1, rational2}];
  
  If[
    gcf == 1,
    False,
    IntegerQ[Log[gcf, rational1]] && IntegerQ[Log[gcf, rational2]]
  ]
];
findFIfAnyInOtherIntervalBasisThatSharesRoot[b1f_, b2_] := Module[{fSharingRoot},
  fSharingRoot = Null;
  Do[
    If[
      rationalsShareRoot[b1f, b2f],
      fSharingRoot = LCM[b1f, b2f]
    ],
    {b2f, b2}
  ];
  
  fSharingRoot
];
bIntersectionBinary[b1_, b2_] := Module[{intersectedB},
  intersectedB = {};
  
  Do[
    fSharingRoot = findFIfAnyInOtherIntervalBasisThatSharesRoot[b1f, b2];
    If[
      fSharingRoot === Null,
      "",
      intersectedB = Join[intersectedB, {fSharingRoot}]
    ],
    {b1f, b1}
  ];
  
  canonicalB[intersectedB] (* TODO: is this necessary to canonicalize too? *)
];
bIntersection[bl___] := Module[{intersectedB},
  intersectedB = First[{bl}];
  
  Do[
    intersectedB = bIntersectionBinary[intersectedB, b],
    {b, Drop[{bl}, 1]}
  ];
  
  canonicalB[intersectedB]
];
test[bIntersection, {2, 3, 5}, {2, 9, 5}, {2, 9, 5}];
test[bIntersection, {2, 9 / 7, 5 / 3}, {2, 9, 5}, {2}];
test[bIntersection, {2, 3, 5, 7}, {2, 3, 5}, {2, 5, 7}, {2, 5}];

isSubspaceOf[candidateSubspaceB_, candidateSuperspaceB_] := bMerge[candidateSubspaceB, candidateSuperspaceB] == candidateSuperspaceB;
test[isSubspaceOf, {2, 9, 5}, {2, 3, 5}, True];
test[isSubspaceOf, {2, 3, 5}, {2, 3, 5, 7}, True];
test[isSubspaceOf, {2, 3, 5}, {2, 9, 5}, False];
test[isSubspaceOf, {2, 3, 5, 7}, {2, 3, 5}, False];
test[isSubspaceOf, {4}, {2}, True];
test[isSubspaceOf, {8}, {4}, False];
test[isSubspaceOf, {16}, {4}, True];
test[isSubspaceOf, {3, 5, 7}, {2, 11, 13}, False];
test[isSubspaceOf, {2, 3, 5}, {2, 3, 7}, False];
test[isSubspaceOf, {2, 3, 7}, {2, 3, 5}, False];
test[isSubspaceOf, {2, 5 / 3, 7}, {2, 3, 5, 7}, True];
test[isSubspaceOf, {2, 5 / 3, 7 / 5}, {2, 3, 5, 7}, True];
test[isSubspaceOf, {2, 7 / 5}, {2, 5, 7}, True];
test[isSubspaceOf, {2, 5, 7}, {2, 7 / 5}, False];
test[isSubspaceOf, {2, 105, 11}, {2, 15, 7, 11}, True];
test[isSubspaceOf, {2, 25 / 9, 11 / 7}, {2, 5 / 3, 7, 11}, True];
test[isSubspaceOf, {2, 3 / 2, 5 / 2, 5 / 3}, {2, 3, 5}, True];
test[isSubspaceOf, {2, 9 / 5, 3}, {2, 3, 5}, True];

getStandardPrimeLimitB[t_] := getPrimes[getD[t]];
test[getStandardPrimeLimitB, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {2, 3, 5}];

isStandardPrimeLimitB[b_] := canonicalB[b] == getPrimes[Length[b]];
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

signsMatch[integer1_, integer2_] := Sign[integer1] == 0 || Sign[integer2] == 0 || Sign[integer1] == Sign[integer2];
test[signsMatch, 3, 5, True];
test[signsMatch, -3, -5, True];
test[signsMatch, -3, 5, False];
test[signsMatch, 3, -5, False];
test[signsMatch, 3, 0, True];
test[signsMatch, 0, 5, True];
test[signsMatch, -3, 0, True];
test[signsMatch, 0, - 5, True];

factorizationIsAcceptableForThisPrimesCounts[integer1_, integer2_] := Abs[integer1] >= Abs[integer2] && signsMatch[integer1, integer2];

(*TODO: DRY this up with isDenominatorFactor *)
isNumeratorFactor[factorizedSubspaceF_, factorizedSuperspaceF_] := !MemberQ[MapThread[factorizationIsAcceptableForThisPrimesCounts, {factorizedSubspaceF, factorizedSubspaceF - factorizedSuperspaceF}], False];
test[isNumeratorFactor, {1, 0, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {2, 0, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 0, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 1, 0}, True];
test[isNumeratorFactor, {2, 1, 0}, {1, 1, 0}, True];
test[isNumeratorFactor, {1, 1, 0}, {1, 2, 0}, False];
test[isNumeratorFactor, {1, 0, 0}, {0, 0, 1}, False];

isDenominatorFactor[factorizedSubspaceF_, factorizedSuperspaceF_] := !MemberQ[MapThread[factorizationIsAcceptableForThisPrimesCounts, {factorizedSubspaceF, factorizedSubspaceF + factorizedSuperspaceF}], False];
test[isDenominatorFactor, {1, 0, 0}, {1, 0, 0}, False];
test[isDenominatorFactor, {1, -1, 0}, {1, 0, 0}, False];
test[isDenominatorFactor, {1, -1, 0}, {0, 1, 0}, True];

(* express the target formal primes in terms of the initial formal primes*)
getRforM[originalSuperspaceB_, targetSubspaceB_] := Module[
  {
    d,
    factorizedTargetSubspaceB,
    factorizedOriginalSuperspaceB,
    r,
    rCol,
    rColEntry,
    remainingToBeFactorizedTargetSubspaceF
  },
  
  d = getDforB[Join[originalSuperspaceB, targetSubspaceB]];
  factorizedTargetSubspaceB = padD[Map[rationalToI, targetSubspaceB], d];
  factorizedOriginalSuperspaceB = padD[Map[rationalToI, originalSuperspaceB], d];
  
  r = {};
  
  Do[
    rCol = {};
    remainingToBeFactorizedTargetSubspaceF = factorizedTargetSubspaceF;
    Do[
      rColEntry = 0;
      
      While[
        isNumeratorFactor[remainingToBeFactorizedTargetSubspaceF, factorizedOriginalSuperspaceF],
        rColEntry += 1;
        remainingToBeFactorizedTargetSubspaceF -= factorizedOriginalSuperspaceF
      ];
      
      While[
        isDenominatorFactor[remainingToBeFactorizedTargetSubspaceF, factorizedOriginalSuperspaceF],
        rColEntry -= 1;
        remainingToBeFactorizedTargetSubspaceF += factorizedOriginalSuperspaceF
      ];
      
      rCol = Join[rCol, {rColEntry}],
      {factorizedOriginalSuperspaceF, factorizedOriginalSuperspaceB}
    ];
    r = Join[r, {rCol}],
    {factorizedTargetSubspaceF, factorizedTargetSubspaceB}
  ];
  
  Transpose[r] (* TODO: I don't think this should be transposed, and dealt with accordingly in the few places where it's used *)
];
test[getRforM, {2, 3, 5, 7}, {2, 3, 5}, Transpose[{{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}]];
test[getRforM, {2, 3, 7}, {2, 9, 7}, Transpose[{{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}]];
test[getRforM, {2, 3, 5, 7}, {2, 9 / 7, 5 / 3}, Transpose[{{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}]];

getRforC[originalSubspaceB_, targetSuperspaceB_] := getRforM[targetSuperspaceB, originalSubspaceB]; (* yes, just swapping initial and target, that's all! *)
test[getRforC, {2, 3, 5}, {2, 3, 5, 7}, Transpose[{{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}]];
test[getRforC, {2, 9, 7}, {2, 3, 7}, Transpose[{{1, 0, 0}, {0, 2, 0}, {0, 0, 1}}]];
test[getRforC, {2, 9 / 7, 5 / 3}, {2, 3, 5, 7}, Transpose[{{1, 0, 0, 0}, {0, 2, 0, -1}, {0, -1, 1, 0}}]];

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

changeBforM[m_, targetSubspaceB_] := If[
  isSubspaceOf[getB[m], targetSubspaceB],
  Error,
  canonicalFormWithB[{getA[m].getRforM[getB[m], targetSubspaceB], "co", targetSubspaceB}]
];
test[changeBforM, {{{12, 19, 28}}, "co"}, {2, 3, 5, 7}, Error];
t = {{{22, 35, 51, 76}}, "co", {2, 3, 5, 11}};
targetSubspaceB = {2, 9, 11};
expectedT = {{{11, 35, 38}}, "co", {2, 9, 11}};
test[changeBforM, t, targetSubspaceB, expectedT];

changeBforC[c_, targetSuperspaceB_] := If[
  isSubspaceOf[getB[c], targetSuperspaceB],
  canonicalFormWithB[{Transpose[getRforC[getB[c], targetSuperspaceB].Transpose[getA[c]]], "contra", targetSuperspaceB}],
  Error
];
test[changeBforC, {{{4, -4, 1}}, "contra"}, {2, 9, 7}, Error];
t = {{{0, 1, 0}, {0, -2, 1}}, "contra", {2, 9 / 7, 5 / 3}};
targetB = {2, 3, 5, 7};
expectedT = {{{0, -1, 1, 0}, {0, -2, 0, 1}}, "contra"}; (*{{{0,2,0,-1},{0,-5,1,2}},"contra"}, before canonicalization *)
test[changeBforC, t, targetB, expectedT];
test[changeBforC, {{{1}}, "contra", {27}}, {9}, Error];
test[changeBforC, {{{1}}, "contra", {81}}, {9}, {{{1}}, "contra", {9}}];

mapMergeWithB[tl___] := Module[{bl, intersectedB, tlWithIntersectedB},
  bl = Map[getB, {tl}];
  intersectedB = Apply[bIntersection, bl];
  tlWithIntersectedB = Map[changeBforM[#, intersectedB]&, {tl}];
  
  canonicalFormWithB[{Apply[Join, Map[getM, tlWithIntersectedB]], "co", intersectedB}]
];
t1 = {{{22, 35, 51, 76}}, "co", {2, 3, 5, 11}};
t2 = {{{17, 54, 48, 59}}, "co", {2, 9, 7, 11}};
expectedT = {{{1, 0, 13}, {0, 1, -3}}, "co", {2, 9, 11}};(* {{{22,70,76},{17,54,59}},"co",{2,9,11}}; before canonicalization *)
test[mapMergeWithB, t1, t2, expectedT];

commaMergeWithB[tl___] := Module[{bl, mergedB, tlWithMergedB},
  bl = Map[getB, {tl}];
  mergedB = Apply[bMerge, bl];
  tlWithMergedB = Map[changeBforC[#, mergedB]&, {tl}];
  
  canonicalFormWithB[{Apply[Join, Map[getC, tlWithMergedB]], "contra", mergedB}]
];
t1 = {{{4, -4, 1}}, "contra"};
t2 = {{{6, -1, -1}}, "contra", {2, 9, 7}};
expectedT = {{{4, -4, 1, 0}, {-6, 2, 0, 1}}, "contra"};
test[commaMergeWithB, t1, t2, expectedT];

Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
