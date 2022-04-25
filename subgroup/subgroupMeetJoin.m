quotientToV[quotient_] := Module[{factorization, biggestPrime, count, primes, result, currentPrimeIndex},
  factorization = FactorInteger[quotient];
  biggestPrime = First[Last[factorization]];
  count = PrimePi[biggestPrime];
  primes = Map[Prime, Range[count]];
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

vToQuotient[v_] := Module[{quotient, primeIndex},
  quotient = 1;
  primeIndex = 1;
  Do[
    quotient = quotient * Prime[primeIndex]^vEntry;
    primeIndex = primeIndex + 1,
    {vEntry, v}
  ];
  
  quotient
];

changeS[t_, targetS_] := Module[{},
  0
];

pLimit[a_] := PrimePi[Max[Map[First, Map[Last, Map[FactorInteger, a]]]]];

padD[a_, d_] := Map[PadRight[#, d]&, a];

canonicalS[s_] := Map[vToQuotient, removeAllZeroRows[hnf[padD[Map[quotientToV, s], pLimit[s]]]]];

sUnion[s1_, s2_] := Module[{d, factorizedS1, factorizedS2},
  d = pLimit[Join[s1, s2]];
  factorizedS1 = padD[Map[quotientToV, s1], d];
  factorizedS2 = padD[Map[quotientToV, s2], d];
  
  canonicalS[Map[vToQuotient, Join[factorizedS1, factorizedS2]]]
];

shareRoot[quotient1_, quotient2_] := Module[{gcd},
  gcd = getGcd[quotient1, quotient2];
  
  If[
    gcd == 1,
    False,
    IntegerQ[Log[gcd, quotient1]] && IntegerQ[Log[gcd, quotient2]]
  ]
];

matchFound[s1entry_, s2_] := Module[{result},
  result = Null;
  Do[
    If[
      shareRoot[s1entry, s2entry],
      result = LCM[s1entry, s2entry]
    ],
    {s2entry, s2}
  ];
  
  result
];

sIntersection[s1_, s2_] := Module[{result},
  result = {};
  
  Do[
    maybeMatch = matchFound[s1entry, s2, True];
    If[
      maybeMatch === Null,
      "",
      result = Join[result, {maybeMatch}]
    ],
    {s1entry, s1}
  ];
  
  canonicalS[result]
];


test[quotientToV, 22 / 5, {1, 0, -1, 0, 1}];
test[vToQuotient, {1, 0, -1, 0, 1}, 22 / 5];

t = {{{22, 35, 51, 76}}, "co", {2, 3, 5, 11}};
targetS = {2, 9, 11};
expectedT = {{{22, 70, 76}}, "co", {2, 9, 11}};
test2args[changeS, t, targetS, expectedT];

test[canonicalS, {2, 3, 9, 11}, {2, 3, 11}];

test2args[sUnion, {2, 9, 5 / 11}, {2, 3, 7 / 11}, {2, 3, 5 / 11, 7 / 11}];
test2args[sUnion, {2, 9, 7 / 5}, {2, 3, 11 / 5}, {2, 3, 5 / 11, 7 / 11}];

test2args[sIntersection, {2, 9, 7 / 5}, {2, 3, 11 / 5}, {2, 9}];
