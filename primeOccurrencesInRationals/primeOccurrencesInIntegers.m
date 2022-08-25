integerToPcv[integer_] := Module[{factorization, greatestPrime, count, primes, pcv, currentPrimeIndex},
  factorization = FactorInteger[integer];
  greatestPrime = First[Last[factorization]];
  count = PrimePi[greatestPrime];
  primes = getPrimes[count];
  pcv = Table[0, count];
  currentPrimeIndex = 1;
  
  If[
    Length[primes] == 0,
    {0},
    Do[
      While[
        primes[[currentPrimeIndex]] < First[factorizationEntry],
        currentPrimeIndex += 1
      ];
      pcv[[currentPrimeIndex]] = Last[factorizationEntry],
      {factorizationEntry, factorization}
    ];
    pcv
  ]
];

padVectorsWithZerosUpToD[a_, d_] := Map[PadRight[#, d]&, a];

getPrimes[count_] := Map[Prime, Range[count]];

checkPrimeCountsForN[primeCounts_, n_] := Module[
  {primes},
  
  primes = getPrimes[Length[primeCounts]];
  
  AllTrue[
    MapThread[
      Function[
        {primeCount, p},
        primeCount <= upperBound[n, p] && primeCount >= lowerBound[n, p]
      ],
      {primeCounts, primes}
    ],
    TrueQ
  ]
];

upperBound[n_, p_] := (n - 1) / (p - 1);
lowerBound[n_, p_] := n / (p - 1) - Log[n + 1] / Log[p];

getPrimeCountsForN[n_] := Total[Abs[padVectorsWithZerosUpToD[Map[integerToPcv, Range[1, n]], PrimePi[n]]]];

checkTotalPrimeCountsForN[primeCounts_, n_] := Total[primeCounts] <= If[n == 1, 0, n * (Log[Log[n]] + 1.0345061758)];

(* check all n up to 1000 *)
AllTrue[
  Map[
    Function[
      {n},
      
      primeCounts = getPrimeCountsForN[n];
      
      checkPrimeCountsForN[primeCounts, n] && checkTotalPrimeCountsForN[primeCounts, n]
    ],
    Range[1, 100]
  ],
  TrueQ
]

(* check 10 random n between 1000 and 10000 *)
AllTrue[
  Map[
    Function[
      {i},
      n = RandomInteger[{1000, 10000}];
      primeCounts = getPrimeCountsForN[n];
      
      checkPrimeCountsForN[primeCounts, n] && checkTotalPrimeCountsForN[primeCounts, n]
    ],
    Range[1, 10]
  ],
  TrueQ
]
