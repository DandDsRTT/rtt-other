getPrimes[count_] := Map[Prime, Range[count]];
getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, getPrimes[PrimePi[limit]]];

m = {1, 1};
Do[
  mPrevious = {};
  While[
    m != mPrevious,
    
    mPrevious = m;
    tm = g * m;
    d = Length[m];
    p = Log[2, getPrimes[d]];
    e = tm / p - Table[1, d];
    solution = NMinimize[Norm[e, Infinity], g, Method -> "NelderMead", WorkingPrecision -> 15];
    edo = 1 / g /. Last[solution];
    e = e /. Last[solution];
    dMax = Max[e]; (* right so here's where I'm like... isn't this damage to the primes, not error ?!*)
    primeMax = Power[2, 1 / (2 * dMax * edo)] + 2;
    m = getUniformMap[edo, primeMax];
  ];
  
  primeIndexOfNarrowestPrime = First[First[Position[e, Min[e]]]];
  m[[primeIndexOfNarrowestPrime]]++;
  
  Print[edo, m],
  
  10
];
