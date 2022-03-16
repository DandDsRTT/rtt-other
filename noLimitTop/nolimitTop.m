getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];

m = {1, 1};
Do[
  mPrevious = {};
  While[
    m != mPrevious,
    
    mPrevious = m;
    
    mWeighted = MapIndexed[First[#1 / Log[2, Prime[#2]]]&, m];
    indexMin = First[First[Position[mWeighted, Min[mWeighted]]]];
    indexMax = First[First[Position[mWeighted, Max[mWeighted]]]];
    minPrime = Prime[indexMin];
    maxPrime = Prime[indexMax];
    mEntryMinPrime = m[[indexMin]];
    mEntryMaxPrime = m[[indexMax]];
    
    edo = (mEntryMinPrime * Log[2, maxPrime] + mEntryMaxPrime * Log[2, minPrime]) / (2 * Log[2, minPrime] * Log[2, maxPrime]);
    
    dMax = Abs[(mEntryMinPrime / edo) / Log[2, minPrime] - 1];
    primeMax = Min[1000, Power[2, 1 / (2 * dMax * edo)] + 2];
    
    m = getUniformMap[edo, primeMax];
  ];
  
  Print[N[edo, 17], " ", m];
  m[[indexMin]]++,
  
  {i, 100}
];
