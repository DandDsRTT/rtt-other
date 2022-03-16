getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];

m = {1, 1};
results = "";

Do[
  mPrevious = {};
  While[
    m != mPrevious,
    
    mPrevious = m;
    
    mWeighted = MapIndexed[First[#1 / Log[2, Prime[#2]]]&, m];
    narrowestPrimeIndex = First[First[Position[mWeighted, Min[mWeighted]]]];
    widestPrimeIndex = First[First[Position[mWeighted, Max[mWeighted]]]];
    narrowestPrime = Prime[narrowestPrimeIndex];
    widestPrime = Prime[widestPrimeIndex];
    narrowestPrimeMapping = m[[narrowestPrimeIndex]];
    widestPrimeMapping = m[[widestPrimeIndex]];
    
    edo = (narrowestPrimeMapping * Log[2, widestPrime] + widestPrimeMapping * Log[2, narrowestPrime]) / (2 * Log[2, narrowestPrime] * Log[2, widestPrime]);
    
    damage = Abs[(narrowestPrimeMapping / edo) / Log[2, narrowestPrime] - 1];
    maximumPrimePossibleToExceedDamage = Min[1000, Power[2, 1 / (2 * damage * edo)] + 2];
    
    m = getUniformMap[edo, maximumPrimePossibleToExceedDamage];
  ];
  
  results = results <> ToString[N[edo, 17]] <> " " <> ToString[m] <> "\n";
  
  m[[narrowestPrimeIndex]]++,
  
  1000
];

results
