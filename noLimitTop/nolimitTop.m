getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];

map = {1, 1};
results = "";

Do[
  mapPrevious = {};
  While[
    map != mapPrevious,
    
    mapPrevious = map;
    
    mapWeighted = MapIndexed[First[#1 / Log[2, Prime[#2]]]&, map];
    narrowestPrimeIndex = First[First[Position[mapWeighted, Min[mapWeighted]]]];
    widestPrimeIndex = First[First[Position[mapWeighted, Max[mapWeighted]]]];
    narrowestPrime = Prime[narrowestPrimeIndex];
    widestPrime = Prime[widestPrimeIndex];
    narrowestPrimeMapping = map[[narrowestPrimeIndex]];
    widestPrimeMapping = map[[widestPrimeIndex]];
    
    edo = (narrowestPrimeMapping * Log[2, widestPrime] + widestPrimeMapping * Log[2, narrowestPrime]) / (2 * Log[2, narrowestPrime] * Log[2, widestPrime]);
    
    damage = Abs[(narrowestPrimeMapping / edo) / Log[2, narrowestPrime] - 1];
    maximumPrimePossibleToExceedDamage = Min[1000, Power[2, 1 / (2 * damage * edo)] + 2];
    
    map = getUniformMap[edo, maximumPrimePossibleToExceedDamage];
  ];
  
  results = results <> ToString[N[edo, 17]] <> " " <> ToString[map] <> "\n";
  
  map[[narrowestPrimeIndex]]++,
  
  1000
];

results
