getUniformMap[n_, limit_] := Module[
  {generatorIterations,uniformMap,pIndex, p,  purePrimeTuning, candidateBestPrimeTuning},
  
  generatorIterations = 1;
  uniformMap = {};
  
  pIndex = 1;
  p = Prime[pIndex];
  purePrimeTuning = Log[2,p];
  
  While[
    p <= limit,
    
    candidateBestPrimeTuning = n *generatorIterations;
    If[
      Abs[purePrimeTuning - candidateBestPrimeTuning] < n/2,
      AppendTo[uniformMap, generatorIterations];
      
      pIndex++;
      p  = Prime[pIndex];
      purePrimeTuning = Log[2,p];
      generatorIterations--; (*becaus sometimes it'll be the same number for consecutive primes*)
    ];
    generatorIterations++;
  ];
  
  uniformMap
];
