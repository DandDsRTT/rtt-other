getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];
getDamage[primeMapping_, prime_, edo_] := Abs[(primeMapping / edo) / Log[2, prime] - 1];

visualizeResult[m_, edo_, narrowestPrimeIndex_, damage_] := Module[{},
  primes = Map[Prime, Range[Length[m] + 2]];
  primeDamagesPrimes = primes[[1 ;; Length[primes] - 2]];
  primeDamages = MapThread[{#2, N[getDamage[#1, #2, edo], 5]}&, {m, primeDamagesPrimes}];
  primes[[Length[primes]]] = primes[[Length[primes] - 1]] + 1;
  maxDamageContour = Map[{#1, 1 / (2 * edo * Log[2, #1])}&, Range[2, Max[primes], 0.25]];
  primes[[1]] = 0;
  threshold = MapThread[{#2, #1}&, {Table[damage, Length[primes]], primes}];
  primes[[1]] = 2;
  primes = primes[[1 ;; Length[primes] - 1]];
  maxPrimeForAxis = Max[primes] + 1;
  primeLabels = Map[ToString[#]&, primes];
  primeLabels[[Length[primeLabels]]] = "(" <> primeLabels[[Length[primeLabels]]] <> ")";
  maxes = {{widestPrime, damage}, {narrowestPrime, damage}};
  biggerPrime = Max[widestPrime, narrowestPrime];
  highestPointInChart = Part[First[maxDamageContour], 2];
  biggerPrimeMax = {{biggerPrime, 0}, {biggerPrime, highestPointInChart =}};
  crossingPoint = First[First[Select[maxDamageContour, Part[#, 2] < damage&, 1]]];
  crossingLine = {{crossingPoint, 0}, {crossingPoint, highestPointInChart}};
  thresholdForFilling = {{biggerPrime, damage}, {crossingPoint, damage}};
  maxDamageContourForFilling = Select[maxDamageContour, (crossingPoint >= First[#] >= biggerPrime)&];
  
  ListLinePlot[
    {primeDamages, threshold, maxDamageContour, maxes, biggerPrimeMax, crossingLine, thresholdForFilling, maxDamageContourForFilling},
    
    ImageSize -> 1000,
    PlotLabel -> Style[ToString[N[edo, 18]] <> " \[RightArrow] ⟨" <> StringRiffle[m] <> " ...]", 16, Bold],
    PlotLabels -> {"", "Erlich damage", "max possible damage"},
    PlotRange -> {{1, All}, All},
    
    PlotStyle -> {Opacity[0], {Magenta}, Opacity[1], {Magenta, Thickness[0]}, {Orange}, {Orange}, {Orange, Opacity[0]}, {Orange, Opacity[0]}},
    PlotMarkers -> {Automatic, Graphics[], Graphics[], {"\[EmptyCircle]", 30}, Graphics[], Graphics[], Graphics[], Graphics[]},
    
    AxesStyle -> {Opacity[0], Opacity[1]},
    AxesLabel -> {"damage", ""},
    Filling -> {7 -> {8}},
    FillingStyle -> Directive[Opacity[0.3], Orange],
    Epilog -> AxisObject[
      Line[{{1, 0}, {maxPrimeForAxis, 0}}],
      {1, maxPrimeForAxis},
      TickPositions -> {{primes}},
      TickLabels -> {primeLabels},
      AxisLabel -> "primes"
    ]
  ]
]

(* examples (though you'll eventually render for many of them I hope *)
m = {1, 1};
(*m = {12, 19, 28, 34, 42, 45, 49, 51, 55, 59, 60, 63, 65, 66, 67, 69};*) (* this is a good one *)
(*m = {12, 19, 28, 34, 41, 44};*)
(*m = {12, 19, 28, 34, 42, 44, 49};*)
(*m = {12, 20,29};*)
(*m = {22, 35, 51, 62, 76, 81, 90, 93, 99, 107, 109};*)

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
    
    damage = getDamage[narrowestPrimeMapping, narrowestPrime, edo];
    maximumPrimePossibleToExceedDamage = Min[1000, Power[2, 1 / (2 * damage * edo)] + 2];
    
    m = getUniformMap[edo, maximumPrimePossibleToExceedDamage];
  ];
  
  Print[visualizeResult[m, edo, narrowestPrimeIndex, damage]];
  
  m[[narrowestPrimeIndex]]++,
  
  2
];

(* TODO: would be cool to format the y-axis the same way *)
(* TODO: names for the lines *)
(* TODO: manually edit out the primes included past the intersection of the max damage contour and the threshold, or solve that problem in the algorithm *)
