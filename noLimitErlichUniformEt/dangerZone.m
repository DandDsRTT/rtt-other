getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];
getDamage[primeMapping_, prime_, edo_] := Abs[(primeMapping / edo) / Log[2, prime] - 1];

(* TODO: manually edit out the primes included past the intersection of the max damage contour and the threshold, or solve that problem in the algorithm... for now I'm hacking it off with "adjusto" and "shortM" but the problem myight be deper and if i fix it in the actual algorithm and then use this anyway it could braeak FYI to future sel *)

getTopPossibility[prime_] := Part[First[Select[maxDamageContour, First[#] == prime&]], 2];

visualizeResult[m_, edo_, narrowestPrimeIndex_, damage_] := Module[{},
  adjusto = 1;
  primes = Map[Prime, Range[Length[m] + adjusto]];
  primeDamagesPrimes = primes[[1 ;; Length[primes] - (adjusto + 1)]];
  shorterM = m[[1 ;; Length[m] - 1]];
  primeDamages = MapThread[{#2, N[1200 * getDamage[#1, #2, edo], 5]}&, {shorterM, primeDamagesPrimes}];
  primes[[Length[primes]]] = primes[[Length[primes] - 1]] + 1;
  maxDamageContour = Map[{#1, 1200 / (2 * edo * Log[2, #1])}&, Range[2, Max[primes], 0.1]];
  primes[[1]] = 0;
  threshold = MapThread[{#2, #1}&, {Table[damage, Length[primes]], primes}];
  primes[[1]] = 2;
  primes = primes[[1 ;; Length[primes] - 1]];
  maxPrimeForAxis = Max[primes] + 1;
  primeLabels = Map[ToString[#]&, primes];
  primeLabels[[Length[primeLabels]]] = "(" <> primeLabels[[Length[primeLabels]]] <> ")";
  maxes = {{widestPrime, damage}, {narrowestPrime, damage}};
  biggerPrime = Max[widestPrime, narrowestPrime];
  smallerPrime = Min[widestPrime, narrowestPrime];
  highestPointInChart = Part[First[maxDamageContour], 2];
  biggerPrimeMax = {{biggerPrime, 0}, {biggerPrime, highestPointInChart}};
  crossingPoint = First[First[Select[maxDamageContour, Part[#, 2] < damage&, 1]]];
  crossingLine = {{crossingPoint, 0}, {crossingPoint, highestPointInChart}};
  thresholdForFilling = {{biggerPrime, damage}, {crossingPoint, damage}};
  maxDamageContourForFilling = Select[maxDamageContour, (crossingPoint >= First[#] >= biggerPrime)&];
  myCyan = RGBColor[0.3, 0.7, 0.2];
  dangerZoneTopCenter = Part[maxDamageContourForFilling, Round[Length[maxDamageContourForFilling] / 2 ]];
  dangerZoneLabelPoint = Offset[{0, 66}, dangerZoneTopCenter ];
  dangerZoneCenterPoint = {First[dangerZoneTopCenter], (Part[dangerZoneTopCenter, 2] + damage) / 2};
  primePossibilities = Map[{{#, 0}, {#, getTopPossibility[#]}}&, primeDamagesPrimes];
  plotStyle = {
    White,
    Magenta,
    myCyan,
    Magenta,
    Orange,
    Orange,
    Opacity[0],
    Opacity[0]
  };
  plotMarkers = {{Graphics[{Black, Disk[]}], .03}, None, None, {Graphics[{Magenta, Circle[]}], .04}, None, None, None, None};
  Do[AppendTo[plotStyle, {myCyan, Dashed}], Length[primePossibilities]];
  Do[AppendTo[plotMarkers, None], Length[primePossibilities]];
  
  ListLinePlot[
    Flatten[{{primeDamages}, {threshold}, {maxDamageContour}, { maxes}, { biggerPrimeMax}, { crossingLine}, { thresholdForFilling}, {maxDamageContourForFilling}, primePossibilities}, 1],
    
    (* overall config *)
    ImageSize -> 1000,
    ImagePadding -> 44,
    PlotLabel -> Style[ToString[N[edo, 18]] <> " \[RightArrow] ⟨" <> StringRiffle[shorterM] <> " ...]", 22, Bold, FontFamily -> "Arial"],
    
    (* per line config *)
    PlotStyle -> plotStyle,
    PlotMarkers -> plotMarkers,
    
    (* per axis config *)
    PlotRange -> {{1, All }, All},
    Ticks -> {primes, Automatic},
    AxesLabel -> {"primes", "Erlich\ndamage"},
    
    (* filling config *)
    FillingStyle -> Directive[Opacity[0.3], Orange],
    Filling -> {7 -> {8}},
    
    Epilog -> {
      Magenta,
      Text["tuning-defining\nprime (smaller)", Offset[{44, 22}, {smallerPrime, damage}]],
      Text["tuning-defining\n prime (larger)", Offset[{-44, -22}, {biggerPrime, damage}]],
      Text["max damage defining\nthis maps's Erlich tuning", Offset[{-44, 22}, Last[threshold]]],
      Orange,
      Text["region where a prime\nmight possibly break\nthis map's Erlich tuning", dangerZoneLabelPoint],
      Line[{Offset[{0, 44}, dangerZoneTopCenter], dangerZoneCenterPoint}],
      myCyan,
      Text["max possible\ndamage\nper prime", Offset[{-44, -33}, Last[maxDamageContour]]]
    }
  ]
]

(* examples (though you'll eventually render for many of them I hope *)
(*m = {1, 1};*)
m = {12, 19, 28, 34, 42, 45, 49, 51, 55, 59, 60, 63, 65, 66, 67, 69}; (* this is a good one *)
(*m = {12, 19, 28, 34, 41, 44};*)
m = {12, 19, 28, 34, 42, 44, 49};
m = {12, 20, 29};
(*m = {22, 35, 51, 62, 76, 81, 90, 93, 99, 107, 109};*)
(*m = {2, 3, 4, 5, 6, 7, 8, 8, 8, 9, 9};*)
(*m = getUniformMap[1.86135311615,5];*)(*here's a way to check the values from Keenan's original post which don't have their maps with them *)
(*m = getUniformMap[1.861353116, 19];
m = getUniformMap[20.8201746563 , 43];
m = getUniformMap[20.84434941344809, 7 ];*)

howManyToDoAtOnceStartingWithTheAbove = 1;

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
  
  Print["\n\n\n"];
  Print[visualizeResult[m, edo, narrowestPrimeIndex, 1200 * damage]];
  
  m[[narrowestPrimeIndex]]++,
  
  howManyToDoAtOnceStartingWithTheAbove
];
(*]*)



(*this is me attempting to do one-off charts, including counterexamples (this is one such case), but there must be some assumptions in the gnarled code that prevent things that are counterexamples from rendering without crashing *)
edo = 20.8201746563;
m = getUniformMap[edo , 43];
narrowestPrimeIndex = PrimePi[43];
damage = 10.364486176;
visualizeResult[m, edo, narrowestPrimeIndex, damage]

