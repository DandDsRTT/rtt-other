keenansF[edo_, limit_] := Module[{},
  m = Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];
  
  mWeighted = MapIndexed[First[#1 / Log[2, Prime[#2]]]&, m];
  narrowestPrimeIndex = First[First[Position[mWeighted, Min[mWeighted]]]];
  widestPrimeIndex = First[First[Position[mWeighted, Max[mWeighted]]]];
  narrowestPrime = Prime[narrowestPrimeIndex];
  widestPrime = Prime[widestPrimeIndex];
  narrowestPrimeMapping = m[[narrowestPrimeIndex]];
  widestPrimeMapping = m[[widestPrimeIndex]];
  
  erlichEdo = (narrowestPrimeMapping * Log[2, widestPrime] + widestPrimeMapping * Log[2, narrowestPrime]) / (2 * Log[2, narrowestPrime] * Log[2, widestPrime]);
  
  erlichEdo - edo
];

discontinuities[prime_, chartEndEdo_] = Quiet[Range[0.5 / Log[2, prime], chartEndEdo, 1 / Log[2, prime]]];


chartStartEdo = 0;
chartEndEdo = 2;
biggestAffectingPrime = 97;

maxY = 0.2;

allDiscontinuities = Map[discontinuities[#, chartEndEdo]&, Map[Prime, Range[PrimePi[biggestAffectingPrime]]]];
(*Print["allDiscontinuities: ", allDiscontinuities];*)
allOfThem = Sort[Apply[Join, allDiscontinuities]];
allOfThem = Select[allOfThem, # >= chartStartEdo&];
PrependTo[allOfThem, chartStartEdo];
allOfThemRanges = MapIndexed[{x, #1, If[First[#2] == Length[allOfThem], chartEndEdo, Part[allOfThem, First[#2] + 1]]}&, allOfThem];
(*Print["allOfThemRanges: ", allOfThemRanges];*)
plots = Map[Plot[keenansF[x, biggestAffectingPrime], #, PlotRange -> plotRange, ImageSize -> 1000, PlotLabel -> Style[ToString[biggestAffectingPrime] <> "-limit Erlich-uniform contour from " <> ToString[chartStartEdo] <> "-EDO to " <> ToString[chartEndEdo] <> "-EDO" , 16, Bold], AxesLabel -> {"input EDO", "difference\nbetween\ninput EDO\nand\noutput EDO"}, ImagePadding -> 100, Ticks -> {Range[chartStartEdo, chartEndEdo, 0.1], Range[-maxY, maxY, 0.1]}]&, allOfThemRanges];
plotRange = {{chartStartEdo, chartEndEdo}, {-maxY, maxY}};

Apply[Show, plots]
(*Plot[keenansF[x, biggestAffectingPrime],{x,chartStartEdo,chartEndEdo},ImageSize -> 1000, PlotRange -> plotRange]*)
