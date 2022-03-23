chartStartEdo = (*6.875;*)6.8; (*11.8;*)
chartEndEdo = (*6.9;*)7.3;(*12.1;*)
chartMaxPrime = 13;
sliderIncrement = 0.0001;
gifIncrement = 0.01;

primes = Map[Prime, Range[PrimePi[maxPrime]]];
primeLogs = Map[Log[2, #]&, primes];
primeCount = Length[primes];
p = 1200 * primeLogs;
(*Print["what is even happening right now", primes];*)

myCyan = RGBColor[0.3, 0.7, 0.2];

getTopPossibility[prime_, maxDamageContour_] := Part[First[Select[maxDamageContour, First[#] == prime&]], 2];

doPlot[edo_] := Apply[ListLinePlot, Module[{g, uniformMap, t, e, d, maxD, secondmostMaxD, isErlichTuning, maxPrime, secondmostMaxPrime, damagesLine, maximumDamageLine, maxDamageContour, maxes, primePossibilities, plotStyle, plotMarkers, biggerPrime, smallerPrime, epilog},
  g = 1200 / edo;
  uniformMap = Map[Round[edo * #]&, primeLogs];
  t = g * uniformMap;
  e = t - p;
  d = MapThread[Abs[#2] / Log[2, #1]&, {primes, e}];
  
  maxD = Max[d];
  secondmostMaxD = Last[Sort[d][[1 ;; Length[d] - 1]]];
  (*Print["nowhere close?", maxD," and 2nd ", secondmostMaxD," and diff ", Abs[maxD - secondmostMaxD] ," and sorted ", Sort[d]];*)
  isErlichTuning = Abs[maxD - secondmostMaxD] < 0.1;
  maxPrime = Part[primes, First[First[Position[d, maxD]]]];
  secondmostMaxPrime = Part[primes, First[First[Position[d, secondmostMaxD]]]];
  (* If[
   Part[e, First[First[Position[d,maxD]]]] > 0,
   widestPrime = maxPrime; narrowestPrime = secondmostMaxPrime,
   narrowestPrime = maxPrime; widestPrime = secondmostMaxPrime
   ];*)
  biggerPrime = Max[maxPrime, secondmostMaxPrime];
  smallerPrime = Min[maxPrime, secondmostMaxPrime];
  
  primeDamages = MapThread[{#1, #2}&, {primes, d}];
  maximumDamageLine = {{1, maxD}, {chartMaxPrime + 1, maxD}};
  maxDamageContour = Map[{#, 1200 / (2 * edo * Log[2, #])}&, Range[2, chartMaxPrime + 1, 0.1]];
  maxes = {{maxPrime, maxD}, {secondmostMaxPrime, maxD}};
  primePossibilities = Map[{{#, 0}, {#, getTopPossibility[#, maxDamageContour]}}&, primes];
  plotStyle = {White, If[isErlichTuning, Magenta, Orange], myCyan, If[isErlichTuning, Magenta, Opacity[0]]};
  plotMarkers = {{Graphics[{Black, Disk[]}], .03}, None, None, If[isErlichTuning, {Graphics[{Magenta, Circle[]}], .04}, None]};
  Do[AppendTo[plotStyle, {myCyan, Dashed}], Length[primePossibilities]];
  Do[AppendTo[plotMarkers, None], Length[primePossibilities]];
  
  (*  Print["primeDamages: ", primeDamages, " maximumDamageLine: ", maximumDamageLine];*)
  
  epilog = {
    myCyan,
    Text["max possible\ndamage\nper prime", Offset[{55, -22}, First[maxDamageContour]]]
  };
  If[
    isErlichTuning,
    epilog = Join[{
      Magenta,
      Text["tuning-defining\nprime (smaller)", Offset[{44, 22}, {smallerPrime, damage}]],
      Text["tuning-defining\n prime (larger)", Offset[{-44, -22}, {biggerPrime, damage}]],
      Text["pair of tied damages\n(with opposite errors)\ndefining an Erlich tuning", Offset[{-350, -33}, Last[maximumDamageLine]]],
    }, epilog],
    epilog = Join[{
      Orange,
      Text["maximum damage", Offset[{-350, -11}, Last[maximumDamageLine]]],
    }, epilog]
  ];
  
  {
    Flatten[{
      {primeDamages},
      {maximumDamageLine},
      {maxDamageContour},
      {maxes},
      primePossibilities
    }, 1],
    
    (* overall config *)
    ImageSize -> 1000,
    ImagePadding -> 44,
    PlotLabel -> Style["prime damages for uniform map of " <> ToString[N[edo, 18]] <> "-EDO", 22, Bold, FontFamily -> "Arial"],
    
    (* per line config *)
    PlotStyle -> plotStyle,
    PlotMarkers -> plotMarkers,
    
    (* per axis config *)
    PlotRange -> {{1, chartMaxPrime + 1}, {0, 600 / chartStartEdo }},
    Ticks -> {primes, Automatic},
    AxesLabel -> {"primes", "Erlich\ndamage"},
    
    Epilog -> epilog
  }
]];
(*doPlot[chartStartEdo]*)

Animate[doPlot[edo], {edo, chartStartEdo, chartEndEdo, sliderIncrement, Appearance -> "Labeled"}, AnimationRunning -> False]

(*gif = Table[doPlot[edo], {edo, chartStartEdo, chartEndEdo, gifIncrement}];
CloudExport[gif, "GIF", "bouncyAnimation", "DisplayDurations" -> 0.5]*)
