chartStartEdo = (*6.875;*)6.8; (*11.8;*)
chartEndEdo = (*6.9;*)7.3;(*12.1;*)
maxPrime = 13;
sliderIncrement = 0.0001;
gifIncrement = 0.01;

primes = Map[Prime, Range[PrimePi[maxPrime]]];
primeLogs = Map[Log[2, #]&, primes];
primeCount = Length[primes];
p = 1200 * primeLogs;

myCyan = RGBColor[0.3, 0.7, 0.2];

(* TODO: style this thing so that it more closely resembles the other chart *)

doPlot[edo_] := Apply[ListLinePlot, Module[{g, uniformMap, t, e, d},
  g = 1200 / edo;
  uniformMap = Map[Round[edo * #]&, primeLogs];
  t = g * uniformMap;
  e = t - p;
  d = MapThread[Abs[#2] / Log[2, #1]&, {primes, e}];
  
  maxD = Max[d];
  
  damagesLine = MapThread[{#1, #2}&, {primes, d}];
  maximumDamageLine = {{1, maxD}, {maxPrime, maxD}};
  maxDamageContour = Map[{#, 1200 / (2 * edo * Log[2, #])}&, Range[2, maxPrime, 0.1]];
  
  {
    {
      damagesLine,
      maximumDamageLine,
      maxDamageContour
    },
    
    (* overall config *)
    ImageSize -> 1000,
    ImagePadding -> 44,
    PlotLabel -> Style["prime damages shifting with generator size", 22, Bold, FontFamily -> "Arial"],
    
    (* per line config *)
    PlotStyle -> {White, Orange, myCyan},
    PlotMarkers -> {{Graphics[{Black, Disk[]}], .03}, None, None},
    
    (* per axis config *)
    PlotRange -> {{1, maxPrime + 1}, {0, 600 / chartStartEdo }}
  }
]];
(*doPlot[chartStartEdo]*)

Animate[doPlot[edo], {edo, chartStartEdo, chartEndEdo, sliderIncrement, Appearance -> "Labeled"}, AnimationRunning -> False]

gif = Table[doPlot[edo], {edo, chartStartEdo, chartEndEdo, gifIncrement}];
CloudExport[gif, "GIF", "bouncyAnimation", "DisplayDurations" -> 0.5]
