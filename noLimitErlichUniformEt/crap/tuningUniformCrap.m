pureLine = Map[{#,1200*Log[2,#]}&,Range[1,19,0.25]];
map ={16, 25,37,45,55,59,65,67};(* {12,19,28,34,42,45,49,51};*)
primes = Map[Prime,Range[Length[map]]];
generator =(* 99.3855;*)75.4676;
tunings = MapThread[{#1,#2*generator}&,{primes,map}];

ListLinePlot[
  {
    tunings,
    pureLine
  },
  PlotStyle -> {Opacity[0],Opacity[1]},
  PlotMarkers -> {Automatic,Graphics[]},
  PlotRange -> All,
  ImageSize -> 1000
]






pureLine = Map[{#,#}&,Range[1,19,0.25]];
map = {12,19,28,34,42,45,49,51};
primes = Map[Prime,Range[Length[map]]];
generator = 99.3855/1200;(*75.4676;*)
Print[generator];
tunings = MapThread[{#1,Power[2,#2*generator]}&,{primes,map}];
Print[tunings];

ListLinePlot[
  {
    tunings,
    pureLine
  },
  PlotStyle -> {Opacity[0],Opacity[1]},
  PlotMarkers -> {Automatic,Graphics[]},
  PlotRange -> All,
  ImageSize -> 1000
]
