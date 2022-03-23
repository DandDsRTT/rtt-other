getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];
increment = 0.0001;
edos = Range[1,12,increment];
(*Print["edos: ", edos];*)
uniformMaps = Map[getUniformMap[#,2]&, edos];
(*Print["uniformMaps: ", uniformMaps];*)
generatorSizes = Map[1200/#&,edos];
approxesOfTwo = MapThread[#1*#2&, {uniformMaps, generatorSizes}];
(*Print["approxesOfTwo: ", approxesOfTwo];*)
errors = MapIndexed[{First[#2],First[#1]-1200}&,approxesOfTwo];

(*Print[errors]*)

ListLinePlot[
  errors,
  PlotRange -> All
]





damages = Map[Abs, errors];
ListLinePlot[damages, PlotRange -> All]






getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];
increment = 0.01;
fakeInfinity = 1000000;
generatorSizes =1200* Reverse[Range[increment,1-increment,increment]];
dataPoints = 1/increment;
Print["generatorSizes: ", generatorSizes];
edos = Map[1200/#&,generatorSizes];
Print["edos: ", edos];
uniformMaps = Map[First[getUniformMap[#,2]]&, edos];
Print["uniformMaps: ", uniformMaps];
approxesOfTwo = MapThread[#1*#2&, {uniformMaps, generatorSizes}];
(*Print["approxesOfTwo: ", approxesOfTwo];*)
errors = MapIndexed[{First[#2],#1-1200}&,approxesOfTwo];

(* TODO: I really gotta find a cheaper way to render this, just the key points *)
ListLinePlot[errors, ImageSize -> 1000, Ticks -> { generatorSizes, Automatic}]
