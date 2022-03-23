visualizeTuningUniform[config_] := Module[{},
  map = Part[config, 1]; (* the display map *)
  generator = Part[config, 2];
  wartNotation = Part[config, 3];
  originalMapForTitle = Part[config, 4];
  isErlichTuned = Part[config, 5];
  
  primes = Map[Prime, Range[Length[map]]];
  tunings = Map[# * generator&, map];
  pures = Map[1200 * Log[2, #]&, primes];
  centsOff = tunings - pures;
  
  mapWithNextClosestMappingsForEachPrime = MapThread[(#2 - Sign[#1])&, {centsOff, map}];
  nextClosestTunings = Map[# * generator&, mapWithNextClosestMappingsForEachPrime];
  nextClosestCentsOff = nextClosestTunings - pures;
  
  mostOffYouCanPossiblyBe = generator / 2;
  mostOffYouCanPossiblyBeUpperLine = {{0.5, mostOffYouCanPossiblyBe}, {Length[primes] + 0.5, mostOffYouCanPossiblyBe}};
  mostOffYouCanPossiblyBeLowerLine = {{0.5, -mostOffYouCanPossiblyBe}, {Length[primes] + 0.5, - mostOffYouCanPossiblyBe}};
  
  generatorUpperLine = {{0.5, generator}, {Length[primes] + 0.5, generator}};
  generatorLowerLine = {{0.5, -generator}, {Length[primes] + 0.5, - generator}};
  
  myCyan = RGBColor[0.3, 0.7, 0.2];
  
  tuningsConnectors = MapIndexed[{{First[#2], 0}, {First[#2], #1}}&, centsOff];
  nextClosestTuningsConnectors = MapIndexed[{{First[#2], 0}, {First[#2], #1}}&, nextClosestCentsOff];
  
  plotStyle = {Opacity[0], Opacity[0], {myCyan, Dashed}, {myCyan, Dashed}, {Orange, Dashed}, {Orange, Dashed}, Opacity[0]};
  Do[AppendTo[plotStyle, {myCyan, Dashed}], Length[tuningsConnectors]];
  Do[AppendTo[plotStyle, {Orange, Dashed}], Length[nextClosestTuningsConnectors]];
  plotMarkers = {Graphics[{Black, Disk[]}, ImageSize -> 15], Graphics[{Orange, Disk[]}, ImageSize -> 15], Graphics[], Graphics[], Graphics[], Graphics[], Graphics[{Magenta, Circle[]}, ImageSize -> 25]};
  Do[AppendTo[plotMarkers, Graphics[]], Length[tuningsConnectors]];
  Do[AppendTo[plotMarkers, Graphics[]], Length[nextClosestTuningsConnectors]];
  plotLegends = {"\[FilledCircle] uniform map", Style["\[FilledCircle] the other closest\n    prime mappings", Orange], None, None, None, None, Style["\[EmptyCircle] non-matches (if any)", Magenta]};
  Do[AppendTo[plotLegends, None], Length[tuningsConnectors]];
  Do[AppendTo[plotLegends, None], Length[nextClosestTuningsConnectors]];
  
  limit = Prime[Length[map]];
  
  aaa = MapThread[#1 != #2&, {map, originalMapForTitle}];
  bbb = MapIndexed[If[#1, {First[#2], Part[centsOff, First[#2]]}, {}]&, aaa];
  disagrees = Select[bbb, Length[#] > 0&];
  disagreesMap = MapThread[If[#1, #2, 0]&, {aaa, map}];
  disagreesMap = Select[disagreesMap, # > 0&];
  If[Length[disagrees] == 0, AppendTo[disagrees, {-69, 0}]];
  If[Length[disagreesMap] == 0, AppendTo[disagreesMap, {-69, 0}]];
  
  Print["\n\n\n"];
  ListLinePlot[
    Flatten[{
      {MapThread[Labeled, {centsOff, map}]},
      {MapThread[Labeled[#1, #2, LabelStyle -> Orange]&, {nextClosestCentsOff, mapWithNextClosestMappingsForEachPrime}]},
      {mostOffYouCanPossiblyBeUpperLine},
      {mostOffYouCanPossiblyBeLowerLine},
      {generatorUpperLine},
      {generatorLowerLine},
      {MapThread[Labeled[#1, #2, LabelStyle -> Magenta]&, {disagrees, disagreesMap}]},
      tuningsConnectors,
      nextClosestTuningsConnectors
    }, 1],
    
    (* overall config *)
    PlotLabel -> Pane[Style[If[isErlichTuned, ToString[limit] <> "-limit uniform map for " <> "the Erlich-tuned generator of " <> wartNotation <> " ⟨" <> StringRiffle[originalMapForTitle] <> "]: " <> ToString[generator] <> "¢\n", ToString[limit] <> "-limit uniform map for " <> "the generator " <> ToString[generator] <> "¢: " <> "⟨" <> StringRiffle[originalMapForTitle] <> "]\n"], 16, Bold], Alignment -> Center],
    PlotRange -> {{0.5, Length[primes] + 0.5}, All},
    PlotRangePadding -> {0, Scaled[0.1]},
    ImageSize -> 800,
    
    (* per axis config *)
    AxesStyle -> {{myCyan, 14, FontFamily -> "Helvetica"}, {Black, 14, FontFamily -> "Helvetica"}},
    AxesLabel -> {"     primes\n(justly-tuned)", "error (¢)"},
    Ticks -> {MapIndexed[{First[#2], Style["        " <> ToString[#1], 16, myCyan]}&, primes], Automatic},
    
    (* per line config *)
    PlotStyle -> plotStyle,
    PlotMarkers -> plotMarkers,
    PlotLegends -> Placed[plotLegends, Left],
    
    (* filling config *)
    FillingStyle -> Directive[Opacity[0.1], myCyan],
    Filling -> {3 -> {4}},
    
    Epilog -> {
      myCyan,
      Text["½-generator wide", Offset[{50, 0}, {Length[primes] + 0.5, mostOffYouCanPossiblyBe}]],
      Text["½-generator narrow", Offset[{50, 0}, {Length[primes] + 0.5, -mostOffYouCanPossiblyBe}]],
      Orange,
      Text["1 generator wide", Offset[{50, 0}, {Length[primes] + 0.5, generator}]],
      Text["1 generator narrow", Offset[{50, 0}, {Length[primes] + 0.5, -generator}]],
    }
  ]
];

visualizeTuningUniform[{{12, 19, 28, 34, 42, 45, 49, 51}, 99.3855, "12f", {12, 19, 28, 34, 42, 45, 49, 51}, True}]
(*visualizeTuningUniform[{{16, 25,37,45,55,59,65,68}, 75.4676, "16h", {16, 25,37,45,55,59,65,67}, True}] (* counterexample *)*)
(*visualizeTuningUniform[{{16, 25,37,45,55,59,65,67}, 75.6, "16h", {16, 25,37,45,55,59,65,67}, False}] (* showing off how 16h is still a uniform map *)*)
