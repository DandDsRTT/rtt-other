getLogSopfrDamage[tm_, {meanPower_, tima_, notion_, d_, t_, ptm_, weighted_, weightingDirection_, complexityWeighting_, complexityPower_}] := Module[{e, w},
  e = N[tm.Transpose[tima]] - N[ptm.Transpose[tima]];
  w = Map[getLogSopfrComplexity[#, t]&, tima];
  w = If[weightingDirection == "simplicityWeighted", 1 / w, w];
  
  e * w
];
