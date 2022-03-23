getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];

edos = Range[1, 1200];

epilog = Map[Text[Style["⟨" <> ToString[#] <> "]", 50 / #], {-1200 / #, 900 / (# + 0.5)}]&, edos];
PrependTo[epilog, Black];

points = Flatten[Map[{
  {1200 / #, 0},
  {1200 / (# + 0.5), -600 / (# + 0.5)},
  {1200 / (# + 0.5), 600 / (# + 0.5)}
}&, edos], 1];

ListLinePlot[
  points,
  ScalingFunctions -> {"Reverse"},
  PlotRange -> All,
  AxesOrigin -> {1200, 0},
  AxesLabel -> {"generator\nsize (¢)", "error (¢)"},
  Epilog -> epilog,
  ImageSize -> 1000,
  PlotLabel -> Style["error of nearest approximation of prime 2 per generator size", Bold, 16]
]

twoDamage = Map[ {Part[#, 1], Abs[Part[#, 2]]}&, points];
ListLinePlot[
  twoDamage,
  ScalingFunctions -> {"Reverse"},
  PlotRange -> All,
  AxesOrigin -> {1200, 0},
  AxesLabel -> {"generator\nsize (¢)", "Erlich damage"},
  Epilog -> epilog,
  ImageSize -> 1000,
  PlotLabel -> Style["damage of nearest approximation of prime 2 per generator size", Bold, 16]
]

getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];

epilog = Map[Text[Style["⟨_ " <> ToString[#] <> "]", 50 / #], {-1200 * Log[2, 3] / #, 900 * Log[2, 3] / (# + 0.5)}]&, edos];
PrependTo[epilog, Black];

edos = Range[1, 1200];

pointsThree = Flatten[Map[{
  {1200 * Log[2, 3] / #, 0},
  {1200 * Log[2, 3] / (# + 0.5), -600 * Log[2, 3] / (# + 0.5)},
  {1200 * Log[2, 3] / (# + 0.5), 600 * Log[2, 3] / (# + 0.5)}
}&, edos], 1];

ListLinePlot[
  {{{0, 0}}, pointsThree},
  ScalingFunctions -> {"Reverse"},
  PlotRange -> {{0, 1200}, {-1200 * Log[2, 3] / 3, 1200 * Log[2, 3] / 3}},
  AxesOrigin -> {1200, 0},
  AxesLabel -> {"generator\nsize (¢)", "error (¢)"},
  Epilog -> epilog,
  ImageSize -> 1000,
  PlotLabel -> Style["error of nearest approximation of prime 3 per generator size", Bold, 16]
]

getUniformMap[edo_, limit_] := Map[Round[edo * Log[2, #]]&, Map[Prime, Range[PrimePi[limit]]]];

epilog = Map[Text[Style["⟨" <> ToString[#] <> "]", 50 / #], {-1200 * Log[2, 3] / #, 900 * Log[2, 3] / (# + 0.5)}]&, edos];
PrependTo[epilog, Black];

edos = Range[1, 1200];

pointsThree = Flatten[Map[{
  {1200 * Log[2, 3] / #, 0},
  {1200 * Log[2, 3] / (# + 0.5), -600 * Log[2, 3] / (# + 0.5)},
  {1200 * Log[2, 3] / (# + 0.5), 600 * Log[2, 3] / (# + 0.5)}
}&, edos], 1];

ListLinePlot[
  {points, pointsThree},
  ScalingFunctions -> {"Reverse"},
  PlotRange -> {{0, 1200}, {-1200 * Log[2, 3] / 3, 1200 * Log[2, 3] / 3}},
  AxesOrigin -> {1200, 0},
  AxesLabel -> {"generator\nsize (¢)", "error (¢)"},
  (*Epilog -> epilog,*)
  ImageSize -> 1000,
  PlotLabel -> Style["error of nearest approximations of prime 2 and prime 3 per generator size", Bold, 16]
]

twoDamage = Map[ {Part[#, 1], Abs[Part[#, 2]]}&, points];
threeDamage = Map[ {Part[#, 1], Abs[Part[#, 2]] / Log[2, 3]}&, pointsThree];
ListLinePlot[
  {twoDamage, threeDamage},
  PlotRange -> {{0, 1200}, All},
  ScalingFunctions -> {"Reverse"},
  PlotRange -> All,
  AxesOrigin -> {1200, 0},
  AxesLabel -> {"generator\nsize (¢)", "Erlich damage"},
  
  (*Epilog -> epilog,*)
  ImageSize -> 1000,
  PlotLabel -> Style["damage of nearest approximation of prime 2 and prime 3 per generator size", Bold, 16]
]
