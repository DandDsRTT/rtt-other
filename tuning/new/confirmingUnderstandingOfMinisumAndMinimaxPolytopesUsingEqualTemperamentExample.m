diff[pcv_] := Abs[{g1, g2}.{{1, 1, 0}, {0, 1, 4}}.pcv - 1200 * Log2[{2, 3, 5}].pcv]


Plot3D[
  {
    diff[{-1, 1, 0}],
    diff[{2, -1, 0}],
    diff[{-2, 0, 1}],
    diff[{3, 0, -1}],
    diff[{0, -1, 1}],
    diff[{1, 1, -1}]
  },
  {g1, 1180, 1220},
  {g2, 680, 720}
]

diff2[pcv_] := Abs[{g}.{{12, 19, 28}}.pcv - 1200 * Log2[{2, 3, 5}].pcv]

Plot[
  {
    diff2[{-1, 1, 0}],
    diff2[{2, -1, 0}],
    diff2[{-2, 0, 1}],
    diff2[{3, 0, -1}],
    diff2[{0, -1, 1}],
    diff2[{1, 1, -1}],
    diff2[{-1, 1, 0}] + diff2[{2, -1, 0}] + diff2[{-2, 0, 1}] + diff2[{3, 0, -1}] + diff2[{0, -1, 1}] + diff2[{1, 1, -1}],
  },
  (*{g, 70,115}*)
  {g, 95, 105}
]

diff3[pcv_, g_] := Abs[{g}.{{12, 19, 28}}.pcv - 1200 * Log2[{2, 3, 5}].pcv]
diff4[pcv_] := Map[{#, diff3[pcv, #]}&, Range[95, 106, 0.1]];
a1 = diff4[{-1, 1, 0}];
a2 = diff4[{2, -1, 0}];
a3 = diff4[{-2, 0, 1}];
a4 = diff4[{3, 0, -1}];
a5 = diff4[{0, -1, 1}];
a6 = diff4[{1, 1, -1}];

ListLinePlot[
  {
    a1, a2, a3, a4, a5, a6,
    MapThread[{First[#1], Last[#1] + Last[#2] + Last[#3] + Last[#4] + Last[#5] + Last[#6]}&, {a1, a2, a3, a4, a5, a6}],
    {
      {100.9024066, 6.47},
      {105.2137623, 34.54},
      {105.2137623, 28.02},
      {100.0000000, 15.64},
      {100.0000000, 13.69},
      {100.0000000, 1.96},
      {101.9550009, 11.73},
      {101.7107858, 10.51},
      {91.20185607, 63.54},
      {91.20185607, 42.04}
    },
    {
      {99.609, 62.565},
      {101.711, 82.606},
      {100.279, 62.565},
      {105.214, 187.695},
      {96.578, 123.177},
      {98.262, 76.034}
    }
  },
  PlotMarkers -> {None, None, None, None, None, None, None, {Graphics[{Black, Disk[]}], .01}, {Graphics[{Black, Disk[]}], .02}},
  PlotStyle -> {Automatic, Automatic, Automatic, Automatic, Automatic, Automatic, Automatic, Opacity[0], Opacity[0]},
  PlotRange -> {{95, 106}, {0, 190}},
  ImageSize -> 1000
]
