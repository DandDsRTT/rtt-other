Plot3D[
  {
    (*2/1*) Abs[1200.000 - (3 * g1 + 0 * g2)] / Log[2, 2],
    (*3/1*) Abs[1901.955 - (0 * g1 + 1 * g2)] / Log[2, 3],
    (*5/1*) Abs[2786.314 - (7 * g1 + 0 * g2)] / Log[2, 5],
    (*7/1*) Abs[3368.826 - (18 * g1 + -2 * g2)] / Log[2, 7]
  },
  {g1, 396, 402},
  {g2, 1890, 1917},
  PlotTheme -> "BoldColor",
  PlotRange -> {0, 40},
  MaxRecursion -> 6
]

In[257] := Plot3D[
  {
    (*2/1*) Abs[1200.000 - (3 * g1 + 0 * g2)] / Log[2, 2]
  },
  {g1, 396, 402},
  {g2, 1890, 1917},
  PlotTheme -> "BoldColor",
  PlotRange -> {0, 40},
  MaxRecursion -> 6
]

Plot3D[
  {
    0,
    (*3/1*) Abs[1901.955 - (0 * g1 + 1 * g2)] / Log[2, 3]
  },
  {g1, 396, 402},
  {g2, 1890, 1917},
  PlotTheme -> "BoldColor",
  PlotRange -> {0, 40},
  MaxRecursion -> 6,
  PlotStyle -> {Opacity[0], Opacity[1]}
]

In[259] := Plot3D[
  {
    0,
    0,
    (*5/1*) Abs[2786.314 - (7 * g1 + 0 * g2)] / Log[2, 5]
  },
  {g1, 396, 402},
  {g2, 1890, 1917},
  PlotTheme -> "BoldColor",
  PlotRange -> {0, 40},
  MaxRecursion -> 6,
  PlotStyle -> {Opacity[0], Opacity[0], Opacity[1]}
]

In[260] := Plot3D[
  {
    0,
    0,
    0,
    (*7/1*) Abs[3368.826 - (18 * g1 + -2 * g2)] / Log[2, 7]
  },
  {g1, 396, 402},
  {g2, 1890, 1917},
  PlotTheme -> "BoldColor",
  PlotRange -> {0, 40},
  MaxRecursion -> 6,
  PlotStyle -> {Opacity[0], Opacity[0], Opacity[0], Opacity[1]}
]
