m = {{3, 0, 7, 0}, {0, 4, 0, 7}};
(*m = {{3,0,7,18},{0,1,0,-2}};*)
m = {{5, 8, 0}, {0, 0, 1}};

(* TODO: would be better if this automatically asisgned the g mins and maxes below *)
optimizeGtm[{m, "co"}, "tuning" -> "Tenney"]

g1min = 237;
g1max = 241;
g2min = 2750;
g2max = 2850;


Plot3D[
  {
    (*2/1*)Abs[1200.000 - (m[[1]][[1]] * g1 + m[[2]][[1]] * g2)],
    (*3/1*) Abs[1901.955 - (m[[1]][[2]] * g1 + m[[2]][[2]] * g2)] / Log[2, 3],
    (*5/1*) Abs[2786.314 - (m[[1]][[3]] * g1 + m[[2]][[3]] * g2)] / Log[2, 5](*,
    (*7/1*) Abs[3368.826 - (m[[1]][[4]] * g1 + m[[2]][[4]] * g2)]/ Log[2,7]*)
  },
  {g1, g1min, g1max},
  {g2, g2min, g2max},
  PlotTheme -> "BoldColor", PlotRange -> {0, 40}, MaxRecursion -> 6
]

Plot3D[
  {
    (*2/1*) Abs[1200.000 - (m[[1]][[1]] * g1 + m[[2]][[1]] * g2)]
  },
  {g1, g1min, g1max},
  {g2, g2min, g2max},
  PlotTheme -> "BoldColor", PlotRange -> {0, 40}, MaxRecursion -> 6
]

(*Print["hhm", m[[1]][[2]] , "and", m[[2]][[2]]];*)
Plot3D[
  {
    0,
    (*3/1*) Abs[1901.955 - (m[[1]][[2]] * g1 + m[[2]][[2]] * g2)] / Log[2, 3]
  },
  {g1, g1min, g1max},
  {g2, g2min, g2max},
  PlotTheme -> "BoldColor", PlotRange -> {0, 40}, PlotStyle -> {Opacity[0], Opacity[1]}, MaxRecursion -> 6
]

Plot3D[
  {
    0,
    0,
    (*5/1*) Abs[2786.314 - (m[[1]][[3]] * g1 + m[[2]][[3]] * g2)] / Log[2, 5]
  },
  {g1, g1min, g1max},
  {g2, g2min, g2max},
  PlotTheme -> "BoldColor", PlotRange -> {0, 40}, PlotStyle -> {Opacity[0], Opacity[0], Opacity[1]}, MaxRecursion -> 6
]

(*Plot3D[
 {
 0,
 0,
 0,
    (*7/1*) Abs[3368.826 - (m[[1]][[4]] * g1 + m[[2]][[4]] * g2)]/ Log[2,7]
  },
    {g1, g1min, g1max},
  {g2, g2min, g2max},
  PlotTheme -> "BoldColor", PlotRange -> {0,40}, PlotStyle -> {Opacity[0], Opacity[0],Opacity[0],Opacity[1]}, MaxRecursion -> 6
]*)
