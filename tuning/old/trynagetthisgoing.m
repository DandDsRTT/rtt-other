jip[d_] := Map[Log2, Map[Prime, Range[d]]];
tenneyWeights[m_] := DiagonalMatrix[1 / jip[getD[m]]];
unweightedG[m_] := PseudoInverse[m];
weightedG[m_] := tenneyWeights[m].PseudoInverse[m.tenneyWeights[m]];
octaves[vOrVList_] := jip[Length[vOrVList]].vOrVList;
absOctaves[vOrVList_] := jip[Length[vOrVList]].Abs[vOrVList];
cents[vOrVList_] := 1200.octaves[vOrVList];

meantone = {{1, 1, 0}, {0, 1, 4}};
M = meantone.tenneyWeights[meantone];
Print["what?", M];
G = {x, y};
V = {1, 1, 1};

Plot3D[
  Transpose[(G.M - V)].(G.M - V),
  {x, -5, 5},
  {y, -5, 5},
  PlotTheme -> "Web"
]



M = {{1, 1, 0}, {0, 1, 4}};
g = {g1, g2};
p = Log[2, {2, 3, 5}];
e = g.M - p;

Plot3D[
  (* Transpose[e].e,*)
  (*Power[Norm[e, 2],2],*)
  (*e,*)
  Norm[e, 2],
  {g1, 0.8, 1.2},
  {g2, 0.5, 0.7},
  PlotTheme -> "Web"
]

M = {{1, 1, 0}, {0, 1, 4}};
g = {g1, g2};
p = Log[2, {2, 3, 5}];
e = g.M - p;

Plot3D[
  (* Transpose[e].e,*)
  (*Power[Norm[e, 2],2],*)
  (*e,*)
  Norm[e, Infinity],
  {g1, 0.8, 1.2},
  {g2, 0.5, 0.65},
  PlotTheme -> "Web",
  PlotStyle -> Directive[RGBColor["#6600CC"]],
  ImageSize -> 1000,
  MaxRecursion -> 6,
  Axes -> False
]

Plot3D[
  (* Transpose[e].e,*)
  (*Power[Norm[e, 2],2],*)
  (*e,*)
  Norm[e, 1],
  {g1, 0.8, 1.2},
  {g2, 0.5, 0.65},
  PlotTheme -> "Web",
  PlotStyle -> Directive[RGBColor["#CC6600"]],
  ImageSize -> 1000,
  MaxRecursion -> 6,
  Axes -> False
]

Plot3D[
  (* Transpose[e].e,*)
  (*Power[Norm[e, 2],2],*)
  (*e,*)
  Norm[e, 2],
  {g1, 0.8, 1.2},
  {g2, 0.5, 0.65},
  PlotTheme -> "Web",
  PlotStyle -> Directive[RGBColor["#00CC00"]],
  ImageSize -> 1000,
  MaxRecursion -> 6,
  Axes -> False
]



Plot3D[
  Max[
    (*3/2*) Abs[701.96 - (1 * g1 + 1 * g2)],
    (*5/4*) Abs[386.31 - (1 * g1 + -2 * g2)],
    (*5/3*) Abs[884.36 - (2 * g1 + -3 * g2)],
    (*7/4*) Abs[968.83 - (2 * g1 + -2 * g2)],
    (*7/6*) Abs[266.87 - (1 * g1 + -3 * g2)],
    (*7/5*) Abs[582.51 - (1 * g1 + 0 * g2)]
  ],
  {g1, 590, 610},
  {g2, 100, 120},
  PlotTheme -> "Web"
]
