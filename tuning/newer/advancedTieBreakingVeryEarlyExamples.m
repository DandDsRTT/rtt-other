In[101]:= Clear[{g1,g2}];
M = {{1, 1,0},{0,1,4}};
r = {g1,g2}.M - 1200*Log2[{2,3,5}];

damage[i_] := Abs[r.i]

Plot3D[
  {
    damage[{1,0,0}],
    damage[{0,1,0}],
    damage[{0,0,1}]
  },
  {g1,1202.3,1202.5},
  {g2, 697.1,697.3},
  PlotRange -> {2,3},
  (*ClippingStyle -> None,*)
  MaxRecursion ->6,
  ImageSize -> 1000
]

In[116]:= damage[i_] := Abs[r.i]

Plot3D[
  {
    damage[{1,0,0}],
    damage[{0,1,0}],
    (*damage[{0,0,1}]*)
  },
  {g1,1190,1210},
  {g2, 690,710},
  PlotRange -> {0,30},
  (*ClippingStyle -> None,*)
  MaxRecursion ->6,
  ImageSize -> 1000
]
