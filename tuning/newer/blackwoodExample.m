Clear[{g1, g2}];
M = {{5, 8, 0}, {0, 0, 1}};
r = {g1, g2}.M - 1200 * Log2[{2, 3, 5}];

damage[i_] := Abs[r.i]

Plot3D[
  {
    damage[{1, 0, 0}],
    damage[{0, 1, 0}],
    damage[{0, 0, 1}](*,
damage[{1,1,-1}]*)
  },
  {g1, 230, 245},
  {g2, 2750, 2825},
  PlotRange -> {0, 150},
  (*ClippingStyle -> None,*)
  MaxRecursion -> 6,
  ImageSize -> 1000
]

debug = True;
optimizeGeneratorTuningMap["[⟨5 8 0] ⟨0 0 1]}", "primes minimax-U"]

debug = True;
optimizeGeneratorTuningMap["[⟨5 8 0] ⟨0 0 1]}", "primes minimax-U"]
debug = False;

debug = True;
optimizeGeneratorTuningMap["[⟨5 8 0] ⟨0 0 1]}", "{2/1,3/1,5/1,6/5} minimax-U"]

Unprotect[D];
M = {{5, 8, 0}, {0, 0, 1}};
j = 1200 * Log2[{2, 3, 5}];
D = {{0, 13.88}}; (* that's -4x the change for g3 as for g2, so techincally it could be {{0,1,-4}}, but then the scale on the diagram wouldn't be quite right, i.e. from 0 to 1 the b1 value *)
b = {b1};
gZero = {238.612, 2779.374};
tZero = gZero.M;
negativeRZero = j - tZero;

damage[i_] := Abs[(b.D.M - negativeRZero).i];

Plot[
  {
    damage[{1, 0, 0}],
    damage[{0, 1, 0}],
    damage[{0, 0, 1}]
  },
  {b1, -0.25, 1.25},
  PlotRange -> {0, 10},
  PlotLabels -> {"2/1", "3/1", "5/1"},
  ImageSize -> 1000
]
