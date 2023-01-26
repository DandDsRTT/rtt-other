M = {{3,0,7},{0,1,0}};
M.{4,-1,-1}
M.{1,-2,1}  (*(2*{4,-1,-1} + {-7,0,3}) I took advantage of the comma to find a somewhat simple interval *)

(* HERE'S ANOTHER ATTEMPT AT A FINAL EXAMPLE *)
Clear[{g1,g2}];

r = {g1,g2}.M - 1200*Log2[{2,3,5}];
L = DiagonalMatrix[Log2[{2,3,5}]];

damage[i_] := Abs[r.i] *Total[Abs[L.i]];

Plot3D[
  {
    (* here's 18/5 and 15/8 creating the tie strip above the world *)
    (*Max[*)
    damage[{1,2,-1}],
    damage[{-3,1,1}],
    
    (*{4/3,5/4,6/5,9/8}, just being below for noise*)
    damage[{2,-1,0}],
    damage[{-2,0,1}],
    damage[{1,1,-1}],
    damage[{-3,2,0}]
    (*]*)
  },
  {g1,390,410},
  {g2, 1880,1930},
  PlotRange -> {0,150},
  (*ClippingStyle -> None,*)
  MaxRecursion -> 2,
  ImageSize -> 1000
]

debug = True;
optimizeGeneratorTuningMap[ "[⟨3 0 7] ⟨0 1 0]}", {"tuningSchemeSystematicName" -> "{4/3,5/4,6/5,9/8,10/9,16/15} minimax-C", "logging" -> True}]

Unprotect[D];
j = 1200*Log2[{2,3,5}];
D = {{15.604,78.020}}; (* that's -4x the change for g3 as for g2, so techincally it could be {{0,1,-4}}, but then the scale on the diagram wouldn't be quite right, i.e. from 0 to 1 the b1 value *)
b = {b1};
gZero = {394.053, 1871.290};(*{600,1899.04,2774.66};*)
tZero = gZero.M;
negativeRZero = j - tZero;

damage[i_] := Abs[(b.D.M-negativeRZero).i];

Plot[
  {
    (* here's 10/9 and 16/5 creating the tie strip above the world *)
    (*Max[*)
    damage[{-1,-2,1}],
    damage[{3,-1,-1}],
    
    (*{4/3,5/4,6/5,9/8}, just being below for noise*)
    damage[{2,-1,0}],
    damage[{-2,0,1}],
    damage[{1,1,-1}],
    damage[{-3,2,0}]
    (*]*)
  },
  {b1, -0.25,1.25},
  PlotRange -> {0,150},
  PlotLabels -> "Expressions",
  ImageSize -> 1000
]
