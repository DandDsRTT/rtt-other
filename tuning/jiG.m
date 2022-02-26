(*

jiG[mapping]

Returns JI generators for the given temperament
(provided as a mapping or comma basis).

Examples:

In    meantoneMapping = {{{1, 0, -4}, {0, 1, 4}}, "co"};
      jiG[meantoneMapping]

Out   {{{1, 0, 0}, {2, -1, 0}}, "contra"}

*)
jiG[t_] := Module[{ma, ca, g, preclearedGenesU},
  ma = getA[getM[t]];
  ca = nullSpaceBasis[ma];
  g = getG[t];
  preclearedGenesU = Map[multByLcd, getA[g]];
  
  {MapThread[jiGenerator[#1, #2, c]&, {preclearedGenesU, m}], "contra"}
];




(* ___ PRIVATE ___ *)

getG[t_] := {Transpose[PseudoInverse[getA[getM[t]]]], "contra"};

jiGenerator[preclearedGenesUEl_, mEl_, c_] := Module[{base},
  base = Transpose[colHermiteDefactor[Join[{preclearedGenesUEl}, c]]];
  
  Drop[First[Take[hnf[Transpose[Join[{mEl}.base, base]]], 1]], 1]
];



(* ___ TESTS ___ *)

(* jiG *)
test[jiG, {{{1, 2, 4}, {0, -1, -4}}, "co"}, {{{1, 0, 0}, {2, -1, 0}}, "contra"}];
test[jiG, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{{1, 0, 0}, {3, -3, 1}}, "contra"}];
test[jiG, {{{1, 0, -4}, {0, 1, 4}}, "co"}, {{{1, 0, 0}, {0, 1, 0}}, "contra"}];
test[jiG, {{{1, 2, 3}, {0, -3, -5}}, "co"}, {{{0, 5, -3}, {0, 3, -2}}, "contra"}];
test[jiG, {{{-4, 4, -1}}, "contra"}, {{{3, -3, 1}, {0, 1, 0}}, "contra"}];
