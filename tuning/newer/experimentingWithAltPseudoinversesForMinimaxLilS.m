format = "Wolfram";

z = 1;
zs = {-5, -4, -3, -2.5, -12 / 5, -2, -1.5, -1, -0.5, -0.2, -0.0001, 0.0001, 0.2, 0.5, 1, 2, 3, 4, 5};
Do[
  thing2 = {
    {z - 1, -1, -1, 1},
    {-1, z - 1, -1, 1},
    {-1, -1, z - 1, 1}
  };
  original = {
    {1 / z, 0, 0},
    {0, 1 / z, 0},
    {0, 0, 1 / z},
    {1 / z, 1 / z, 1 / z}
  };
  (*Print[thing.original];*)
  (*Print[thing2];*)
  augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[DiagonalMatrix[Log2[{2, 3, 5}]]].thing2, "row"};
  t = getL[optimizeTuningMap[mavila, "minimax-lil-S"]];
  (*Print[Max[Abs[t-1200*Log2[{2,3,5}]]], " a: ",a, " b: ",b," c: ",c," t: ", t],*)
  Print[Max[Abs[t - 1200 * Log2[{2, 3, 5}]]], " z: ", z, " t: ", t],
  
  {z, zs}
];

Minimize[{thing = {
  {aaa - 1, -1, -1, 1},
  {-1, bbb - 1, -1, 1},
  {-1, -1, ccc - 1, 1}
};
original = {
  {1 / aaa, 0, 0},
  {0, 1 / bbb, 0},
  {0, 0, 1 / ccc},
  {1 / aaa, 1 / bbb, 1 / ccc}
};
Print[thing.original];
augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[DiagonalMatrix[Log2[{2, 3, 5}]]].thing, "row"};
Max[Abs[getL[optimizeTuningMap[dicot, "minimax-lil-S"]] - 1200 * Log2[{2, 3, 5}]]]}, {aaa, bbb, ccc}, Integers]


Minimize[
  {
    myThing = {
      {zzz - 1, -1, -1, 1},
      {-1, zzz - 1, -1, 1},
      {-1, -1, zzz - 1, 1}
    };
    myOriginal = {
      {1 / zzz, 0, 0},
      {0, 1 / zzz, 0},
      {0, 0, 1 / zzz},
      {1 / zzz, 1 / zzz, 1 / zzz}
    };
    Print[zzz, myThing.myOriginal];
    augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[DiagonalMatrix[Log2[{2, 3, 5}]]].myThing, "row"};
    Max[Abs[getL[optimizeTuningMap[mavila, "minimax-lil-S"]] - 1200 * Log2[{2, 3, 5}]]]},
  {zzz}(*,
 Reals*)
]



a = -12 / 5;
b = -1 / 2;
c = -9 / 5;
thing = {
  {a - 1, -1, -1, 1},
  {-1, b - 1, -1, 1},
  {-1, -1, c - 1, 1}
};
original = {
  {1 / a, 0, 0},
  {0, 1 / b, 0},
  {0, 0, 1 / c},
  {1 / a, 1 / b, 1 / c}
};
Print[thing.original];
augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[DiagonalMatrix[Log2[{2, 3, 5}]]].thing, "row"};
t = getL[optimizeTuningMap[porcupine, "minimax-lil-S"]];
Print[Max[Abs[t - 1200 * Log2[{2, 3, 5}]]], " a: ", a, " b: ", b, " c: ", c, " t: ", t]

{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}



(* THSHSHITHTIIIT Maybe these are all for minimax-E-lil-S, oops *)
Max[Abs[{1199.568, 1907.462, 2779.247} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=-1*)
Max[Abs[{1199.568, 1907.462, 2779.248} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=-0.5*)
Max[Abs[{1199.568, 1907.464, 2779.250} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=0.0001*)
Max[Abs[{1199.568, 1907.462, 2779.248} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=0.2*)
Max[Abs[{1199.568, 1907.462, 2779.248} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=0.5*)
Max[Abs[{1199.567, 1907.461, 2779.246} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=1*)
Max[Abs[{1199.562, 1907.453, 2779.234} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=2*)
Max[Abs[{1199.550, 1907.434, 2779.206} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=3*)
Max[Abs[{1199.5444, 1907.4244, 2779.1926} - 1200 * Log2[{2, 3, 5}]]] (* porcupine z=4*)




zz = 5
myThing = {
  {zz - 1, -1, -1, 1},
  {-1, zz - 1, -1, 1},
  {-1, -1, zz - 1, 1}
};
myOriginal = {
  {1 / zz, 0, 0},
  {0, 1 / zz, 0},
  {0, 0, 1 / zz},
  {1 / zz, 1 / zz, 1 / zz}
};
Print[myThing.myOriginal];



{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}

aa = 3
bb = 5.5
cc = -44
thing = {
  {aa - 1, -1, -1, 1},
  {-1, bb - 1, -1, 1},
  {-1, -1, cc - 1, 1}
};
original = {
  {1 / aa, 0, 0},
  {0, 1 / bb, 0},
  {0, 0, 1 / cc},
  {1 / aa, 1 / bb, 1 / cc}
};
Print[thing.original];



(* and then the following things could be removed from tuning main: *)

intervalComplexityNormPrescalerSizeFactor = tuningSchemeProperty[tuningSchemeProperties, "intervalComplexityNormPrescalerSizeFactor"]; (* trait 5c *)

(* for e.g. minimax-lil "Weil" "WE" and unchanged-octave minimax-lil-S "Kees" "KE" tunings, remove the junk final entry from the augmentation; 
I wish this didn't have to bleed up to this level, but better here maybe in one place than in each method individually? *)
If[
  ToString[targetIntervals] == "Null" && intervalComplexityNormPrescalerSizeFactor != 0,
  optimumGeneratorTuningMap = rowify[Drop[getL[optimumGeneratorTuningMap], -1]]
];


augmentedTemperedSideGeneratorsPartArg[generatorTuningMap_] := generatorTuningMap;
augmentedTemperedSideMappingPartArg[m_, intervalComplexityNormPrescalerSizeFactor_] := m;
augmentedJustSideGeneratorsPartArg[centsConversionAndSummationMapAndLogPrimeA_] := centsConversionAndSummationMapAndLogPrimeA;
augmentedJustSideMappingPartArg[primesI_] := primesI;
augmentedEitherSideIntervalsPartArg[transposedPrimesI_] := transposedPrimesI;
augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[DiagonalMatrix[Log2[{2, 3, 5}]]].{
  {2 - 1, -1, -1, 1},
  {-1, 2 - 1, -1, 1},
  {-1, -1, 2 - 1, 1}
}, "row"};
augmentedUnchangedIntervalsArg[unchangedIntervals_] := unchangedIntervals;
