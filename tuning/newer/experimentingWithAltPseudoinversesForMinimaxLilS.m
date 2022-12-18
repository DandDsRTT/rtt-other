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

Minimize[
  {
    thing = {
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
    Max[Abs[getL[optimizeTuningMap[dicot, "minimax-lil-S"]] - 1200 * Log2[{2, 3, 5}]]]
  }, 
  {aaa, bbb, ccc}, 
  Integers
]


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



actuallyGetLilDamageD4[tuningMap_, z_] := Module[
  {},
  
  L = DiagonalMatrix[Log2[{2, 3, 5, 7}]];
  map1 = {1, 1, 1, 1};
  
  Z = {
    {1, 0, 0, 0},
    {0, 1, 0, 0},
    {0, 0, 1, 0},
    {0, 0, 0, 1},
    {1, 1, 1, 1}
  } / 2;
  
  inverseZ = getInverseZD4[z];
  
  justMap = 1200 * map1.L;
  retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
  preTransformation = Inverse[L].inverseZ;
  preTransformedRetuningMap = retuningMap.preTransformation;
  expectedMaxDamage = Max[Abs[preTransformedRetuningMap]];
  
  Print["retuningMap: ", retuningMap];
  Print["Z: ", Z // MatrixForm];
  Print["inverseZ: ", inverseZ // MatrixForm];
  Print["preTransformation: ", preTransformation // N // MatrixForm];
  Print["confirming duality: ", preTransformation.Z.L // MatrixForm];
  Print["preTransformedRetuningMap: ", preTransformedRetuningMap];
  Print["expectedMaxDamage: ", expectedMaxDamage];
  
  actualMaxDamage = 0;
  worstInterval = {};
  Do[
    interval = RandomInteger[{-20, 20}, 4];
    
    If[i == 1, interval = {6, 0, 1, -3}];
    If[i == 2, interval = {10, 0, 0, -4}];
    If[i == 3, interval = {0, 0, 19, -15}];
    If[i == 4, interval = {18, 0, 14, -18}];
    
    error = retuningMap.interval;
    complexity = Norm[Z.L.interval, 1];
    damage = Quiet[Abs[error] / complexity];
    (*Print["interval: ", interval, " error: ", error, " complexity: ", complexity // N, " damage: ", damage, If[Abs[damage -expectedMaxDamage]<0.001," (max)",""]];*)
    If[damage > actualMaxDamage, actualMaxDamage = damage; worstInterval = interval],
    {i, Range[0, 10000]}
  ];
  
  Print["actualMaxDamage: ", actualMaxDamage];
  Print["worstInterval: ", worstInterval];
];

actuallyGetLilDamageD4["⟨1197.42 1902.74 2780.31 3379.02]", -2]

actuallyGetLilDamageD4["⟨1193.803 1896.996 2771.924 3368.826]", 5.0095]


M = "[⟨34 54 79]}";

L = DiagonalMatrix[Log2[{2, 3, 5}]];
map1 = {1, 1, 1};
Do[
  If[
    z == 0,
    Print["n/a"],
    a = 2 / z;
    inverseZ = {
      {2 - a, -a, -a, a},
      {-a, 2 - a, -a, a},
      {-a, -a, 2 - a, a}
    };
    
    augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[L].inverseZ, "row"};
    tuningMap = optimizeTuningMap[M, "minimax-lil-S"];
    
    justMap = 1200 * map1.L;
    retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
    expectedMaxDamage = Max[Abs[retuningMap.Inverse[L].inverseZ]];
    
    (* Print["z: ", z, " a: ", a, " t: ", tuningMap, " d: ",expectedMaxDamage]*)
    Print[expectedMaxDamage];
  ],
  
  {z, zs}
];

M = "[⟨2 3 5 6] ⟨0 1 -2 -2]}";

L = DiagonalMatrix[Log2[{2, 3, 5, 7}]];
map1 = {1, 1, 1, 1};
Do[
  If[
    z == 0,
    Print["n/a"],
    a = 2 / z;
    inverseZ = getInverseZD4[z];
    augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[L].inverseZ, "row"};
    tuningMap = optimizeTuningMap[M, "minimax-lil-S"];
    
    justMap = 1200 * map1.L;
    retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
    preTransformation = Inverse[L].inverseZ;
    preTransformedRetuningMap = retuningMap.preTransformation;
    expectedMaxDamage = Max[Abs[preTransformedRetuningMap]];
    
    (*  Print["retuningMap: ", retuningMap];
    Print["Z: ", Z // MatrixForm];
    Print["inverseZ: ", inverseZ // MatrixForm];
    Print["preTransformation: ", preTransformation // N // MatrixForm];
    Print["confirming duality: ", preTransformation.Z.L // MatrixForm];
    Print["preTransformedRetuningMap: ", preTransformedRetuningMap];
    Print["expectedMaxDamage: ", expectedMaxDamage];*)
    
    (* Print["z: ", z, " a: ", a, " t: ", tuningMap, " d: ",expectedMaxDamage, " retuningMap: ", retuningMap];*)
    Print[expectedMaxDamage];
  ],
  
  {z, zs}
];

M = "[⟨1 0 0 0 7] ⟨0 1 0 0 1] ⟨0 0 1 0 -1] ⟨0 0 0 1 -1]}";
(* "[⟨1 0 0 0 -4] ⟨0 1 0 0 0] ⟨0 0 1 0 2] ⟨0 0 0 1 1]}"; *)

L = DiagonalMatrix[Log2[{2, 3, 5, 7, 11}]];
map1 = {1, 1, 1, 1, 1};
Do[
  If[
    z == 0,
    Print["n/a"],
    a = 2 / z;
    inverseZ = {
      {2 - a, -a, -a, -a, -a, a},
      {-a, 2 - a, -a, -a, -a, a},
      {-a, -a, 2 - a, -a, -a, a},
      {-a, -a, -a, 2 - a, -a, a},
      {-a, -a, -a, -a, 2 - a, a}
    };
    augmentedEitherSideMultiplierPartArg[simplicityPrescaler_] := {Inverse[L].inverseZ, "row"};
    tuningMap = optimizeTuningMap[M, "minimax-lil-S"];
    
    justMap = 1200 * map1.L;
    retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
    preTransformation = Inverse[L].inverseZ;
    preTransformedRetuningMap = retuningMap.preTransformation;
    expectedMaxDamage = Max[Abs[preTransformedRetuningMap]];
    
    (* Print["z: ", z, " a: ", a, " t: ", tuningMap, " d: ",expectedMaxDamage, " retuningMap: ", retuningMap];*)
    Print[expectedMaxDamage];
  ],
  
  {z, zs}
];



actualMinimum = Minimize[
  {
    parameterizedPseudoinverseDualPreTransformer = getParameterizedPseudoinverseDualPreTransformer[pseudoinverseParameter, d];

    (* override with this particular parameterized pseudoinverse *)
    augmentedEitherSideMultiplierPartArg[na_] := {inverseLogPrimeMatrix.parameterizedPseudoinverseDualPreTransformer, "row"};

    tuningMap = optimizeTuningMap[mapping, "minimax-lil-S"];
    retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
    minimaxDamage = Max[Abs[retuningMap.inverseLogPrimeMatrix.parameterizedPseudoinverseDualPreTransformer]];
    
    minimaxDamage
  },
  pseudoinverseParameter,
  Integers
]




actuallyGetLilDamage[tuningMap_] := Module[
  {},
  
  L = DiagonalMatrix[Log2[{2, 3, 5}]];
  map1 = {1, 1, 1};
  Z = {
    {1, 0, 0},
    {0, 1, 0},
    {0, 0, 1},
    {1, 1, 1}
  } / 2;
  justMap = 1200 * map1.L;
  retuningMap = getL[parseTemperamentData[tuningMap]] - justMap;
  
  actualMaxDamage = 0;
  worstInterval = {};
  Do[
    interval = RandomInteger[{-20, 20}, 3];
    error = retuningMap.interval;
    complexity = Norm[Z.L.interval, 1];
    damage = Quiet[Abs[error] / complexity];
    If[damage > actualMaxDamage, actualMaxDamage = damage; worstInterval = interval],
    {i, Range[0, 10000]}
  ];
  
  Print["actualMaxDamage: ", actualMaxDamage];
  Print["worstInterval: ", worstInterval];
];
actuallyGetLilDamage["⟨1194.134 1890.713 2786.314]"]


In[500]:= format = "EBK";
optimizeTuningMap["⟨12 19 28]", "minimax-lil-S"]

Out[501]= ⟨1194.134 1890.713 2786.314]






(* THIS DOESNT REALLY WORK BECAUSE THE TUNING CHANGES FROM POINT TO POINT WHICH IS WHY IT GIVES HUGE DAMAGE AMOUNTS *)
In[540]:= (* yes, this actually has nothing to do with the mapping at all, only the final tuning map *)
    getSpikeTipForKnownMinMinimaxDamageTuningFromAugmentedMethod[unparsedTuningMap_, pseudoinverseParameterForMinMinimaxDamage_] := Module[
      {tuningMap, d, logPrimeMatrix, partiallyPreTransformedRetuningMap},
      
      tuningMap = getL[parseTemperamentData[unparsedTuningMap]];
      d = Length[tuningMap];
      logPrimeMatrix = getLogPrimeMatrix[d];
      partiallyPreTransformedRetuningMap = (tuningMap - 1200 * getSummationMap[d].logPrimeMatrix).Inverse[logPrimeMatrix];
      
      NMaximize[
        {
          Max[Abs[partiallyPreTransformedRetuningMap.getParameterizedPseudoinverseDualPreTransformer[pseudoinverseParameter, d]]],
          pseudoinverseParameterForMinMinimaxDamage - 1 < pseudoinverseParameter <  pseudoinverseParameterForMinMinimaxDamage + 1
        },
        pseudoinverseParameter,
        WorkingPrecision -> 8
      ]
    ];

In[541]:= getSpikeTipForKnownMinMinimaxDamageTuningFromAugmentedMethod["⟨1200.000 1894.737 2778.947]", 3.39334]

Out[541]= {6.4568532,{pseudoinverseParameter->2.39334}}



FindFormula[{
  7.11877,
  7.11894,
  7.11913,
  7.11932,
  7.11953,
  7.11977,
  7.12002,
  7.12029,
  7.12059,
  7.12092,
  7.12128,
  7.12168,
  7.12214,
  7.12264,
  7.12322,
  7.12388,
  7.12464,
  7.12552,
  7.12657,
  7.12783,
  7.12937,
  7.13129,
  7.13377,
  7.24264,
  7.45086,
  7.6714,
  7.9054,
  8.15412,
  8.419,
  8.63903,
  8.36035,
  8.09909,
  7.85366,
  7.62267,
  7.40488,
  7.19919,
  7.09437,
  7.09682,
  7.09873,
  7.10025,
  7.1015,
  7.10254,
  7.10342,
  7.10418,
  7.10483,
  7.10541,
  7.10591,
  7.10636,
  7.10677,
  7.10713,
  7.10746,
  7.10775,
  7.10802,
  7.10827,
  7.1085,
  7.10872,
  7.10891,
  7.10909,
  7.10926,
  7.10942,
  7.10957,
  7.10971,
  7.10984,
  7.10996,
  7.11008,
  7.11019,
  7.11029,
  7.11039,
  7.11048,
  7.11057,
  7.11065,
  7.11073,
  7.11081,
  7.11088,
  7.11095,
  7.11102,
  7.11108,
  7.11114,
  7.1112,
  7.11126,
  7.11131,
  7.11136,
  7.11141,
  7.11146,
  7.11151,
  7.11155,
  7.1116,
  7.11164,
  7.11168,
  7.11172,
  7.11176,
  7.11179,
  7.11183,
  7.11186,
  7.11189,
  7.11193,
  7.11196,
  7.11199,
  7.11202,
  7.11205}, PerformanceGoal -> "Quality"]