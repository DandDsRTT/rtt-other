In[580]:= diff5[g1_,g2_,pcv_] := Abs[{g1, g2}.getA[meantone].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, meantone];

(*optimizeGeneratorsTuningMap[meantone, "systematicTuningSchemeName" -> "minimax-ZS"]

getPcvLogIntegerLimitComplexity[{1,0,0}, meantone]
getPcvLogIntegerLimitComplexity[{0, 1,0}, meantone]
getPcvLogIntegerLimitComplexity[{0,0,1}, meantone]
*)
randos = {};
Do[
  rando = Table[RandomInteger[{-5,5}],3];
  AppendTo[randos, rando],
  100
];
(*randos
(*Max[{1,2,3}]
Map[diff5[g1,g2,#]&,randos]*)

Map[getPcvLogIntegerLimitComplexity[#, meantone]&, randos]*)

Plot3D[
  Max[
    Map[diff5[g1,g2,#]&,randos]
    (*diff5[g1,g2,{1,0,0}],
    diff5[g1,g2,{0,1,0}],
     diff5[g1,g2,{0,0,1}]*)
  ],
  {g1,1190,1210},
  {g2,690,710},
  ImageSize -> 1000(*,
  MaxRecursion ->3*)
]

In[1041]:= getDamage[meantone,{1200.`7.079181246047626,696.5784284662087202378`6.842970021165644}, "systematicTuningSchemeName" -> "minimax-ZS"]
getDamage[meantone,{1201.8,697.1}, "systematicTuningSchemeName" -> "minimax-ZS"]
getDamage[meantone, {1201.6985204945660825615`7.079795526478642,697.5643890802275564056`6.8435843015966595}, "systematicTuningSchemeName" -> "minimax-ZS"]

In[1047]:= diff5[g1_,g2_,pcv_] := Abs[{g1, g2}.getA[blackwood].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, blackwood];

result = optimizeGeneratorsTuningMap[blackwood, "systematicTuningSchemeName" -> "minimax-ZS"]

Plot3D[
  Max[
    diff5[g1,g2,{1,0,0}],
    diff5[g1,g2,{0,1,0}],
    diff5[g1,g2,{0,0,1}]
  ],
  {g1,Part[result,1]-10,Part[result,1]+10},
  {g2,Part[result,2]-10,Part[result,2]+10},
  ImageSize -> 1000
]

In[1087]:= diff5[g1_,g2_,pcv_] := Abs[{g1, g2}.getA[meantone].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, meantone];

result = optimizeGeneratorsTuningMap[meantone, "systematicTuningSchemeName" -> "minimax-ZES"]
damage = getDamage[meantone, result, "systematicTuningSchemeName" -> "minimax-ZES"]

mikeResult = {1201.391,697.045}
mikeDamage = getDamage[meantone, mikeResult, "systematicTuningSchemeName" -> "minimax-ZES"]

Show[
  Plot3D[
    Sqrt[
      Power[diff5[g1,g2,{1,0,0}],2] +
          Power[diff5[g1,g2,{0,1,0}],2] +
          Power[diff5[g1,g2,{0,0,1}],2]
    ],
    {g1,Part[result,1]-10,Part[result,1]+10},
    {g2,Part[result,2]-10,Part[result,2]+10},
    ImageSize -> 1000
  ],
  Graphics3D[{Black, PointSize[0.01], Point[Join[result,{damage}]],Red, PointSize[0.01], Point[Join[mikeResult, {mikeDamage}]]}]
]


In[1101]:= diff5[g1_,g2_,pcv_] := Abs[{g1, g2}.getA[meantone].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, meantone];SetAccuracy[NMinimize[Min[Sqrt[
  Power[diff5[g1,g2,{1,0,0}],2] +
      Power[diff5[g1,g2,{0,1,0}],2] +
      Power[diff5[g1,g2,{0,0,1}],2]
]],{g1,g2}],4]

In[1108]:= diff5[g1_,g2_,pcv_] := Abs[{g1, g2}.getA[blackwood].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, blackwood];

result = optimizeGeneratorsTuningMap[blackwood, "systematicTuningSchemeName" -> "minimax-ZES"]
damage = getDamage[blackwood, result, "systematicTuningSchemeName" -> "minimax-ZES"]

mikeResult = {238.8508, 2786.189}
mikeDamage = getDamage[blackwood, mikeResult, "systematicTuningSchemeName" -> "minimax-ZES"]

Show[
  Plot3D[
    Sqrt[
      Power[diff5[g1,g2,{1,0,0}],2] +
          Power[diff5[g1,g2,{0,1,0}],2] +
          Power[diff5[g1,g2,{0,0,1}],2]
    ],
    {g1,Part[result,1]-10,Part[result,1]+10},
    {g2,Part[result,2]-10,Part[result,2]+10},
    ImageSize -> 1000
  ],
  Graphics3D[{Black, PointSize[0.01], Point[Join[result,{damage}]],Red, PointSize[0.01], Point[Join[mikeResult, {mikeDamage}]]}]
]


In[1114]:= diff5[g1_,g2_,pcv_] := Abs[{g1, g2}.getA[blackwood].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, blackwood];SetAccuracy[NMinimize[Min[Sqrt[
  Power[diff5[g1,g2,{1,0,0}],2] +
      Power[diff5[g1,g2,{0,1,0}],2] +
      Power[diff5[g1,g2,{0,0,1}],2]
]],{g1,g2}],4]

In[1306]:= randos = {};
Do[
  rando = Table[RandomInteger[{-5,5}],3];
  AppendTo[randos, rando],
  1000
];
randos


myTuning = {1201.699,697.564};
mikeTuning = {1200.0, 696.578};
myTuningWithPseudo = {1203.402, 698.553};

Map[Abs[myTuning.getA[meantone].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, meantone]&, randos];
Max[%]
Map[Abs[mikeTuning.getA[meantone].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, meantone]&, randos];
Max[%]
Map[Abs[myTuningWithPseudo.getA[meantone].pcv - 1200 * Log2[{2, 3, 5}].pcv] / getPcvLogIntegerLimitComplexity[pcv, meantone]&, randos];
Max[%]
