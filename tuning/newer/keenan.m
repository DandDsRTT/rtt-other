constraintMatrices[r_, c_] := Module[
  {ret, rets},
  
  rets = {};
  
  Do[
    Do[
      ret = Table[Table[0, c], r];
      
      Do[
        ret[[ i, Part[indices, 1]]] = 1;
        ret[[ i, Part[indices, i + 1]]] = Part[signs, i],
        
        {i, Range[r]}
      ];
      
      AppendTo[rets, ret],
      
      {signs, Tuples[{1, -1}, r]}
    ],
    
    {indices, DeleteDuplicates[Map[Sort, Select[Tuples[Range[1, c], r + 1], DuplicateFreeQ[#]&]]]}
  ];
  
  If[
    r == 1,
    
    Do[
      ret = {Table[0, c]};
      ret[[1, i]] = 1;
      
      AppendTo[rets, ret],
      
      {i, Range[c]}
    ]
  ];
  
  rets
];

candidates[A_, b_, tol_, numAlready_] := Module[
  {shape, n, m, xs, residuals, minError, newXs, newRes(*, x*)},
  
  shape = Dimensions[A];
  n = First[shape];
  m = Last[shape];
  
  xs = {};
  Do[
    (*Print["hALP"];*)
    AppendTo[xs, Quiet[Check[LinearSolve[C.A, C.b], "err"]]],
    (*Print["got an x ", x];
    Print["and something else"];
    If[
    x =="failedToSolve",
    
    Print["fuck you", x],
    
    Print["woo hoo", x];
    AppendTo[xs, x]
    (*x ==  "failedToSolve",
    Print["this fao;e"],
    AppendTo[xs,x];
    Print["success"]*)*)
    (*]*)
    (*],*)
    {C, constraintMatrices[m, n]}
  ];
  (*Print["before", xs];*)
  xs = Select[xs, !TrueQ[# == "err"]&];
  (*Print["and so xs ", xs];*)
  
  residuals = Quiet[ Map[Function[{x}, ReverseSort[N[Flatten[Abs[A.x - b]], 16]]], xs]];
  (*Print["now i could swore this was something. and it appears to be! ", N[residuals], " and its lenght is ", Length[residuals]];*)
  
  Do[
    (*Print["this i ", i];
    Print["okay well first of all", Range[Length[residuals]]];
    Print["i just cna't figure out what this is ",N[ Map[Function[{j}, Part[residuals,j,i]],Range[Length[residuals]]]]];*)
    minError = Min[Map[Function[{j}, Part[residuals, j, i]], Range[Length[residuals]]]];
    (*Print["minError ",N[minError]];*)
    newXs = {};
    newRes = {};
    Do[
      (*Print[N[Part[residuals,j,i]], " <= than ", minError + tol];*)
      If[
        Part[residuals, j, i] <= minError + tol,
        (*Print["YES DIDIT"];*)
        AppendTo[newXs, Part[xs, j]];
        AppendTo[newRes, Part[residuals, j]]
      ],
      
      {j, Range[Length[xs]]}
    ];
    xs = newXs;
    residuals = newRes,
    
    {i, Range[Min[numAlready + m + 1, n]]}
  ];
  
  xs
];

tiptop[primes_, mapping_, tol_] := Module[
  {weight, A, b, xs, ABack, bBack, X},
  
  weight = DiagonalMatrix[1 / Log2[primes]];
  A = weight.Transpose[mapping] / 1200;
  b = Table[{1}, Last[Dimensions[mapping]]];
  xs = candidates[A, b, tol, 0];
  numAlready = Last[Dimensions[A]] + 1;
  (*Print["weight: ",N[ weight]];
  Print["A: ", N[A]];
  Print["b: ", N[b]];*)
  
  ABack = IdentityMatrix[Last[Dimensions[A]]];
  bBack = Table[{0}, Last[Dimensions[A]]];
  (*Print["ABack: ", ABack];
  Print["bBack: ", bBack];*)
  
  While[
    Length[xs] > 1,
    
    (*Print["this is happening"];*)
    b = b - A.First[xs];
    (*Print["b ",N[b]];*)
    
    X = Flatten[Map[Part[xs, #] - Part[xs, 1]&, Range[2, Length[xs]]], 1];
    (*Print["X ",N[X]];*)
    
    A = A.X;
    (*Print["well this cand be where it is can it, because xs gotta be betigger than 1"];*)
    bBack = ABack.First[xs] + bBack;
    ABack = ABack.X;
    
    xs = candidates[A, b, tol, numAlready];
    numAlready += Last[Dimensions[A]] + 1;
  ];
  
  (*Print["we are just happening here thern?"];*)
  {ABack.First[xs] + bBack, A.First[xs] - b}
];

doKeenanTuning[primes_, mapping_, tol_] := Module[{},
  result = tiptop[primes, mapping, tol];
  generators = Part[result, 1];
  residual = Part[result, 2];
  
  Print["The TOP generators are: "];
  Print[N[generators,16]];
  Print["The errors of each prime are: "];
  Print[Quiet[Map[Function[{i},Log2[Part[primes,i]]*1200*N[Part[residual, i],16]], Range[Length[residual]]]]];
  Print["(Or weighted, in cents per octave:)"]; (* TODO: note to Dave another example of cents per octave *)
  Print[Quiet[Map[1200*N[#,16]&,residual]]];
  Print["So the TOP damage is: ", Quiet[1200*N[Max[Abs[residual]],16]], " cents per octave."];
];

(*mapping = {{2,3,5,6},{0,1,-2,-2}};
primes = {2,3,5,7};*)

mapping = {{12, 19, 28}};
primes = {2, 3, 5};

(*mapping = {{1, 0, 0, 0}, {0, 1, 1, 2}, {0, 0, 2, -1}};
primes = {2,3,5,7};
mapping = {{3,0,7,18},{0,1,0,-2}};
primes = {2,3,5,7};*)

tol = 0.000000001;

doKeenanTuning[primes, mapping, tol]





keenanBinding[
  t_,
  unchangedIntervals_, (* trait -1 *)
  targetedIntervalsA_, (* trait 0 *)
  optimizationPower_, (* trait 1 *)
  damageWeightingSlope_, (* trait 2 *)
  complexityNormPower_, (* trait 3 *)
  complexityNegateLogPrimeCoordination_, (* trait 4a *)
  complexityPrimePower_, (* trait 4b *)
  complexitySizeFactor_, (* trait 4c *)
  complexityMakeOdd_ (* trait 4d *)
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    tuningMap,
    
    damagesMagnitude,
    constraints,
    
    damagesL,
    normFn,
    normPower,
    normPowerLimit,
    periodsPerOctave,
    minimizedNorm,
    solution,
    
    prevMax,
    thisMax,
    newConstraints,
    resolvedDamagesLFilteredForPreviousMax,
    maxDamageTargetedIntervalIndices,
    constrainedIndices
  },
  
  tuningMappings = getTuningSchemeMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  tuningMap = Part[tuningMappings, 3];
  
  damagesL = getTargetedIntervalDamagesL[
    tuningMap,
    t,
    targetedIntervalsA, (* trait 0 *)
    damageWeightingSlope, (* trait 2 *)
    complexityNormPower, (* trait 3 *)
    complexityNegateLogPrimeCoordination, (* trait 4a *)
    complexityPrimePower, (* trait 4b *)
    complexitySizeFactor, (* trait 4c *)
    complexityMakeOdd (* trait 4d *)
  ];
  normFn = Norm;
  normPower = optimizationPower; (* TODO: this is always \[Infinity] *)
  prevMax = \[Infinity];
  constrainedIndices = {};
  
  periodsPerOctave = getPeriodsPerOctave[t];
  constraints = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    {}
  ];
  
  (* Print[Map[pcvToQuotient, targetedIntervalsA]];*)
  
  While[
    Length[constraints] < Length[damagesL],
    
    (*Print["going for a go. ",  Length[constraints], "/", Length[damagesL], " constraints set."];*)
    minimizedNorm = Join[{normFn[damagesL, normPower]}, constraints];
    (* Print["constraints: ", N[constraints, 5]];*)
    
    solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 8];
    
    newConstraints = {};
    
    resolvedDamagesLFilteredForPreviousMax = damagesL /. Last[solution];
    
    If[
      Length[constrainedIndices] > 0, (* TODO: this doesn't seem necessary *)
      Map[
        Function[
          {constrainedIndex},
          resolvedDamagesLFilteredForPreviousMax[[constrainedIndex]] = 0;
        ],
        
        constrainedIndices
      ]
    ];
    
    thisMax = Max[resolvedDamagesLFilteredForPreviousMax];
    damagesMagnitude = First[solution];
    prevMax = damagesMagnitude;
    maxDamageTargetedIntervalIndices = Flatten[ Position[resolvedDamagesLFilteredForPreviousMax, el_ /; Abs[el - thisMax] < 0.0001]]; (* TODO: maybe tie that 256 constant to a global accuracy setting somewhere *)
    
    
    Map[
      AppendTo[newConstraints, Part[damagesL, #] == thisMax]&,
      maxDamageTargetedIntervalIndices
    ];
    
    constrainedIndices = Join[constrainedIndices, maxDamageTargetedIntervalIndices];
    
    (* Print["alrighty then. so we've got resolvedDamagesL: ", N[resolvedDamagesLFilteredForPreviousMax, 5], " and constrainedIndices: ", constrainedIndices];*)
    
    constraints = SetPrecision[Join[constraints, newConstraints], 10]
  ];
  
  generatorsTuningMap /. Last[solution]
];
