(* MINIMAX *)

(* based on https://github.com/keenanpepper/tiptop/blob/main/tiptop.py *)
optimizeGeneratorsTuningMapSemianalyticalMaxPolytope[t_, targetedIntervalsA_, damageWeightsOrComplexityMultiplier_] := Module[
  {
    tuningMappings,
    ma,
    tuningMap,
    primesTuningMap,
    
    mappedSide,
    justSide,
    generatorCount,
    maxCountOfNestedMinimaxibleDamages,
    minimaxTunings,
    minimaxLockForMappedSide,
    minimaxLockForJustSide,
    undoMinimaxLocksForMappedSide,
    undoMinimaxLocksForJustSide,
    uniqueOptimalTuning
  },
  
  tuningMappings = getTuningMappings[t];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  mappedSide = Transpose[ma.Transpose[targetedIntervalsA].damageWeightsOrComplexityMultiplier];
  justSide = Transpose[{primesTuningMap.Transpose[targetedIntervalsA].damageWeightsOrComplexityMultiplier}];
  
  (*   
  our goal is to find the generator tuning map not merely with minimaxed damage, 
  but where the next-highest damage is minimaxed as well, and in fact every next-highest damage is minimaxed, all the way down.
  the tuning which has all damages minimaxed within minimaxed all the way down like this we can call a "nested-minimax".
  it's the only sensible optimum given a desire for minimax damage, so in general we can simply still call it "minimax".
  though people have sometimes distinguished this tuning from the range of minimax tunings with a prefix, 
  such as "TIPTOP tuning" versus "TOP tunings", although there is no value in "TOP tunings" given the existence of "TIPTOP",
  so you may as well just keep calling it "TOP" and refine its definition. anyway...
  
  the `findAllNestedMinimaxTuningsFromPolytopeVertices` function this function calls may come back with more than one result. 
  the clever way we compute a nested-minimax uses the same polytope vertex searching method used for the that first pass, but now with a twist.
  so in the basic case, this method finds the vertices of a tuning polytope for a temperament.
  this is the area inside of which the targeted intervals are as close as possible to just (by the definition of minimax damage, anyway).
  so now, instead of running it on the case of the original temperament versus JI, we run it on a distorted version of this case.
  specifically, we run it on a case distorted so that the previous minimaxes are locked down.
  
  we achieve this by picking one of these minimax tunings and offset the just side by it. 
  it doesn't matter which minimax tuning we choose, by the way; they're not sorted, and we simply take the first one.
  the corresponding distortion to the mapped side is trickier, 
  involving the differences between this arbitrarily-chosen minimax tuning and each of the other minimax tunings.
  note that after this distortion, the original rank and dimensionality of the temperament will no longer be recognizable.
  
  we then we search for polytope vertices of this minimax-locked distorted situation.
  and we repeatedly do this until we eventually find a unique, nested-minimax optimum. 
  once we've done that, though, our result isn't in the form of a generators tuning map yet. it's still distorted.
  well, with each iteration, we've been keeping track of the distortion applied, so that in the end we could undo them all.
  after undoing those, voilà, we're done!
  *)
  
  (* the mapped and weighted targeted intervals on one side, and the just and weighted targeted intervals on the other;
  note that just side goes all the way down to tuning map level (logs of primes) 
  while the tempered side isn't tuned, but merely mapped. that's so we can solve for the rest of it, i.e. its tunings *)
  
  (* the same as rank here, but named this for correlation with elsewhere in this code *)
  generatorCount = Last[Dimensions[mappedSide]];
  
  (* this is too complicated to be explained here and will be explained later *)
  maxCountOfNestedMinimaxibleDamages = 0;
  
  (* the candidate generator tuning maps which minimax damage to the targets*)
  minimaxTunings = findAllNestedMinimaxTuningsFromPolytopeVertices[mappedSide, justSide, maxCountOfNestedMinimaxibleDamages];
  maxCountOfNestedMinimaxibleDamages = generatorCount + 1;
  
  (* no minimax-damage-locking transformations yet, so the transformation trackers are identities 
  per their respective operations of matrix multiplication and addition *)
  undoMinimaxLocksForMappedSide = IdentityMatrix[generatorCount];
  undoMinimaxLocksForJustSide = Table[{0}, generatorCount];
  
  While[
    (* a unique optimum has not yet been found *)
    Length[minimaxTunings] > 1,
    
    (* arbitrarily pick one of the minimax damage generator tuning maps; the first one from this unsorted list *)
    minimaxLockForJustSide = First[minimaxTunings];
    (* list of differences between each other minimax generator tuning map and the first one; 
    note how the range starts on index 2 in order to skip the first one *)
    minimaxLockForMappedSide = Map[Flatten, Transpose[Map[
      Part[minimaxTunings, #] - minimaxLockForJustSide&,
      Range[2, Length[minimaxTunings]]
    ]]];
    
    (* apply the minimax-damage-locking transformation to the just side, and track it to undo later *)
    justSide -= mappedSide.minimaxLockForJustSide;
    undoMinimaxLocksForJustSide += undoMinimaxLocksForMappedSide.minimaxLockForJustSide;
    
    (* apply the minimax-damage-locking transformation to the mapped side, and track it to undo later *)
    (* this would be a .= if Wolfram supported an analog to += and -= *)
    (* unlike how it is with the justSide, the undo operation is not inverted here; 
    that's because we essentially invert it in the end by left-multiplying rather than right-multiplying *)
    mappedSide = mappedSide.minimaxLockForMappedSide;
    undoMinimaxLocksForMappedSide = undoMinimaxLocksForMappedSide.minimaxLockForMappedSide;
    
    (* search again, now in this transformed state *)
    minimaxTunings = findAllNestedMinimaxTuningsFromPolytopeVertices[mappedSide, justSide, maxCountOfNestedMinimaxibleDamages];
    maxCountOfNestedMinimaxibleDamages += generatorCount + 1;
  ];
  
  uniqueOptimalTuning = First[minimaxTunings];
  SetAccuracy[Flatten[
    (* here's that left-multiplication mentioned earlier *)
    undoMinimaxLocksForMappedSide.uniqueOptimalTuning + undoMinimaxLocksForJustSide
  ], 10]
];

findAllNestedMinimaxTuningsFromPolytopeVertices[mappedSide_, justSide_, maxCountOfNestedMinimaxibleDamages_] := Module[
  {
    targetCount,
    generatorCount,
    nthmostMinDamage,
    vertexConstraintAs,
    targetIndices,
    candidateTunings,
    sortedDamagesByCandidateTuning,
    candidateTuning,
    sortedDamagesForThisCandidateTuning,
    newCandidateTunings,
    newSortedDamagesByCandidateTuning
  },
  
  (* in the basic case where no minimax-damage-locking transformations have been applied, 
  these will be the same as the count of original targeted intervals and the rank of the temperament, respectively *)
  targetCount = First[Dimensions[mappedSide]];
  generatorCount = Last[Dimensions[mappedSide]];
  
  (* here's the meat of it: solving a linear problem for each vertex of the of tuning polytope *)
  candidateTunings = {};
  vertexConstraintAs = getTuningPolytopeVertexConstraintAs[generatorCount, targetCount];
  Do[
    AppendTo[
      candidateTunings,
      Quiet[Check[
        LinearSolve[N[vertexConstraintA.mappedSide, 10], N[vertexConstraintA.justSide, 10]],
        "err"
      ]
      ]],
    {vertexConstraintA, vertexConstraintAs}
  ];
  (* ignore the problems that are singular and therefore have no solution *)
  candidateTunings = Select[candidateTunings, !TrueQ[# == "err"]&];
  
  (* each damages list is sorted in descending order; 
  the list of lists itself is sorted corresponding to the candidate tunings*)
  sortedDamagesByCandidateTuning = Quiet[Map[
    Function[
      {candidateTuning},
      (* note that because of being sorted by size, this is no longer sorted by which target the damage applies to *)
      ReverseSort[SetAccuracy[Flatten[Abs[mappedSide.candidateTuning - justSide]], 10]]
    ],
    candidateTunings
  ]];
  
  (*     
  here we're iterating by index of the targeted intervals, 
  repeatedly updating the lists candidate tunings and their damages,
  (each pass the list gets shorter, hopefully eventually hitting length 1, at which point a unique tuning has been found,
  but this doesn't necessarily happen, and if it does, it's handled by the function that calls this function)
  until by the final pass they are what we want to return.
  
  there's an inner loop by candidate tuning, and since that list is shrinking each time, the size of the inner loop changes.
  in other words, we're not covering an m \[Times] n rectangular grid's worth of possibilities; more like a jagged triangle.
  
  note that because the damages have all been sorted in descending order,
  these target "indices" do not actually correspond to an individual targeted interval.
  that's okay though because here it's not important which target each of these damages is for.
  all that matters is the size of the damages.
  once we find the tuning we want, we can easily compute its damages list sorted by target when we need it later; that info is not lost.
  
  and note that we don't iterate over *every* target "index".
  we only check as many targets as we could possibly nested-minimax by this point.
  that's why this method doesn't simply always return a unique nested-minimax tuning each time.
  this is also why the damages have been sorted in this way
  so first we compare each tuning's actual minimum damage,
  then we compare each tuning's second-closest-to-minimum damage,
  then compare each third-closest-to-minimum, etc.
  the count of target indices we iterate over is a running total; 
  each time it is increased, it goes up by the present generator count plus 1.
  why it increases by that amount is a bit of a mystery to me, but perhaps someone can figure it out and let me know.
  *)
  targetIndices = Range[Min[maxCountOfNestedMinimaxibleDamages + generatorCount + 1, targetCount]];
  Do[
    newCandidateTunings = {};
    newSortedDamagesByCandidateTuning = {};
    
    (* this is the nth-most minimum damage across all candidate tunings,
    where the actual minimum is found in the 1st index, the 2nd-most minimum in the 2nd index,
    and we index it by target index *)
    nthmostMinDamage = Min[Map[Part[#, targetIndex]&, sortedDamagesByCandidateTuning]];
    
    Do[
      (* having found the minimum damage for this target index, we now iterate by candidate tuning index *)
      candidateTuning = Part[candidateTunings, minimaxTuningIndex];
      sortedDamagesForThisCandidateTuning = Part[sortedDamagesByCandidateTuning, minimaxTuningIndex];
      
      If[
        (* and if this is one of the tunings which is tied for this nth-most minimum damage,
        add it to the list of those that we'll check on the next iteration of the outer loop 
        (and add its damages to the corresponding list) 
        note the tiny tolerance factor added to accommodate computer arithmetic error problems *)
        Part[sortedDamagesForThisCandidateTuning, targetIndex] <= nthmostMinDamage + 0.000000001,
        
        AppendTo[newCandidateTunings, candidateTuning];
        AppendTo[newSortedDamagesByCandidateTuning, sortedDamagesForThisCandidateTuning]
      ],
      
      {minimaxTuningIndex, Range[Length[candidateTunings]]}
    ];
    
    candidateTunings = newCandidateTunings;
    sortedDamagesByCandidateTuning = newSortedDamagesByCandidateTuning,
    
    {targetIndex, targetIndices}
  ];
  
  (* if duplicates are not deleted, then when differences are checked between tunings,
  some will come out to all zeroes, and this causes a crash *)
  DeleteDuplicates[candidateTunings]
];

(* TODO: explain what happens in here and name variables better *)
getTuningPolytopeVertexConstraintAs[generatorCount_, targetCount_] := Module[
  {vertexConstraintA, vertexConstraintAs, targetCombinations},
  
  vertexConstraintAs = {};
  
  targetCombinations = DeleteDuplicates[Map[Sort, Select[Tuples[Range[1, targetCount], generatorCount + 1], DuplicateFreeQ[#]&]]];
  
  Do[
    signPermutations = Tuples[{1, -1}, generatorCount];
    
    Do[
      vertexConstraintA = Table[Table[0, targetCount], generatorCount];
      
      Do[
        vertexConstraintA[[i, Part[targetCombination, 1]]] = 1;
        vertexConstraintA[[i, Part[targetCombination, i + 1]]] = Part[signPermutation, i],
        
        {i, Range[generatorCount]}
      ];
      
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {signPermutation, signPermutations}
    ],
    
    {targetCombination, targetCombinations}
  ];
  
  If[
    generatorCount == 1,
    Do[
      vertexConstraintA = {Table[0, targetCount]};
      vertexConstraintA[[1, i]] = 1;
      
      AppendTo[vertexConstraintAs, vertexConstraintA],
      
      {i, Range[targetCount]}
    ]
  ];
  
  (* count should be the product of the indices count and the signs count, plus the r == 1 ones *)
  vertexConstraintAs
];


(* MINISUM *)

optimizeGeneratorsTuningMapAnalyticalSumPolytope[tuningOptions_] := Module[
  {
    t,
    targetedIntervalsA,
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
    r,
    unchangedIntervalSetIndices,
    potentialUnchangedIntervalSets,
    normalizedPotentialUnchangedIntervalSets,
    filteredNormalizedPotentialUnchangedIntervalSets,
    potentialProjectionAs,
    potentialTuningMaps,
    potentialTuningMapDamages,
    minDamageTuningMapIndices,
    minDamageTuningMapIndex,
    minDamageProjectionA,
    generatorsPreimageTransversal,
    projectedGenerators
  },
  
  t = tuningOption[tuningOptions, "t"];
  targetedIntervalsA = tuningOption[tuningOptions, "targetedIntervalsA"];
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  r = getR[t];
  unchangedIntervalSetIndices = Subsets[Range[Length[targetedIntervalsA]], {r}];
  potentialUnchangedIntervalSets = Map[Map[targetedIntervalsA[[#]]&, #]&, unchangedIntervalSetIndices];
  normalizedPotentialUnchangedIntervalSets = Map[canonicalCa, potentialUnchangedIntervalSets];
  filteredNormalizedPotentialUnchangedIntervalSets = DeleteDuplicates[Select[normalizedPotentialUnchangedIntervalSets, MatrixRank[#] == r&]];
  potentialProjectionAs = Select[Map[
    getProjectionAFromUnchangedIntervals[t, #]&,
    filteredNormalizedPotentialUnchangedIntervalSets
  ], Not[# === Null]&];
  potentialTuningMaps = Map[primesTuningMap.#&, potentialProjectionAs];
  potentialTuningMapDamages = Map[getSumDamage[#, tuningOptions]&, potentialTuningMaps];
  
  minDamageTuningMapIndices = Position[potentialTuningMapDamages, Min[potentialTuningMapDamages]];
  If[
    Length[minDamageTuningMapIndices] == 1,
    
    (* result is unique; done *)
    minDamageTuningMapIndex = First[First[Position[potentialTuningMapDamages, Min[potentialTuningMapDamages]]]];
    minDamageProjectionA = potentialProjectionAs[[minDamageTuningMapIndex]];
    generatorsPreimageTransversal = Transpose[getA[getGeneratorsPreimageTransversal[t]]];
    projectedGenerators = minDamageProjectionA.generatorsPreimageTransversal;
    primesTuningMap.projectedGenerators,
    
    (* result is not unique; fallback to numerical solution *)
    (* note this only happens for minimax, not for minisum or other powers *)
    optimizeGeneratorsTuningMapMinisopNonunique[tuningOptions]
  ]
];

getProjectionAFromUnchangedIntervals[t_, unchangedIntervalEigenvectors_] := Module[
  {commaEigenvectors, eigenvectors, diagonalEigenvalueA},
  
  commaEigenvectors = getA[getC[t]];
  eigenvectors = Transpose[Join[unchangedIntervalEigenvectors, commaEigenvectors]];
  
  diagonalEigenvalueA = getDiagonalEigenvalueA[unchangedIntervalEigenvectors, commaEigenvectors];
  
  If[Det[eigenvectors] == 0, Null, eigenvectors.diagonalEigenvalueA.Inverse[eigenvectors]]
];

getDiagonalEigenvalueA[unchangedIntervalEigenvectors_, commaEigenvectors_] := DiagonalMatrix[Join[
  Table[1, Length[unchangedIntervalEigenvectors]],
  Table[0, Length[commaEigenvectors]]
]];


(* MINISOS *)

optimizeGeneratorsTuningMapWithPseudoInverse[
  t_,
  potentiallyPrimesIdentityTargetedIntervalsA_,
  damageWeightingOrDualMultiplier_
] := Module[
  {
    tuningMappings,
    generatorsTuningMap,
    ma,
    tuningMap,
    primesTuningMap,
    weightedOrMultipliedTargetedIntervalsAMapped,
    generatorsA
  },
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  ma = Part[tuningMappings, 2];
  tuningMap = Part[tuningMappings, 3];
  primesTuningMap = Part[tuningMappings, 4];
  
  weightedOrMultipliedTargetedIntervalsAMapped = ma.
      Transpose[potentiallyPrimesIdentityTargetedIntervalsA].
      damageWeightingOrDualMultiplier;
  generatorsA = Transpose[potentiallyPrimesIdentityTargetedIntervalsA].
      damageWeightingOrDualMultiplier.
      Transpose[weightedOrMultipliedTargetedIntervalsAMapped].
      Inverse[
        weightedOrMultipliedTargetedIntervalsAMapped.Transpose[weightedOrMultipliedTargetedIntervalsAMapped]
      ];
  
  generatorsTuningMap = primesTuningMap.generatorsA;
  
  generatorsTuningMap
];


(* OTHER POWERS *)

optimizeGeneratorsTuningMapNumericalPowerSolver[tuningOptions_, absErrorL_, normPower_] := Module[
  {
    t,
    unchangedIntervals,
    complexityMakeOdd,
    
    tuningMappings,
    generatorsTuningMap,
    
    periodsPerOctave,
    minimizedNorm,
    solution
  },
  
  t = tuningOption[tuningOptions, "t"];
  unchangedIntervals = tuningOption[tuningOptions, "unchangedIntervals"];
  complexityMakeOdd = tuningOption[tuningOptions, "complexityMakeOdd"];
  
  tuningMappings = getTuningMappings[t];
  generatorsTuningMap = Part[tuningMappings, 1];
  
  periodsPerOctave = getPeriodsPerOctave[t];
  
  minimizedNorm = If[
    Length[unchangedIntervals] > 0 || complexityMakeOdd == True,
    {Norm[absErrorL, normPower], generatorsTuningMap[[1]] == 1 / periodsPerOctave},
    Norm[absErrorL, normPower]
  ];
  solution = NMinimize[minimizedNorm, generatorsTuningMap, WorkingPrecision -> 128];
  generatorsTuningMap /. Last[solution]
];
