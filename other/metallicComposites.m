getMetallicMean[n_] := (n + Power[Power[n, 2] + 4, 1 / 2]) / 2;

getMetallicCompositesUpTo[maxCandidateIndex_] := Module[
  {sequence, metallicMeanIndex, metallicMean, oddPower, candidateIndex},
  
  sequence = {};
  metallicMeanIndex = 1;
  
  While[
    True,
    
    (* skip metallic means already shown to be a power of another *)
    If[MemberQ[sequence, metallicMeanIndex], metallicMeanIndex++];
    
    metallicMean = getMetallicMean[metallicMeanIndex];
    oddPower = 3;
    
    While[
      True,
      
      candidateIndex = Floor[Power[metallicMean, oddPower]];
      
      If[
        candidateIndex <= maxCandidateIndex,
        
        AppendTo[sequence, candidateIndex];
        oddPower += 2,
        
        Break[]
      ]
    ];
    
    If[
      oddPower == 3,
      
      (* no chance of finding further results below the max, if even the first candidate at this index exceeded it *)
      Break[],
      
      metallicMeanIndex++
    ];
  ];
  
  Sort[sequence]
];

getMetallicCompositesUpTo[10000]
