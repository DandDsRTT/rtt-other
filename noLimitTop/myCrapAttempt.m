i = 0;
map = {1,1}; (* the supposed start position *)
(*initialMap = {12, 19, 28, 34, 42, 45, 49, 51, 55, 59, 60, 63, 65, 66, 67, 69}; (* this is an actual example from Keenan's results *)*)


While[
  i < 10,
  
  (*Print["\nDOING ONE!"];*)
  i++;
  
  (* first your initial guess of a map *)
  
  previousMap = {};
  While[
    map != previousMap,
    
    (*Print["doing map ", map];*)
    previousMap = map;
    tm = g*map;
    d = Length[map];
    ptm = Log[2, getPrimes[d]];
    e =  tm / ptm - Table[1, d];
    (*Print["tm: ", tm, " d: ", d, " ptm: ", ptm, " e: ", e];*)
    solution = NMinimize[Norm[e, Infinity], g, Method -> "NelderMead", WorkingPrecision -> 15];
    
    (*process the results from it*)
    stepSize = g /.Last[solution];
    
    primesErrorMap = e /. Last[solution];
    maxDamageFromFiniteMap = Max[primesErrorMap];
    (* Print["intermediate stuff: ", 2*maxDamageFromFiniteMap/stepSize, " and exp'd: ", Exp[2*maxDamageFromFiniteMap/stepSize]];
     Print["how about this though: ", Power[2, stepSize/(2*maxDamageFromFiniteMap)]];
     highestPrimeIndexRequiredToCheck =PrimePi[Floor[Exp[2*maxDamageFromFiniteMap/stepSize]]];
      highestPrimeRequiredToCheck = If[highestPrimeIndexRequiredToCheck > 0, Prime[highestPrimeIndexRequiredToCheck], 3];*)
    highestPrimeRequiredToCheck = Power[2, stepSize/(2*maxDamageFromFiniteMap)] + 2;
    
    
    
    (*Print["pmax: ", highestPrimeRequiredToCheck];*)
    
    
    
    (* for now let's just say *)(* highestPrimeRequiredToCheck = 11;*)
    map = getUniformMap[stepSize,  highestPrimeRequiredToCheck];
    
    (*   (*log them for debugging and learning *)
       Print["stepSize: ", stepSize];
      Print["primesErrorMap: ", primesErrorMap];
      Print["maxDamageFromFiniteMap: ", maxDamageFromFiniteMap];
     (* Print["highestPrimeIndexRequiredToCheck: ", highestPrimeIndexRequiredToCheck];*)
     Print["highestPrimeRequiredToCheck: ", highestPrimeRequiredToCheck];
     Print["fullMap: ", fullMap];
       
     (* now repeat it but with the full map you need to check *)
    tm = g*fullMap;
    d = Length[fullMap];
    ptm = Log[2, getPrimes[d]];
    e =  tm / ptm - Table[1, d];
    (*Print["tm: ", tm, " d: ", d, " ptm: ", ptm, " e: ", e];*)
    solution = NMinimize[Norm[e, Infinity], g, Method -> "NelderMead", WorkingPrecision -> 15];
      stepSize = g /.Last[solution];
      
    *)
  ];
  
  (*process the results from the full map*)
  (* primesErrorMap = e /. Last[solution];*)
  primeIndexOfNarrowestPrime = First[First[Position[primesErrorMap,Min[primesErrorMap]]]];
  narrowestPrime = Prime[primeIndexOfNarrowestPrime];
  primeIndexOfWidestPrime = First[First[Position[primesErrorMap,Max[primesErrorMap]]]];
  widestPrime = Prime[primeIndexOfWidestPrime];
  
  (*and log those for debugging and learning *)
  (*Print["primesErrorMap: ", primesErrorMap];
  Print["narrowestPrime: ", narrowestPrime];
  Print["widestPrime: ", widestPrime];*)
  Print[ 1/stepSize, map];
  map[[primeIndexOfNarrowestPrime]]++;
  (* map = map[[1;;Max[primeIndexOfNarrowestPrime, primeIndexOfWidestPrime]]];*)
  
  (* Print["map: ", map, " and primeIndexOfNarrowestPrime: ", primeIndexOfNarrowestPrime];*)
  
  (*initialMap = fullMap[[1;;Max[2, primeIndexOfNarrowestPrime]]];
  
  Print["next initial Map: ", initialMap];*)
  
  (*Break[];*)
];


Solve[Log[2,x/2] == Log[3, 3/x]] // N

Prime @ Range @ PrimePi @ 10
