


checkLonePrimeConjecture[m_] := Module[{a1,a2,b1,b2, c1, c2, d1, d2},
  (*a1 = optimizeGeneratorsTuningMap[m, "originalTuningSchemeName" -> "Frobenius"]; (*AMES-minimax, AKA Frobenius *)
  Print["a1: ", a1];
  a2 = optimizeGeneratorsTuningMap[m, "originalTuningSchemeName" -> "least squares", "tim" -> {IdentityMatrix[getD[m]],"contra"}];(*prime-target U-minisos*)
  Print["a2: ", a2];
  
      b1= optimizeGeneratorsTuningMap[m, "originalTuningSchemeName" -> "TE"]; (*AES-minimax, AKA TE*)
    Print["b1: ", b1];
      b2 = optimizeGeneratorsTuningMap[m, "originalTuningSchemeName" -> "least squares","damage" -> "ES" ,"tim" -> {IdentityMatrix[getD[m]],"contra"}];(*prime-target ES-minisos*)
      Print["b2: ", b2];*)
  
  c1 = optimizeGeneratorsTuningMap[m,"optimization" -> "minimax", "damage" -> "MS", "tim" ->{}]; (*AMS-minimax- member, 1 is on intervals, \[Infinity] is on primes*)
  Print["c1: ", c1];
  c2 = optimizeGeneratorsTuningMap[m, "optimization" -> "minimax", "weighted" -> False,"tim" -> {IdentityMatrix[getD[m]],"contra"}];(*prime-target U-minimax*)
  Print["c2: ", c2];
  
  d1= optimizeGeneratorsTuningMap[m, "optimization" -> "minimax","damage" -> "S", "tim" -> {}]; (*AS-minimax, AKA TOP - member, 1 is on intervals, \[Infinity] is on primes*)
  Print["d1: ", d1];
  d2 = optimizeGeneratorsTuningMap[m, "optimization" -> "minimax","damage" -> "S" ,"tim" -> {IdentityMatrix[getD[m]],"contra"}];(*prime-target S-minimax, that is, simplicity-weighted*)
  Print["d2: ", d2];
  
  (*And @@MapThread[Abs[#1 -#2]<0.01&, {a1, a2}] && 
  And @@ MapThread[Abs[#1 -#2]<0.01&, {b1,b2}] &&*)
  And @@MapThread[Abs[#1 -#2]<0.01&, {c1, c2}] &&
      And @@ MapThread[Abs[#1 -#2]<0.01&, {d1,d2}]
];


meantoneM7 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
marvelM11 = {{{1, 0, 0, -5, 12}, {0, 1, 0, 2, -1}, {0, 0, 1, 2, -3}}, "co"};
gamelanM7 = {{{1, 1, 0, 3}, {0, 3, 0, -1}, {0, 0, 1, 0}}, "co"};
et5M5 = {{{5, 8, 12}}, "co"};
septimalBlackwoodM = {{{5, 8, 0, 14}, {0, 0, 1, 0}}, "co"};
tetracotM = {{{1, 1, 1}, {0, 4, 9}}, "co"};

checkLonePrimeConjecture[meantoneM7]
checkLonePrimeConjecture[marvelM11]
checkLonePrimeConjecture[gamelanM7]
checkLonePrimeConjecture[et5M5]
checkLonePrimeConjecture[septimalBlackwoodM]
checkLonePrimeConjecture[tetracotM]
