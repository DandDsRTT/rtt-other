addWedgies[w1_, w2_] := {getMinors[w1] + getMinors[w2], getGrade[w1], eaGetV[w1]};
randomMapping[] := {RandomInteger[{-9, 9}, {r, d}], "co"};
r = 2;
d = 5;

Do[
  m1 = randomMapping[];
  m2 = randomMapping[];

  m1and2 = join[m1, m2];
  m1and2rSeparately = getR[m1] + getR[m2];
  m1and2rJoined = getR[m1and2];
  li = m1and2rJoined == m1and2rSeparately;

  w1 = matrixToMultivector[m1];
  w2 = matrixToMultivector[m2];

  w1plus2 = addWedgies[w1, w2];
  m1plus2 = multivectorToMatrix[w1plus2];

  If[li, "", Print[m1, m2, w1, w2, w1plus2, m1plus2]];

  (*LI means add wedges \[RightArrow] junk *)
  (*very unlikely, given level of randomness, to be both False *)
  Print["should be both True or both False: ", li, " and ", m1plus2 === Error],
  50
]

randomMappingRow[] := RandomInteger[{-9, 9}, {1, d}]

Do[
  sharedMappingRow = randomMappingRow[];
  m1 = { Join[sharedMappingRow, randomMappingRow[]], "co"};
  m2 = { Join[sharedMappingRow, randomMappingRow[]], "co"};

  w1 = matrixToMultivector[m1];
  w2 = matrixToMultivector[m2];

  w1plus2 = addWedgies[w1, w2];
  m1plus2 = multivectorToMatrix[w1plus2];

  Print["none of these should be errors; should all be real temperaments: ", m1plus2],
  50
]
