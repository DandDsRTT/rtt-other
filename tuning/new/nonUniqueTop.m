countNonzeroElements[l_] := Count[l, element_ /; element != 0];
whichGeneratorIsTheSingleOneApproximatingThisPrime[generatorsApproximatingPrime_] := First[Position[generatorsApproximatingPrime, x_ /; x > 0, 1, 1]];

primesInLockedRatio[m_] := Module[
  {
    generatorsApproximatingEachPrime,
    countGeneratorsInvolvedInApproximatingEachPrime,
    whetherPrimesAreApproximatedBySingleGeneratorOrNot,
    indexesOfPrimesApproximatedBySingleGenerators,
    perGeneratorHowManyPrimesAreApproximatedOnlyByIt,
    index,
    hmmm,
    whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator
  },
  
  generatorsApproximatingEachPrime = Transpose[getA[m]];
  countGeneratorsInvolvedInApproximatingEachPrime = Map[countNonzeroElements, generatorsApproximatingEachPrime];
  whetherPrimesAreApproximatedBySingleGeneratorOrNot = Map[# == 1&, countGeneratorsInvolvedInApproximatingEachPrime];
  indexesOfPrimesApproximatedBySingleGenerators = {};
  MapIndexed[If[#1 == True, AppendTo[indexesOfPrimesApproximatedBySingleGenerators, #2] ]&, whetherPrimesAreApproximatedBySingleGeneratorOrNot];
  
  perGeneratorHowManyPrimesAreApproximatedOnlyByIt = Association[];
  Map[
    Function[{indexOfPrimeApproximatedBySingleGenerator},
      hmmm = First[Part[generatorsApproximatingEachPrime, indexOfPrimeApproximatedBySingleGenerator]];
      index = whichGeneratorIsTheSingleOneApproximatingThisPrime[hmmm];
      If[
        KeyExistsQ[perGeneratorHowManyPrimesAreApproximatedOnlyByIt, index],
        perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] = perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] + 1,
        perGeneratorHowManyPrimesAreApproximatedOnlyByIt[index] = 1
      ];
    ],
    indexesOfPrimesApproximatedBySingleGenerators
  ];
  whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator = Map[# > 1&, Values[perGeneratorHowManyPrimesAreApproximatedOnlyByIt]];
  AnyTrue[whetherGeneratorsApproximateMoreThanOnePrimeForWhichTheyAreItsSingleApproximatingGenerator, TrueQ]
];

test[primesInLockedRatio, {{{5, 8}}, "co"}, True];
test[primesInLockedRatio, {{{5, 8, 12}}, "co"}, True];
test[primesInLockedRatio, {{{1, 0, -4}, {0, 1, 4}}, "co"}, False];
test[primesInLockedRatio, {{{5, 8, 12, 14}}, "co"}, True];
test[primesInLockedRatio, {{{1, 2, 3, 2}, {0, -3, -5, -6}}, "co"}, False];
test[primesInLockedRatio, {{{3, 0, 7, 18}, {0, 1, 0, -2}}, "co"}, True];
test[primesInLockedRatio, {{{3, 0, 7, 0}, {0, 4, 0, 7}}, "co"}, True];
test[primesInLockedRatio, {{{5, 8, 12, 0}, {0, 0, 0, 1}}, "co"}, True];
test[primesInLockedRatio, {{{1, 0, 0, -5}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}, False];
test[primesInLockedRatio, {{{1, 0, -4, 0}, {0, 1, 4, 0}, {0, 0, 0, 1}}, "co"}, False];
test[primesInLockedRatio, {{{5, 8, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}, "co"}, True];
