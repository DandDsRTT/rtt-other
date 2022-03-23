chartStartEdo = 109;
chartEndEdo = 110;
maxPrime = 73;
primes = Map[Prime, Range[PrimePi[maxPrime]]];
primeCount = Length[primes];

(* TODO: I wonder if this would still work if you extracted formulae to outside to DRY thigns up and make them more logical *)
(* TODO: style this thing so that it more closely resembles the other chart *)
Animate[ListLinePlot[
  (*gif = Table[ListLinePlot[*)
  {
    MapThread[{#1, (Abs[#2] / Log[2, #1])}&, {primes, (1200 / edo) * Map[Round[edo * Log[2, #]]&, primes] - 1200.Log[2, primes]}],
    {
      {
        1,
        Max[Abs[  (1200 / edo) * Map[Round[edo * Log[2, #]] / Log[2, #]&, primes] - 1200 * Table[1, primeCount]   ]]
      },
      {
        maxPrime,
        Max[Abs[    (1200 / edo) * Map[Round[edo * Log[2, #]] / Log[2, #]&, primes] - 1200 * Table[1, primeCount]  ]]
      }
    },
    Map[{#1, 1200 / (2 * edo * Log[2, #1])}&, Range[2, maxPrime, 0.1]]
  },
  PlotRange -> {{1, maxPrime + 1}, Automatic},
  ImageSize -> 1000
  (*],{edo,2,3,0.1}]; *)
], {edo, chartStartEdo, chartEndEdo, 0.000001, Appearance -> "Labeled"}, AnimationRunning -> False]

(*CloudExport[gif, "GIF", "CloudExport"]*)
