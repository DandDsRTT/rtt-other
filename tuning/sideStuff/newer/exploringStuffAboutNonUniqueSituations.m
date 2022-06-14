augene = {{3, 3, 7, 12}, {0, 1, 0, -2}};
optimizeGeneratorsTuningMap[{augene, "co"}, "originalTuningSchemeName" -> "TOP"]

diff5[pcv_] := Abs[{g1, g2}.augene.pcv - 1200 * Log2[{2, 3, 5, 7}].pcv] / getPcvLogProductComplexity[pcv, augene];

Plot3D[
  {
    diff5[{1, 0, 0, 0}],
    diff5[{0, 1, 0, 0}],
    diff5[{0, 0, 1, 0}],
    diff5[{0, 0, 0, 1}]
  },
  {g1, 395, 405},
  {g2, 700, 715},
  ImageSize -> 1000
]

weirdAugene = {{3, 0, 7, 0}, {0, 4, 0, 7}};
optimizeGeneratorsTuningMap[{weirdAugene, "co"}, "originalTuningSchemeName" -> "TOP"]

diff6[pcv_] := Abs[{g1, g2}.weirdAugene.pcv - 1200 * Log2[{2, 3, 5, 7}].pcv] / getPcvLogProductComplexity[pcv, weirdAugene]

Plot3D[
  {
    diff6[{1, 0, 0, 0}],
    diff6[{0, 1, 0, 0}],
    diff6[{0, 0, 1, 0}],
    diff6[{0, 0, 0, 1}]
  },
  {g1, 395, 405},
  {g2, 470, 480},
  ImageSize -> 1000
]

getDamage[{weirdAugene, "co"}, {397.722, 475.931}, "originalTuningSchemeName" -> "TOP"]
getDamage[{weirdAugene, "co"}, {399, 478.4}, "originalTuningSchemeName" -> "TOP"]
getDamage[{weirdAugene, "co"}, {398.631, 479.162}, "originalTuningSchemeName" -> "TOP"]
getDamage[{weirdAugene, "co"}, {999, 999}, "originalTuningSchemeName" -> "TOP"]

something = {{5, 8, 12, 0}, {0, 0, 0, 1}};
optimizeGeneratorsTuningMap[{something, "co"}, "originalTuningSchemeName" -> "TOP"]

diff7[pcv_] := Abs[{g1, g2}.something.pcv - 1200 * Log2[{2, 3, 5, 7}].pcv] / getPcvLogProductComplexity[pcv, something]

Plot3D[
  {
    diff7[{1, 0, 0, 0}],
    diff7[{0, 1, 0, 0}],
    diff7[{0, 0, 1, 0}],
    diff7[{0, 0, 0, 1}]
  },
  {g1, 230, 240},
  {g2, 3360, 3370},
  ImageSize -> 1000
]



optimizeGeneratorsTuningMap[meantone, "systematicTuningSchemeName" -> "minimax-S"]
