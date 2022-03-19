getUniformMap[edo_, limit_] := Map[First[Round[edo * Log[2, #]]]&, Map[Prime, Range[PrimePi[limit]]]];

map = {16, 25,37,45,55,59,65,67};
gtm = optimizeGtm[{{map},"co"}, "tuning" -> "Tenney"];
uniformMap = getUniformMap[1200  / gtm , Prime[Length[map]]]
uniformMap == map

map = {12,19,28,34,42,45,49,51};
gtm = optimizeGtm[{{map},"co"}, "tuning" -> "Tenney"];
uniformMap = getUniformMap[1200  / gtm , Prime[Length[map]]]
uniformMap == map
