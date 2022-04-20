getKeesComplexity[{1, -2, 1}]
getKeesComplexity[{1, 0, 0}]
getKeesComplexity[{4, 0, 0}]

getBopComplexity[{1, -2, 1}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{1, -2, 1}, {{{12, 19, 28}}, "co"}]

getBopComplexity[{4, -1, -1}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{4, -1, -1}, {{{12, 19, 28}}, "co"}]

(* And this is why BOP tuning and Wilson tuning are the same ... *)
getBopComplexity[{1, 0, 0}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{1, 0, 0}, {{{12, 19, 28}}, "co"}]

getBopComplexity[{0, 1, 0}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{0, 1, 0}, {{{12, 19, 28}}, "co"}]

getBopComplexity[{0, 0, 1}, {{{12, 19, 28}}, "co"}]
getSopfrComplexity[{0, 0, 1}, {{{12, 19, 28}}, "co"}]
