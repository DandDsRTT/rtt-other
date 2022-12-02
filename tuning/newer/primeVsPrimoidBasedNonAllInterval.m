In[3304]:= machine = "2.9.7.11 [⟨1 3 3 4] ⟨0 1 -1 -3]]";
"{2, 7/4, 9/4, 9/7, 11/4, 11/7, 11/8, 11/9}";
optimizeGeneratorTuningMap[machine,{"targetIntervals" ->"[[1 0 0 0⟩ [-2 0 1 0⟩ [-2 1 0 0⟩ [0 1 -1 0⟩ [-2 0 0 1⟩ [0 0 -1 1⟩ [-3 0 0 1⟩ [0 -1 0 1⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "primoid-based",  "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

In[3398]:= optimizeGeneratorTuningMap[machine,{"targetIntervals" ->"[[1 0 0 0⟩ [-2 0 1 0⟩ [-2 1 0 0⟩ [0 1 -1 0⟩ [-2 0 0 1⟩ [0 0 -1 1⟩ [-3 0 0 1⟩ [0 -1 0 1⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "prime-based",  "optimizationPower" -> \[Infinity], "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

mechanism = "2.9.7.11 [⟨1 5 5 2] ⟨0 -5 -6 4]]";
apparatus = "2.9.7.11 [⟨1 5 3 5] ⟨0 -19 -2 -16]]";
optimizeGeneratorTuningMap[mechanism,{"targetIntervals" ->"[[1 0 0 0⟩ [-2 0 1 0⟩ [-2 1 0 0⟩ [0 1 -1 0⟩ [-2 0 0 1⟩ [0 0 -1 1⟩ [-3 0 0 1⟩ [0 -1 0 1⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "primoid-based",  "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

optimizeGeneratorTuningMap[mechanism,{"targetIntervals" ->"[[1 0 0 0⟩ [-2 0 1 0⟩ [-2 1 0 0⟩ [0 1 -1 0⟩ [-2 0 0 1⟩ [0 0 -1 1⟩ [-3 0 0 1⟩ [0 -1 0 1⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "prime-based",  "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

In[3442]:= breedsmic = "2.3.49/5 [⟨1 1 3] ⟨0 2 1]]"
optimizeGeneratorTuningMap[breedsmic,{"targetIntervals" ->"[[1 0 0⟩ [0 1 0⟩ [-1 1 0⟩ [4 -1 0⟩ [0 0 1⟩ [-2 0 1⟩ [-3 0 1⟩ [4 0 -1⟩ [-1 -1 1⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "primoid-based",  "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

In[3441]:= optimizeGeneratorTuningMap[breedsmic,{"targetIntervals" ->"[[1 0 0⟩ [0 1 0⟩ [-1 1 0⟩ [4 -1 0⟩ [0 0 1⟩ [-2 0 1⟩ [-3 0 1⟩ [4 0 -1⟩ [-1 -1 1⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "prime-based",  "optimizationPower" ->1, "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

In[3445]:= indium = "2.5/3.7/3.11/3 [⟨1 0 0 2] ⟨0 6 10 -1]}"
optimizeGeneratorTuningMap[indium,{"targetIntervals" ->"[[1 0 0 0⟩ [0 1 0 0⟩ [1 -1 0 0⟩ [2 -1 0 0⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "prime-based",  "optimizationPower" -> 1, "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

In[3449]:= optimizeGeneratorTuningMap[indium,{"targetIntervals" ->"[[1 0 0 0⟩ [0 1 0 0⟩ [1 -1 0 0⟩ [2 -1 0 0⟩]","tuningSchemeNonstandardIntervalBasisApproach" -> "primoid-based",  "optimizationPower" -> 2, "damageWeightSlope" -> "complexityWeight", "logging" -> True}]

In[3444]:= getTilt[12]

2, 5/3, 6/5, 12/5, 7/3,7/6,12/7,7/5,10/7,11/6,11/5,11/7
