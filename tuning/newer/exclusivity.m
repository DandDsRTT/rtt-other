tL = {
  meantone,
  blackwood,
  dicot,
  augmented,
  mavila,
  porcupine,
  srutal,
  hanson,
  magic,
  negri,
  tetracot,
  meantone7,
  magic7,
  pajara,
  augene,
  sensi,
  sensamagic
};

Do[
  demonstrateEffectOfScrewingUpTargetSet,
  {t, tL}
];

fineTargetSet = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5}";
screwedUpTargetSet = "{2/1, 3/1, 3/2, 4/3, 5/2, 5/3, 5/4, 6/5, 5/1}";
tuningSchemeSystematicNameOtherThanTargetSet = "minimax-U";
fineTuningScheme = fineTargetSet <> " " <> tuningSchemeSystematicNameOtherThanTargetSet;
screwedUpTuningScheme = screwedUpTargetSet <> " " <> tuningSchemeSystematicNameOtherThanTargetSet;

demonstrateEffectOfScrewingUpTargetSet[t_] := Module[
  {
    fineTuning,
    screwedUpTuning,
    fineMeanDamage,
    screwedUpMeanDamage
  },
  
  fineTuning = optimizeGeneratorsTuningMap[t, fineTuningScheme];
  screwedUpTuning = optimizeGeneratorsTuningMap[t, screwedUpTuningScheme];
  
  fineMeanDamage = getGeneratorsTuningMapMeanDamage[t, fineTuning, fineTuningScheme];
  screwedUpMeanDamage = getGeneratorsTuningMapMeanDamage[t, screwedUpTuning, screwedUpTuningScheme];
  
  Print["t: ", t, " fine: ", fineMeanDamage, " screwed up: ", screwedUpMeanDamage, " diff: ", screwedUpMeanDamage - fineMeanDamage]
];
