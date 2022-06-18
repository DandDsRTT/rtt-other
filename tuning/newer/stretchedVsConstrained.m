In[2728]:= constrainedDamage = 0;
stretchedDamage = 0;
temperaments = {
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
  tetracot(*,
  meantone7,
  magic7,
  pajara,
  augene,
  sensi,
  sensamagic*)
};
tunings = {
  "diamond minimax-U",
  "diamond minisos-U",
  "diamond minisum-U",
  "diamond minimax-C",
  "diamond minisos-C",
  "diamond minisum-C",
  "diamond minimax-S",
  "diamond minisos-S",
  "diamond minisum-S"
};
Do[
  Do[
    (*Print["temperament: ", temperament, " tuning: ", tuning];*)
    constrainedTuningMap = optimizeTuningMap[temperament, "unchanged-octave " <> tuning];
    constrainedTuningMapDamage =  getTuningMapMeanDamage[temperament, constrainedTuningMap, "unchanged-octave "  <> tuning];
    (* Print["constrainedTuningMapDamage: ", constrainedTuningMapDamage];*)
    constrainedDamage +=constrainedTuningMapDamage;
    stretchedTuningMap = optimizeTuningMap[temperament, "pure-stretched-octave " <> tuning];
    stretchedTuningMapDamage = getTuningMapMeanDamage[temperament, stretchedTuningMap, "pure-stretched-octave " <> tuning];
    (*  Print["stretchedTuningMapDamage: ", stretchedTuningMapDamage];*)
    stretchedDamage += stretchedTuningMapDamage;
    
    If[
      Abs[constrainedTuningMapDamage - stretchedTuningMapDamage] > 5,
      Print["temperament: ", temperament, " tuning: ", tuning];
      Print["constrainedTuningMap: ", constrainedTuningMap];
      Print["constrainedTuningMapDamage: ", constrainedTuningMapDamage];
      Print["constrainedTuningMapDamages: ",  getTuningMapDamages[temperament, constrainedTuningMap, "unchanged-octave "  <> tuning]];
      Print["stretchedTuningMap: ", stretchedTuningMap];
      Print["stretchedTuningMapDamage: ", stretchedTuningMapDamage];
      Print["stretchedTuningMapDamages: ", getTuningMapDamages[temperament, stretchedTuningMap, "pure-stretched-octave "  <> tuning]];
    ]
    ,
    
    {temperament, temperaments}
  ],
  {tuning, tunings}
];
Print["constrainedDamage: ", constrainedDamage];
Print["stretchedDamage: ", stretchedDamage];


In[2735]:= optimizeTuningMap[mavila, "diamond minimax-C"]
optimizeTuningMap[mavila, "pure-stretched-octave diamond minimax-C"]

In[2763]:= plainScheme =  "diamond minisum-C";
plainTuning = optimizeTuningMap[mavila, plainScheme]
getTuningMapDamages[mavila, plainTuning, plainScheme]
getTuningMapMeanDamage[mavila, plainTuning, plainScheme]

constrainedScheme =  "unchanged-octave "  <> plainScheme;
constrainedTuning = optimizeTuningMap[mavila, constrainedScheme]
getTuningMapDamages[mavila, constrainedTuning, constrainedScheme]
getTuningMapMeanDamage[mavila, constrainedTuning, constrainedScheme]

stretchedScheme =  "pure-stretched-octave "  <> plainScheme;
stretchedTuning = optimizeTuningMap[mavila, stretchedScheme]
getTuningMapDamages[mavila, stretchedTuning, stretchedScheme]
getTuningMapMeanDamage[mavila, stretchedTuning, stretchedScheme]
