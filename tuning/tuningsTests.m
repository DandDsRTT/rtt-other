failures = 0;
passes = 0;
accuracy = 3;

testClose[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AllTrue[MapThread[Abs[#1 - #2] < 10^-accuracy&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    Print[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testCloseNotList[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    Abs[actual - expectation] < 10^-accuracy,
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] != ``; actual result was:", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
    Print[ToString[SetAccuracy[actual, accuracy + 1]]];
  ]
];
testNotClose[fn_, args___, expectation_] := Module[{actual},
  actual = Apply[fn, {args}];
  
  If[
    AnyTrue[MapThread[Abs[#1 - #2] > 10^-accuracy&, {actual, expectation}], TrueQ],
    passes += 1,
    failures += 1;
    Print[Style[StringForm["``[``] = `` but it was not supposed to", fn, {args}, SetAccuracy[expectation, accuracy + 1]], 14, Red]];
  ]
];


(* GENERATORS PREIMAGE TRANSVERSAL *)


(* getGpt *)
test[getGpt, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{{1, 0, 0}, {-1, 1, 0}}, "contra"}];
test[getGpt, {{{4, -4, 1}}, "contra"}, {{{1, 0, 0}, {0, 1, 0}}, "contra"}];


(* TUNING *)

(* some temperaments to check against *)

meantone = {{{1, 1, 0}, {0, 1, 4}}, "co"};
blackwood = {{{5, 8, 0}, {0, 0, 1}}, "co"};
dicot = {{{1, 1, 2}, {0, 2, 1}}, "co"};
augmented = {{{3, 0, 7}, {0, 1, 0}}, "co"};
mavila = {{{1, 0, 7}, {0, 1, -3}}, "co"};
porcupine = {{{1, 2, 3}, {0, 3, 5}}, "co"};
srutal = {{{2, 0, 11}, {0, 1, -2}}, "co"};
hanson = {{{1, 0, 1}, {0, 6, 5}}, "co"};
magic = {{{1, 0, 2}, {0, 5, 1}}, "co"};
negri = {{{1, 2, 2}, {0, -4, 3}}, "co"};
tetracot = {{{1, 1, 1}, {0, 4, 9}}, "co"};
meantone7 = {{{1, 0, -4, -13}, {0, 1, 4, 10}}, "co"};
magic7 = {{{1, 0, 2, -1}, {0, 5, 1, 12}}, "co"};
pajara = {{{2, 3, 5, 6}, {0, 1, -2, -2}}, "co"};
augene = {{{3, 0, 7, 18}, {0, 1, 0, -2}}, "co"};
sensi = {{{1, -1, -1, -2}, {0, 7, 9, 13}}, "co"};
sensamagic = {{{1, 0, 0, 0}, {0, 1, 1, 2}, {0, 0, 2, -1}}, "co"};

(* optimizeGtm, by individual tuning properties *)

testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "unweighted", {1200.000, 696.578}];

testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", {1202.387, 697.173}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", "complexityNormPower" -> 2, {1202.726, 697.258}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", {1201.695, 697.563}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.596, 697.530}];

testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", {1202.390, 697.174}];
testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", "complexityNormPower" -> 2, {1202.607, 696.741}];
testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", {1201.698, 697.563}];
testClose[optimizeGtm, meantone, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.397, 697.049}];

testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "noop", {1197.613, 694.787}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "noop", "complexityNormPower" -> 2, {1197.437, 694.976}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", {1197.983, 694.712}];
testClose[optimizeGtm, meantone, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1198.160, 695.012}];


testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "unweighted", {1199.022, 695.601}];

testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", {1200.070, 696.005}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", "complexityNormPower" -> 2, {1200.742, 696.205}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", {1200.985, 696.904}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1201.127, 696.905}];

testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "noop", {1198.396, 695.289}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "noop", "complexityNormPower" -> 2, {1198.244, 695.294}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", {1198.085, 694.930}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 2, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1197.930, 694.911}];


(*testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted", {1200.000, 696.578}];*)
optimizeGtm[meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "unweighted"];

testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", {1195.699, 693.352}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "noop", "complexityNormPower" -> 2, {1195.699, 693.352}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", {1200.000, 696.578}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, {1200.000, 696.578}];

testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "noop", {1200.000, 696.578}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityUnitsMultiplier" -> "noop", "complexityNormPower" -> 2, {1200.000, 696.578}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", {1195.699, 693.352}];
testClose[optimizeGtm, meantone, "optimizationPower" -> 1, "damageWeightingSlope" -> "complexityWeighted", "complexityNormPower" -> 2, {1195.699, 693.352}];


(* optimizeGtm, by "systematicTuningName" *)
(* TODO: you should probably instead just test that these map to the correct traits, or something, both for these, and for the by "originalTuningName" *)
(* TODO: you should make some diagrams and actually visually check some of these non-unique ones for pajara *)

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-U", {600.000, 108.125}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MS", {596.496, 106.108}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MES", {598.230, 106.547}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-S", {598.444, 107.706}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-ES", {599.682, 108.372}];

testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-MS", {597.123, 103.304}];
testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-MES", {598.345, 106.693}];
testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-S", {598.451, 106.578}];
testClose[optimizeGtm, pajara, "tim" -> {}, "systematicTuningName" -> "minimax-ES", {598.859, 106.844}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MC", {601.517, 108.012}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-MEC", {601.829, 108.324}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-C", {601.556, 108.012}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minimax-EC", {600.318, 108.185}];


testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-U", {599.450, 107.15}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MS", {597.851, 106.643}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MES", {598.310, 106.798}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-S", {598.436, 106.672}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-ES", {598.762, 106.835}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MC", {601.653, 107.288}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-MEC", {601.522, 107.178}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-C", {600.655, 107.426}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisos-EC", {600.263, 107.259}];


testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-U", {600.000, 106.843}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MS", {597.851, 106.643}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MES", {598.310, 106.798}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-S", {598.436, 106.672}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-ES", {598.762, 106.835}];

testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MC", {601.397, 106.145}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-MEC", {601.397, 106.145}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-C", {600.000, 106.843}];
testClose[optimizeGtm, pajara, "systematicTuningName" -> "minisum-EC", {600.000, 106.843}];


(* optimizeGtm, by "originalTuningName" *)

(* this is simply some coverage that they match the above *)
testClose[optimizeGtm, pajara, "originalTuningName" -> "minimax", {600.000, 106.843}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "least squares", {599.450, 107.15}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "TOP", {598.451, 106.578}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "TIPTOP", {598.451, 106.578}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "TE", {598.859, 106.844}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "Frobenius", {598.345, 106.693}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "POTE", {600.000, 107.048}]; (* TODO: this is weird example for PO b/c not pure octave, I mean it's good to have this, but not as the first and only example *)
testClose[optimizeGtm, pajara, "originalTuningName" -> "POTOP", {600.000, 106.854}];
testClose[optimizeGtm, pajara, "originalTuningName" -> "POTT", {600.000, 106.854}];
(* TODO: include inharmonic TE in my list of originalTuningName (and support in systematicTuningName somehow... do something for now, and then ask Dave what he thinks) *)

(* and then in this section I want to have a bunch of external examples, organized by tuning first, then temperament
sources:
[1] Facebook - I have searched this for and added notes below - well no not yet, just added to Google Keep - about (Kees, KE,), not yet for (POTOP, POTT, BOP, BE, Weil, WE, minimax, least squares, TOP, TE)
[1a] https://www.facebook.com/groups/xenharmonicmath/posts/2363908480416027/?comment_id=2363994823740726 
[2] Yahoo posts - I have searched this for and added notes below about (), not yet for (POTOP, POTT, BOP, BE, Kees, KE, Weil, WE, minimax, least squares, TOP, TE)
[2a] https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_21029.html
[3] Graham's temperament app - supports TE, POTE only
[3a] 
[4] Flora's temperament app - supports TE, TOP, POTE, POTOP, CTE (not BOP, BE, Kees, KE, Weil, WE, minimax, least squares)
[5] Paul's papers - pretty sure we're just talking Middle Path, so that's literally just TOP, nothing else
[5a] 
[6] Graham's papers - searched his site for POTOP, POTT, BOP, BE, Kees, KE, Weil, WE (but not yet TOP, TE, minimax, least squares) 
[6a] 
[7] Xen wiki - I have searched this for and added notes below about (), not yet for (POTOP, POTT, BOP, BE, Kees, KE, Weil, WE, minimax, least squares, TOP, TE)
[7a] https://en.xen.wiki/w/Target_tunings#Example
[7b] https://en.xen.wiki/w/Augene
[8] Sintel's app https://github.com/Sin-tel/temper (the surfaced app only has TE, and CTE, but the code itself may have more -- looks like only WE)
[9] Scala - has TOP, RMS-TOP = TE, Frobenius, that's it
[10] Discord history ... not checked yet
*)

(* minimax *)
testClose[optimizeTm, meantone, "originalTuningName" -> "minimax", {1199.022, 1894.623, 2782.403}]; (* [7a] says {1200.000, 1896.578, 2786.314} but I think that page might enforce pure octaves *)
(* TODO: gather some more; seems like a lot might be available on the xen wiki https://en.xen.wiki/index.php?title=Special:Search&limit=20&offset=20&profile=default&search=minimax *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
testClose[optimizeTm, augene, "originalTuningName" -> "minimax", {1200.046, 1908.852, 2800.108, 3382.574}]; (* [7b] 708.798 is close enough I think, but these actual numbers testing against are mine. hmmm but I wonder if a lot of these minimax tunings assume pure octaves like the instructions on the Target tunings page do *)
(* sensi *)
(* sensamagic *)

(* least squares *)
(* TODO: gather some; some on wiki, but you may have to use a different set of temperaments than the above *)
(* meantone *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)

(* TOP / TIPTOP *)
(* TODO: resolve discrepancies *)
accuracy = 1;
testClose[optimizeGtm, meantone, "originalTuningName" -> "TOP", {1201.70, 1201.70 - 504.13}]; (* [5](Table 1) *)
(*testClose[optimizeGtm, blackwood, "originalTuningName" -> "TOP", {238.87, 238.87*11 +158.78}]; (* [5](Table 1)*) (*this one is way off *) *)
testClose[optimizeGtm, dicot, "originalTuningName" -> "TOP", {1207.66, 353.22}];(* [5](Table 1) *)
(*testClose[optimizeGtm, augmented, "originalTuningName" -> "TOP", {399.02,399.02*5- 93.15}]; (* [5](Table 1) *) (* this one is way off *)*)
testClose[optimizeGtm, mavila, "originalTuningName" -> "TOP", {1206.55, 1206.55 + 685.03}];(* [5](Table 1) *)
testClose[optimizeGtm, porcupine, "originalTuningName" -> "TOP", {1196.91, 1034.59 - 1196.91}]; (* [5](Table 1) *)
testClose[optimizeGtm, srutal, "originalTuningName" -> "TOP", {599.56, 599.56 * 4 - 494.86}];(* [5](Table 1) *)
testClose[optimizeGtm, hanson, "originalTuningName" -> "TOP", {1200.29, 317.07}];(* [5](Table 1) *)
testClose[optimizeGtm, magic, "originalTuningName" -> "TOP", {1201.28, 380.80}]; (* [5](Table 1) *)
testClose[optimizeGtm, negri, "originalTuningName" -> "TOP", {1201.82, 1201.82 - 1075.68}]; (* [5] as "negripent" (Table 1) *)
testClose[optimizeGtm, tetracot, "originalTuningName" -> "TOP", {1199.03, 176.11}]; (* [5](Table 1) *)
testClose[optimizeGtm, meantone7, "originalTuningName" -> "TOP", {1201.70, 1201.70 * 2 - 504.13}]; (* [5](Table 2) *)
testClose[optimizeGtm, magic7, "originalTuningName" -> "TOP", {1201.28, 380.80}]; (* [5] (Table 3) *)
testClose[optimizeGtm, pajara, "originalTuningName" -> "TOP", {598.45, 598.45 - 491.88}];  (* [5](Table 2) *)
testClose[optimizeGtm, augene, "originalTuningName" -> "TOP", {399.02, 399.02 * 5 - 90.59}]; (* [5] (Table 2) *)
testClose[optimizeGtm, sensi, "originalTuningName" -> "TOP", {1198.39, 1198.39 - 755.23}]; (* [5] as "sensisept" (Table 2) *)
accuracy = 3;

(* TE *)
testClose[optimizeTm, meantone, "originalTuningName" -> "TE", {1201.397, 1898.446, 2788.196}]; (* [1a] *)
testClose[optimizeTm, blackwood, "originalTuningName" -> "TE", {1194.308, 1910.892, 2786.314}]; (* [1a] *)
(* TODO: just grab them from Graham Breed's app *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)

(* Frobenius *)
(* TODO: can grab from Flora's app; should be reliable because they'll all be unique *)

(* POTE *)
testClose[optimizeTm, meantone, "originalTuningName" -> "POTE", {1200, 1896.239, 2784.955}]; (* [1a] *)
testClose[optimizeTm, blackwood, "originalTuningName" -> "POTE", {1200, 1920, 2799.594}]; (* [1a] *)
(* TODO: just grab them from Graham Breed's app *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)

(* POTOP / POTT *)
(* TODO: this is everything we have on the wiki. nothing in Graham's site. nothing in Yahoo archives. *)
(*
https://en.xen.wiki/w/Chromatic_pairs#Voltage
Subgroup: 2.7.13
Mapping: [<1 4 4|, <0 -4 -1|]
POTT generator: ~16/13 = 1200 - 300 log₂(7) (357.794)
*)
(*
https://en.xen.wiki/w/Pajara#Tuning_spectrum
706.843  7 and 11-limit POTT
Gencom mapping: [⟨2 2 7 8 14 5], ⟨0 1 -2 -2 -6 2]]
*)
(*
4/3  310.196  
5-, 7-, 11- and 13-limit POTT
https://en.xen.wiki/w/Myna#Tuning_spectrum
Mapping: [⟨1 9 9 8], ⟨0 -10 -9 -7]]
*)
accuracy = 2;
testClose[optimizeTm, {{{1, 3, 0 , 0 , 3}, {0, -3 , 5 , 6, 1}}, "co"}, "originalTuningName" -> "POTOP", {1200., 1915.81, 2806.99, 3368.38, 4161.4}]; (* Mike himself says that maybe he got this one wrong because it should have been TIP... and yeah, I can see that this one has a pair of locked primes! https://www.facebook.com/groups/xenharmonicmath/posts/2086012064872338/ but I agree with it, so it doesn't match the Kees tuning of it elsewhere in here *)
accuracy = 1;
testClose[optimizeGtm, {{{1, 2, 6, 2, 10}, {0, -1, - 9, 2, -16}}, "co"}, "originalTuningName" -> "POTOP", {1200.0, 490.4}]; (* https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064  *)
testClose[optimizeGtm, {{{1, 2, 6, 2, 1}, {0, -1, -9, 2, 6}}, "co"}, "originalTuningName" -> "POTOP", {1200.0, 490.9}];(* https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064  *)
testClose[optimizeGtm, {{{1, 2, -3, 2, 1}, {0, -1, 13, 2, 6}}, "co"}, "originalTuningName" -> "POTOP", {1200.0, 491.9}];(* https://www.facebook.com/groups/xenharmonicmath/posts/478197012320526/?comment_id=478441632296064  *)
accuracy = 3;
testClose[optimizeGtm, {{{1, 1, 2, 1}, {0, 1, 0, 2}, {0, 0, 1, 2}}, "co"}, "originalTuningName" -> "POTOP", {1200, 700.3907806, 384.0221726}]; (* https://www.facebook.com/groups/xenharmonicmath/posts/738498989623659/?comment_id=738515309622027 *)
accuracy = 2;
testClose[optimizeGtm, {{{1, 1, 0}, {0, 1, 4}}, "co"}, "originalTuningName" -> "POTOP", {1200, 696.58}]; (* The POTOP generators for Septimal Meantone and 5-limit meantone, meanwhile, are identical at about 696.58 cents. (some Facebook thing sorry I lost the link *)
testClose[optimizeGtm, {{{1, 1, 0, -3}, {0, 1, 4, 10}}, "co"}, "originalTuningName" -> "POTOP", {1200, 696.58}]; (* The POTOP generators for Septimal Meantone and 5-limit meantone, meanwhile, are identical at about 696.58 cents. (some Facebook thing sorry I lost the link *)
accuracy = 3;
testClose[optimizeGtm, {{{1, 1, 4}, {0, 1, -2}}, "co", {2, 3, 7}}, "originalTuningName" -> "POTOP", {1200, 709.18447040211}]; (* https://www.facebook.com/groups/xenharmonicmath/posts/1035558283251060/?comment_id=1041634519310103&reply_comment_id=1041649585975263 *)

(* BOP *)
(* TODO: resolve disagreement between Flora's results and mine... we disagree on ones with non-unique tunings, and I know that I take specific steps to contend with that while she doesn't, so I suspect that I'm the one who is correct here, but we'll see *)
accuracy = 1;
testClose[optimizeTm, meantone, "originalTuningName" -> "BOP", {1201.7205, 1899.3743, 2790.6149}];  (* [4] *)
testClose[optimizeTm, blackwood, "originalTuningName" -> "BOP", {1194.2, 1910.7, 2784.5}];  (* [4] has {1194.179, 1910.6865, 2784.755} *)
testClose[optimizeTm, dicot, "originalTuningName" -> "BOP", {1207.4392, 1913.1137, 2767.7157}]; (* [4] *)
testClose[optimizeTm, augmented, "originalTuningName" -> "BOP", {1197.2, 1903.6, 2793.4}];  (* [4] has {1197.1684, 1901.5207, 2793.3928} *)
testClose[optimizeTm, mavila, "originalTuningName" -> "BOP", {1206.5842, 1892.0787, 2769.8534}];  (* [4] *)
testClose[optimizeTm, porcupine, "originalTuningName" -> "BOP", {1196.9273, 1906.5643, 2778.6315}];  (* [4] *)
testClose[optimizeTm, srutal, "originalTuningName" -> "BOP", {1199.1112, 1903.288, 2788.5356}];  (* [4] *)
testClose[optimizeTm, hanson, "originalTuningName" -> "BOP", {1200.2845, 1902.3816, 2785.6025}];  (* [4] *)
testClose[optimizeTm, magic, "originalTuningName" -> "BOP", {1201.2339, 1903.8058, 2783.229 }]; (* [4] *)
testClose[optimizeTm, negri, "originalTuningName" -> "BOP", {1201.7937, 1899.2646, 2781.8295}]; (* [4] *)
testClose[optimizeTm, tetracot, "originalTuningName" -> "BOP", {1199.0295, 1903.4108, 2783.8874}];  (* [4] *)
testClose[optimizeTm, meantone7, "originalTuningName" -> "BOP", {1201.7205, 1899.3742, 2790.615, 3371.376 }]; (* [4] *)
testClose[optimizeTm, magic7, "originalTuningName" -> "BOP", {1201.2339, 1903.8057, 2783.2289, 3367.8997}];  (* [4] *)
testClose[optimizeTm, pajara, "originalTuningName" -> "BOP", {1197.3096, 1902.8075, 2779.5876, 3378.2424}];  (* [4] *)
testClose[optimizeTm, augene, "originalTuningName" -> "BOP", {1197.2, 1905.1, 2793.4, 3372.8}];  (* [4] has  {1197.1684, 1903.995, 2793.3928, 3375.0201} *)
testClose[optimizeTm, sensi, "originalTuningName" -> "BOP", {1198.5891, 1903.5232, 2789.8411, 3363.8876}]; (* [4] *)
testClose[optimizeTm, sensamagic, "originalTuningName" -> "BOP", {1199.5, 1903.2, 2784.2, 3365.9}]; (* [4] has  {1200.3433, 1903.2071, 2784.2269, 3365.9043}*)
accuracy = 3;

(* BE *)
testClose[optimizeTm, meantone, "originalTuningName" -> "BE", {1201.4768, 1898.6321, 2788.6213}]; (* [4] *)
testClose[optimizeTm, blackwood, "originalTuningName" -> "BE", {1193.9975, 1910.396, 2786.3137}]; (* [4] *)
testClose[optimizeTm, dicot, "originalTuningName" -> "BE", {1205.8488, 1906.3416, 2761.9439}]; (* [4] *)
testClose[optimizeTm, augmented, "originalTuningName" -> "BE", {1197.2692, 1901.9550, 2793.6282}]; (* [4] *)
testClose[optimizeTm, mavila, "originalTuningName" -> "BE", {1208.5464, 1893.7139, 2778.683 }]; (* [4] *)
testClose[optimizeTm, porcupine, "originalTuningName" -> "BE", {1199.5668, 1906.8283, 2778.1916}]; (* [4] *)
testClose[optimizeTm, srutal, "originalTuningName" -> "BE", {1198.8183, 1902.9219, 2787.6566}]; (* [4] *)
testClose[optimizeTm, hanson, "originalTuningName" -> "BE", {1200.1533, 1902.2425, 2785.3554}]; (* [4] *)
testClose[optimizeTm, magic, "originalTuningName" -> "BE", {1201.1456, 1902.2128, 2782.7337}]; (* [4] *)
testClose[optimizeTm, negri, "originalTuningName" -> "BE", {1202.2630, 1900.8639, 2782.2726}]; (* [4] *)
testClose[optimizeTm, tetracot, "originalTuningName" -> "BE", {1199.5499, 1903.7780, 2784.0631}]; (* [4] *)
testClose[optimizeTm, meantone7, "originalTuningName" -> "BE", {1201.3847, 1898.6480, 2789.0531, 3368.4787}]; (* [4] *)
testClose[optimizeTm, magic7, "originalTuningName" -> "BE", {1200.9990, 1903.1832, 2782.6345, 3366.6407}]; (* [4] *)
testClose[optimizeTm, pajara, "originalTuningName" -> "BE", {1197.9072, 1903.2635, 2781.9626, 3380.9162}]; (* [4] *)
testClose[optimizeTm, augene, "originalTuningName" -> "BE", {1196.4076, 1903.1641, 2791.6178, 3372.1175}]; (* [4] *)
testClose[optimizeTm, sensi, "originalTuningName" -> "BE", {1199.7904, 1902.7978, 2789.2516, 3362.3687}]; (* [4] *)
testClose[optimizeTm, sensamagic, "originalTuningName" -> "BE", {1200.0000, 1903.3868, 2785.5183, 3365.7078}]; (* [4] *)

(* Weil *)
(* TODO: resolve disagreements *)
testClose[optimizeTm, meantone, "originalTuningName" -> "Weil", {1200.0, 1896.578, 2786.314}]; (* [2a] *)
testClose[optimizeTm, blackwood, "originalTuningName" -> "Weil", {1200.000, 1920.000, 2787.801}]; (* [2a] has  {1188.722, 1901.955, 2773.22} *)
testClose[optimizeTm, dicot, "originalTuningName" -> "Weil", {1200.000, 1901.955, 2750.978}]; (* [2a] *)
testClose[optimizeTm, augmented, "originalTuningName" -> "Weil", {1200.000, 1909.213, 2800.000}]; (* [2a] has {1194.134, 1897.307, 2786.314}*)
testClose[optimizeTm, mavila, "originalTuningName" -> "Weil", {1200.0, 1881.31, 2756.07}]; (* [2a] *)
testClose[optimizeTm, porcupine, "originalTuningName" -> "Weil", {1200.000, 1911.788, 2786.314}]; (* [2a] has {1193.828, 1901.955, 2771.982} *)
testClose[optimizeTm, srutal, "originalTuningName" -> "Weil", {1200.000, 1904.776, 2790.447}]; (* [2a] has {1198.222, 1901.955, 2786.314}*)
testClose[optimizeTm, hanson, "originalTuningName" -> "Weil", {1200.0, 1901.955, 2784.963}]; (* [2a] *)
testClose[optimizeTm, magic, "originalTuningName" -> "Weil", {1200.0, 1901.955, 2780.391}]; (* [2a] *)
testClose[optimizeTm, negri, "originalTuningName" -> "Weil", {1200.0, 1896.185, 2777.861}]; (* [2a] *)
testClose[optimizeTm, tetracot, "originalTuningName" -> "Weil", {1200.000, 1905.028, 2786.314}]; (* [2a] has {1198.064, 1901.955, 2781.819} *)
testClose[optimizeTm, meantone7, "originalTuningName" -> "Weil", {1200.0, 1896.578, 2786.314 , 3365.784}]; (* [2a] *)
testClose[optimizeTm, magic7, "originalTuningName" -> "Weil", {1200.0, 1901.955, 2780.391, 3364.692}]; (* [2a] *)
testClose[optimizeTm, pajara, "originalTuningName" -> "Weil", {1200.000, 1906.843, 2786.314, 3386.314}]; (* [2a] has  {1193.803, 1896.996, 2771.924, 3368.826} *)
testClose[optimizeTm, augene, "originalTuningName" -> "Weil", {1200.000, 1908.555, 2800.000, 3382.890}]; (* [2a] has {1194.134, 1899.852, 2786.314, 3365.102}*)
testClose[optimizeTm, sensi, "originalTuningName" -> "Weil", {1200.000, 1906.291, 2793.803, 3368.826}]; (* [2a] has  {1196.783, 1901.181, 2786.314, 3359.796}*)

(* WE *)
(* TODO: gather more examples, though there may be none... *)
testClose[optimizeTm, meantone, "originalTuningName" -> "WE", {1201.391, 1898.436, 2788.182}]; (* [1a] *)
testClose[optimizeTm, blackwood, "originalTuningName" -> "WE", {1194.254, 1910.807, 2786.189}]; (* [1a] *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)

(* Kees *)
(* meantone *)
(* blackwood *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)
accuracy = 2;
testClose[optimizeTm, {{{1, 3, 0 , 0 , 3}, {0, -3 , 5 , 6, 1}}, "co"}, "originalTuningName" -> "Kees", {1200., 1915.93, 2806.79, 3368.14, 4161.36}]; (* https://www.facebook.com/groups/xenharmonicmath/posts/2086012064872338/ *)
accuracy = 3;

(* KE *)
(* TODO: not implemented yet, so this is just doing some who knows other tuning; this would involve the weighting matrix in the pseudoinverse style, or no wait, it actually involves adapting CTE tuning to work with it *)
(*testClose[optimizeTm, meantone, "originalTuningName" -> "KE", {1200, 1896.651, 2786.605}]; (* [1a] *)
testClose[optimizeTm, blackwood, "originalTuningName" -> "KE", {1200, 1920, 2795.126}]; (* [1a] *) *)
(* dicot *)
(* augmented *)
(* mavila *)
(* porcupine *)
(* srutal *)
(* hanson *)
(* magic *)
(* negri *)
(* tetracot *)
(* meantone7 *)
(* magic7 *)
(* pajara *)
(* augene *)
(* sensi *)
(* sensamagic *)


(* tuning equivalences *)

dummyTestFn[result_] := result;

(* logSopfr = TOP *)
checkLogSopfrIsTopConjecture[t_] := Module[{logSopfrTuning, topTuning},
  logSopfrTuning = optimizeGtm[t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "logSopfr"];
  topTuning = optimizeGtm[t, "originalTuningName" -> "TOP"];
  
  testClose[dummyTestFn, logSopfrTuning, topTuning];
];
checkLogSopfrIsTopConjecture[meantone];
checkLogSopfrIsTopConjecture[blackwood];
checkLogSopfrIsTopConjecture[dicot];
checkLogSopfrIsTopConjecture[augmented];
checkLogSopfrIsTopConjecture[mavila];
checkLogSopfrIsTopConjecture[porcupine];
checkLogSopfrIsTopConjecture[srutal];
checkLogSopfrIsTopConjecture[hanson];
checkLogSopfrIsTopConjecture[magic];
checkLogSopfrIsTopConjecture[negri];
checkLogSopfrIsTopConjecture[tetracot];
checkLogSopfrIsTopConjecture[meantone7];
checkLogSopfrIsTopConjecture[magic7];
checkLogSopfrIsTopConjecture[pajara];
checkLogSopfrIsTopConjecture[augene];
checkLogSopfrIsTopConjecture[sensi];

(* sopfr = BOP *)
checkSopfrIsBopConjecture[t_] := Module[{sopfrTuning, bopTuning},
  sopfrTuning = optimizeGtm[t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityUnitsMultiplier" -> "sopfr"];
  bopTuning = optimizeGtm[t, "originalTuningName" -> "BOP"];
  
  testClose[dummyTestFn, sopfrTuning, bopTuning];
];
checkSopfrIsBopConjecture[meantone];
checkSopfrIsBopConjecture[blackwood];
checkSopfrIsBopConjecture[dicot];
checkSopfrIsBopConjecture[augmented];
checkSopfrIsBopConjecture[mavila];
checkSopfrIsBopConjecture[porcupine];
checkSopfrIsBopConjecture[srutal];
checkSopfrIsBopConjecture[hanson];
checkSopfrIsBopConjecture[magic];
checkSopfrIsBopConjecture[negri];
checkSopfrIsBopConjecture[tetracot];
checkSopfrIsBopConjecture[meantone7];
checkSopfrIsBopConjecture[magic7];
checkSopfrIsBopConjecture[pajara];
checkSopfrIsBopConjecture[augene];
checkSopfrIsBopConjecture[sensi];

(* Kees \[TildeTilde] POTOP/POTT, when the tuning is unique *)
checkKeesIsCloseToPotopWhenUniqueConjecture[t_, isUnique_] := Module[{keesTuning, potopTuning},
  keesTuning = optimizeGtm[t, "originalTuningName" -> "Kees"];
  potopTuning = optimizeGtm[t, "originalTuningName" -> "POTOP"];
  
  If[
    isUnique,
    testClose[dummyTestFn, keesTuning, potopTuning],
    testNotClose[dummyTestFn, keesTuning, potopTuning]
  ];
];
accuracy = 1;
checkKeesIsCloseToPotopWhenUniqueConjecture[meantone, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[blackwood, False];
checkKeesIsCloseToPotopWhenUniqueConjecture[dicot, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[augmented, False];
checkKeesIsCloseToPotopWhenUniqueConjecture[mavila, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[porcupine, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[srutal, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[hanson, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[magic, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[negri, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[tetracot, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[meantone7, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[magic7, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[pajara, True];
checkKeesIsCloseToPotopWhenUniqueConjecture[augene, False];
checkKeesIsCloseToPotopWhenUniqueConjecture[sensi, True];
accuracy = 3;

(* KE \[TildeTilde] POTE *)
(* TODO: KE is still not implemented *)
checkKeIsCloseToPoteConjecture[t_] := Module[{keTuning, poteTuning},
  keTuning = optimizeGtm[t, "originalTuningName" -> "KE"];
  poteTuning = optimizeGtm[t, "originalTuningName" -> "POTE"];
  
  testClose[dummyTestFn, keTuning, poteTuning];
];
accuracy = 1;
(*checkKeIsCloseToPoteConjecture[meantone];
checkKeIsCloseToPoteConjecture[blackwood];
checkKeIsCloseToPoteConjecture[dicot];
checkKeIsCloseToPoteConjecture[augmented];
checkKeIsCloseToPoteConjecture[mavila];
checkKeIsCloseToPoteConjecture[porcupine];
checkKeIsCloseToPoteConjecture[srutal];
checkKeIsCloseToPoteConjecture[hanson];
checkKeIsCloseToPoteConjecture[magic];
checkKeIsCloseToPoteConjecture[negri];
checkKeIsCloseToPoteConjecture[tetracot];
checkKeIsCloseToPoteConjecture[meantone7];
checkKeIsCloseToPoteConjecture[magic7];
checkKeIsCloseToPoteConjecture[pajara];
checkKeIsCloseToPoteConjecture[augene];
checkKeIsCloseToPoteConjecture[sensi];*)
accuracy = 3;


(* interval basis *)
(* TODO: find and include more examples of this *)

t = {{{1, 1, 5}, {0, -1, -3}}, "co", {2, 7 / 5, 11}};
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "formalPrimes", {1200.4181, 617.7581}];
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "primes", {1200.0558, 616.4318}];

t = {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}};
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "formalPrimes", {1201.3969, 3796.8919, 5270.7809}];
testClose[optimizeGtm, t, "tim" -> {}, "optimizationPower" -> \[Infinity], "damageWeightingSlope" -> "simplicityWeighted", "complexityNormPower" -> 2, "tuningIntervalBasis" -> "primes", {1201.3969, 3796.8919, 5267.2719}];


(* pure-octave stretch *)

(* TODO: test cover at all *)
(* TODO: test cover to error in the case that more than one generator row affects the first column *)
(* TODO: test cover to error if the subgroup doesn't include prime 2 *)
(* TODO: test cover to find prime 2 if it's not the first prime for some reason *)



(* ___ PRIVATE ___ *)



(* dualPower *)
test[dualPower, 1, \[Infinity]];
test[dualPower, 2, 2];
test[dualPower, \[Infinity], 1];

(* getPFromUnchangedIntervals *)
test[getPFromUnchangedIntervals, {{{1, 1, 0}, {0, 1, 4}}, "co"}, {{1, 0, 0}, {-2, 0, 1}}, {{1, 1, 0}, {0, 0, 0}, {0,FractionBox["1", "4"], 1}}];

(* getDiagonalEigenvalueMatrix *)
test[getDiagonalEigenvalueMatrix, {{1, 0, 0}, {-2, 0, 1}}, {{-4, 4, -1}}, {{1, 0, 0}, {0, 1, 0}, {0, 0, 0}}];

(* getPtm *)
test[getPtm, {{{12, 19, 28}}, "co", {2, 3, 5}}, {Log[2, 2], Log[2, 3], Log[2, 5]}];
test[getPtm, {{{1, 0, -4, 0}, {0, 1, 2, 0}, {0, 0, 0, 1}}, "co", {2, 9, 5, 21}}, {Log[2, 2], Log[2, 9], Log[2, 5], Log[2, 21]}];

(* getDiamond *)
test[getDiamond, 2, {{2, -1}, {-1, 1}}];
test[getDiamond, 3, {{2, -1, 0}, {3, 0, -1}, {-1, 1, 0}, {1, 1, -1}, {-2, 0, 1}, {0, -1, 1}}];
test[getDiamond, 4, {{2, -1, 0, 0}, {3, 0, -1, 0}, {3, 0, 0, -1}, {4, -2, 0, 0}, {-1, 1, 0, 0}, {1, 1, -1, 0}, {2, 1, 0, -1}, {-2, 0, 1, 0}, {0, -1, 1, 0}, {1, 0, 1, -1}, {1, -2, 1, 0}, {-2, 0, 0, 1}, {-1, -1, 0, 1}, {0, 0, -1, 1}, {1, -2, 0, 1}, {-3, 2, 0, 0}, {0, 2, -1, 0}, {0, 2, 0, -1}}];

(* octaveReduce *)
test[octaveReduce, 3, 3 / 2];
test[octaveReduce, 5, 5 / 4];
test[octaveReduce, 2 / 3, 4 / 3];

(* oddLimitFromD *)
test[oddLimitFromD, 2, 3];
test[oddLimitFromD, 3, 5];
test[oddLimitFromD, 4, 9];
test[oddLimitFromD, 5, 11];
test[oddLimitFromD, 6, 15];

(* getComplexity *)
dummy5limitTemp = {{{1, 2, 3}, {0, 5, 6}}, "co"};
test[getComplexity, {1, 1, -1}, dummy5limitTemp, "noop", 1, 3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, "noop", 2, \[Sqrt]3];
test[getComplexity, {1, 1, -1}, dummy5limitTemp, "logProduct", 1, 1 +FractionBox[RowBox[{"Log", "[", "3", "]"}], RowBox[{"Log", "[", "2", "]"}]]+FractionBox[RowBox[{"Log", "[", "5", "]"}], RowBox[{"Log", "[", "2", "]"}]]];

pcv = {1, -2, 1};
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "noop", 1, 4];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "noop", 2, 2.449];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 1, 6.492];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 2, 4.055];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 1, 3.322];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 2, 2.103];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 1, 3.170];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 2, 2.389];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 1, 13];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 2, 8.062];

testCloseNotList[getComplexity, pcv, dummy5limitTemp, "noop", 1, getCopfrComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logProduct", 1, getLogProductComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logIntegerLimit", 1, getLogIntegerLimitComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "logOddLimit", 1, getLogOddLimitComplexity[pcv]];
testCloseNotList[getComplexity, pcv, dummy5limitTemp, "product", 1, getProductComplexity[pcv]];

(* getDamage *)
testCloseNotList[getDamage, meantone, {1201.7, 697.564}, "originalTuningName" -> "TOP", 0.00141543];
testCloseNotList[getDamage, meantone, {1199.02, 695.601}, "originalTuningName" -> "least squares", 0.0000729989];
testCloseNotList[getDamage, meantone, {1200., 696.578}, "originalTuningName" -> "minimax", 0.0179233];
(* TODO: I'm not sure this handles pure-octave stretch and interval basis properly *)



Print["TOTAL FAILURES: ", failures];
Print["TOTAL PASSES: ", passes];
