meantone = {{{1, 0, -4}, {0, 1, 4}}, "co"};
blackwood = {{{5, 8, 0}, {0, 0, 1}}, "co"};
dicot = {{ {1, 1, 2}, {0, 2, 1}}, "co"};
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
pajara = {{{2, 0, 11, 12}, {0, 1, -2, -2}}, "co"};
augene = {{{3, 0, 7, 18}, {0, 1, 0, -2}}, "co"};
sensi = {{{1, -1, -1, -2}, {0, 7, 9, 13}}, "co"};



(* GOT IT!!! *)
(* TE *)
(* Meantone: TE = <1201.397 1898.446 2788.196| *)
optimizeGtm[meantone, "tuning" -> "TE"].getA[meantone]
{1201.397, 1898.446, 2788.196}
(* Blackwood: TE = <1194.308 1910.892 2786.314| *)
optimizeGtm[blackwood, "tuning" -> "TE"].getA[blackwood]
{1194.308, 1910.892, 2786.314}



(* NOPE... *)
(* Kees *)
(* Meantone: POTOP = <1200. 1896.58 2786.31] (should match exactly; found this myself via this code, see potop.m) *)
optimizeGtm[meantone, "tuning" -> "Kees"].getA[meantone]
{1200., 1896.58, 2786.31}
(* Blackwood: POTOP = <1200 1920 2801.37] (should match exactly; found this myself via the below, see potop.m) *)
optimizeGtm[blackwood, "tuning" -> "Kees"].getA[blackwood]
{1200, 1920, 2801.37}



(* not implemented yet, so this is just doing some who knows other tuning; this would involve the weighting matrix in the pseudoinverse style *)
(* KE *)
(* Meantone: KE = <1200 1896.651 2786.605|, POTE = <1200 1896.239 2784.955| close but different *)
optimizeGtm[meantone, "tuning" -> "KE"].getA[meantone]
{1200, 1896.651, 2786.605}
(* Blackwood: KE = <1200 1920 2795.126|, POTE = <1200 1920 2799.594| close but different *)
optimizeGtm[blackwood, "tuning" -> "KE"].getA[blackwood]
{1200, 1920, 2795.126}



(* MEANTONE IS VERY CLOSE, BLACKWOOD IS CLOSE... I THINK WE NEED TO DO MORE EXAMPLES *)
(* Weil *)
(* these test cases taken from https://yahootuninggroupsultimatebackup.github.io/tuning-math/topicId_21029.html *)

"meantone"
optimizeGtm[meantone, "tuning" -> "Weil"].getA[meantone]
{1200.0, 1896.578, 2786.314}

"blackwood"
optimizeGtm[blackwood, "tuning" -> "Weil"].getA[blackwood]
{1188.722, 1901.955, 2773.22}

"dicot"
optimizeGtm[dicot, "tuning" -> "Weil"].getA[dicot]
{1200.000, 1901.955, 2750.978}

"augmented"
optimizeGtm[augmented, "tuning" -> "Weil"].getA[augmented]
{1194.134, 1897.307, 2786.314}

"mavila"
optimizeGtm[mavila, "tuning" -> "Weil"].getA[mavila]
{1200.0, 1881.31, 2756.07}

"porcupine"
optimizeGtm[porcupine, "tuning" -> "Weil"].getA[porcupine]
{1193.828, 1901.955, 2771.982}

"srutal"
optimizeGtm[srutal, "tuning" -> "Weil"].getA[srutal]
{1198.222, 1901.955, 2786.314}

"hanson"
optimizeGtm[hanson, "tuning" -> "Weil"].getA[hanson]
{1200.0, 1901.955, 2784.963}

"magic"
optimizeGtm[magic, "tuning" -> "Weil"].getA[magic]
{1200.0, 1901.955, 2780.391}

"negri"
optimizeGtm[negri, "tuning" -> "Weil"].getA[negri]
{1200.0, 1896.185, 2777.861}

"tetracot"
optimizeGtm[tetracot, "tuning" -> "Weil"].getA[tetracot]
{1198.064, 1901.955, 2781.819}

"meantone7"
optimizeGtm[meantone7, "tuning" -> "Weil"].getA[meantone7]
{1200.0, 1896.578, 2786.314 , 3365.784}

"magic7"
optimizeGtm[magic7, "tuning" -> "Weil"].getA[magic7]
{1200.0, 1901.955, 2780.391, 3364.692}

"pajara"
optimizeGtm[pajara, "tuning" -> "Weil"].getA[pajara]
{1193.803, 1896.996, 2771.924, 3368.826}

"augene"
optimizeGtm[augene, "tuning" -> "Weil"].getA[augene]
{1194.134, 1899.852, 2786.314, 3365.102}

"sensi"
optimizeGtm[sensi, "tuning" -> "Weil"].getA[sensi]
{1196.783, 1901.181, 2786.314, 3359.796}



(* WE... not implemented yet, so this is just doing some who knows other tuning; this would involve the weighting matrix in the pseudoinverse style *)
(* Meantone: WE = <1201.391 1898.436 2788.182| *)
optimizeGtm[meantone, "tuning" -> "WE"].getA[meantone]
{1201.391, 1898.436, 2788.182}
(* Blackwood: WE = <1194.254 1910.807 2786.189| *)
optimizeGtm[blackwood, "tuning" -> "WE"].getA[blackwood]
{1194.254, 1910.807, 2786.189}



(* BOP *)
(* Meantone: BOP = ??? can find with Flora's code... but I don't trust it right now, it's still got my minkowksi and chebyshev stuff *)
optimizeGtm[meantone, "tuning" -> "BOP"].getA[meantone]
{0, 0, 0}
(* Blackwood: BOP = ??? can find with Flora's code... but I don't trust it right now, it's still got my minkowksi and chebyshev stuff *)
optimizeGtm[blackwood, "tuning" -> "BOP"].getA[blackwood]
{0, 0, 0}



(* BE... not implemented yet, so this is just doing some who knows other tuning; this would involve the weighting matrix in the pseudoinverse style *)
(* Meantone: BE = <1201.4768 1898.6321 2788.6213] (found with Flora's code) *)
optimizeGtm[meantone, "tuning" -> "BE"].getA[meantone]
(* Blackwood: BE = <1193.9975 1910.396     0. ]   (found with Flora's code... probably that 0 should be a purely-tuned prime 5 )*)
optimizeGtm[blackwood, "tuning" -> "BE"].getA[blackwood]
