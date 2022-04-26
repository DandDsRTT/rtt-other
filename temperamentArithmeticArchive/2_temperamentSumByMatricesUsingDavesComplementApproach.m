complement[t_] := If[
  isCo[t],
  {getA[dual[t]], "co"},
  {getA[dual[t]], "contra"},
];

areCollinearM[m1_, m2_] := Module[{m1and2, m1and2JoinedR, m1and2SeparatelyR},
  m1and2 = join[m1, m2];
  m1and2JoinedR = getR[m1and2];
  m1and2SeparatelyR = getR[m1] + getR[m2];
  
  Print["m1and2: ", m1and2, " m1and2JoinedR: ", m1and2JoinedR, " m1and2SeparatelyR: ", m1and2SeparatelyR];
  
  m1and2JoinedR < m1and2SeparatelyR
];

areCollinearC[c1_, c2_] := Module[{c1and2, c1and2JoinedN, c1and2SeparatelyN},
  c1and2 = meet[c1, c2];
  c1and2JoinedN = getN[c1and2];
  c1and2SeparatelyN = getN[c1] + getN[c2];
  
  Print["c1and2: ", c1and2, " c1and2JoinedN: ", c1and2JoinedN, " c1and2SeparatelyN: ", c1and2SeparatelyN];
  
  c1and2JoinedN < c1and2SeparatelyN
];

addWedgies[w1_, w2_] := {eaGetLargestMinorsL[w1] + eaGetLargestMinorsL[w2], eaGetGrade[w1], getVariance[w1]};

(*
I'm not sure this is right at all...
eaAreCollinear[w1_, w2_] := Module[{w1and2, gcd},
w1and2 = addWedgies[w1, w2];
gcd = getGcd[eaGetLargestMinorsL[w1and2]];
Print["eaAreCollinear, w1and2: ", w1and2, " gcd: ", gcd];

gcd===1
];
*)

temperamentSum[M1_, M2_] := Module[{commonRows, firstExcept, secondExcept, addedRows},
  commonRows = dual[meet[M1, M2]];
  firstExcept = meet[M1, complement[commonRows]];
  secondExcept = meet[M2, complement[commonRows]];
  addedRows = getA[firstExcept] + getA[secondExcept];
  Print["commonRows: ", commonRows, " firstExcept: ", firstExcept, " secondExcept: ", secondExcept, " addedRows: ", addedRows, " complement[commonRows]: ", complement[commonRows]];
  
  {Join[addedRows, getA[commonRows]], "co"}
];

porcupine = {{{1, 2, 3}, {0, 3, 5}}, "co"};
meantone = {{{1, 0, -4}, {0, 1, 4}}, "co"};



complement[porcupine]
complement[{{{-4, 4, -1}}, "contra"}]
temperamentSum[porcupine, meantone]



addWedgies[{{1, 4, 10, 4, 13, 12}, 2, "co"}, {{1, -3, -11, -7, -20, -17}, 2, "co"}] (*septimal meantone & septimal mavila *)
multivectorToMatrix[%]
addWedgies[{{1, 4, 10, 4, 13, 12}, 2, "co"}, {{2, 8, 1, 8, -4, -20}, 2, "co"}] (* septimal meantone & godzilla *)
    multivectorToMatrix[%]
(* so I was expecting collinear temperament sums to work, and linearly independent temperament sums to error...
does this mean that actually meantone & godzilla, despite being seemingly more related, are actually the pair that are LI?

I could maybe figure this out by using x31eq, and unison vector search

godzilla comma basis is 49/48, 81/80 so
godzilla ETs: 5, 19, 14c, 24p, 9c, 10c, 33cd, 38d, 28ccd, 29c

and septimal meantone comma basis is 81/80, 126/125 so
septimal meantone ETs: 12, 19, 31, 7d, 24d, 38d, 43, 50, 5d, 26d

okay so those share 19. so... they're not... LI??? then why do they error?

what about septimal mavila comma basis, htat's 126/125, 135/128
so septimal mavila ETs: 7d, 9d, 16, 14cdd, 2dd, 23d, 5cdddd, 30bcdd, 21cddd, 25bd

wait, so do I have it totally backwards??
*)
addWedgies[{{1, 4, 10, 4, 13, 12}, 2, "co"}, {{0, 5, 0, 8, 0, -14}, 2, "co"}] (* septimal meantone & septimal blackwood *)
    multivectorToMatrix[%]


areCollinearM[multivectorToMatrix[{{1, 4, 10, 4, 13, 12}, 2, "co"}], multivectorToMatrix[{{1, -3, -11, -7, -20, -17}, 2, "co"}]]
    areCollinearM[multivectorToMatrix[{{1, 4, 10, 4, 13, 12}, 2, "co"}], multivectorToMatrix[{{2, 8, 1, 8, -4, -20}, 2, "co"}]]
    areCollinearM[multivectorToMatrix[{{1, 4, 10, 4, 13, 12}, 2, "co"}], multivectorToMatrix[{{0, 5, 0, 8, 0, -14}, 2, "co"}]]
    
    areCollinearC[dual[multivectorToMatrix[{{1, 4, 10, 4, 13, 12}, 2, "co"}]], dual[multivectorToMatrix[{{1, -3, -11, -7, -20, -17}, 2, "co"}]]]
    areCollinearC[dual[multivectorToMatrix[{{1, 4, 10, 4, 13, 12}, 2, "co"}]], dual[multivectorToMatrix[{{2, 8, 1, 8, -4, -20}, 2, "co"}]]]
    areCollinearC[dual[multivectorToMatrix[{{1, 4, 10, 4, 13, 12}, 2, "co"}]], dual[multivectorToMatrix[{{0, 5, 0, 8, 0, -14}, 2, "co"}]]]
    
    
    
    
    
    multivectorToMatrix[{{12, -8, 5}, 2, "contra"}]
    multivectorToMatrix[{{16, -11, 7}, 2, "contra"}]
    dual[{{{5, 8, 12}}, "co"}]
    
    
    
    
    areCollinearC[{{{-19, 12, 0}}, "contra"}, {{{8, 5, 0}}, "contra"}]
    areCollinearM[dual[{{{-19, 12, 0}}, "contra"}], dual[{{{8, 5, 0}}, "contra"}]]
    dual[{{{-19, 12, 0}}, "contra"}]
    dual[{{{8, 5, 0}}, "contra"}]
    
    
    
    
    
    et7 = {7, 11, 16};

(* meantone only maps *)
et2c = {2, 3, 4};
et5 = {5, 8, 12};
et12 = {12, 19, 28};
et31 = {31, 49, 72};

(* porcupine only maps *)
et1c = {1, 2, 3};
et15 = {15, 24, 35};
et22 = {22, 35, 51};
et
    et30 = {30, 48, 70};

canonicalForm[{{et2c + et1c, et7}, "co"}](*no*)
    canonicalForm[{{et2c + et15, et7}, "co"}] (*no*)
    canonicalForm[{{et2c + et22, et7}, "co"}] (*no*)
    canonicalForm[{{et2c + et30, et7}, "co"}] (*no*)
    canonicalForm[{{et5 + et1c, et7}, "co"}] (*yes*)
    canonicalForm[{{et5 + et15, et7}, "co"}] (*yes*)
    canonicalForm[{{et5 + et22, et7}, "co"}] (*yes*)
    canonicalForm[{{et5 + et30, et7}, "co"}](*no*)
    canonicalForm[{{et12 + et1c, et7}, "co"}](*yes*)
    canonicalForm[{{et12 + et15, et7}, "co"}](*yes*)
    canonicalForm[{{et12 + et22, et7}, "co"}](*yes*)
    canonicalForm[{{et12 + et30, et7}, "co"}](*no*)
    canonicalForm[{{et31 + et1c, et7}, "co"}](*no*)
    canonicalForm[{{et31 + et15, et7}, "co"}](*no*)
    canonicalForm[{{et31 + et22, et7}, "co"}](*no*)
    canonicalForm[{{et31 + et30, et7}, "co"}] (*yes*)
