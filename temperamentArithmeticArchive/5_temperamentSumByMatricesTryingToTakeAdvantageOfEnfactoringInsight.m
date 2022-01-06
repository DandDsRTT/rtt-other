temperamentSum[M1_, M2_] := Module[{ commonRow, m1extraRow, m2extraRow, extraRow, m1row1, m1row2, m2row1, m2row2},
    commonRow = First[getA[dual[meet[M1, M2]]]];

    m1row1 = First[getA[M1]];
    m1row2 = Last[getA[M1]];
    m2row1 = First[getA[M2]];
    m2row2 = Last[getA[M2]];

  m1extraRow = extractGcf[4*commonRow +m1row1  +m1row2 ];
  m2extraRow= extractGcf[11*commonRow + m2row1 + m2row2];
  Print[TensorWedge[commonRow, m1extraRow] // MatrixForm, TensorWedge[commonRow, m2extraRow] // MatrixForm];
  extraRow = extractGcf[m1extraRow + m2extraRow];
  Print["commonRow: ", commonRow, " m1extraRow: ", m1extraRow, " m2extraRow: ", m2extraRow, " extraRow: ", extraRow, " m1extraRowBeforeGcf: ", commonRow +m1row1  +m1row2, " m2extraRowBeforeGcf: ", commonRow + m2row1 + m2row2, " extraRowBeforeGcf: ", m1extraRow + m2extraRow];

    canonicalForm[{Join[{commonRow}, {extraRow}], "co"}]
];

meantone = {{{1,0,-4},{0,1,4}},"co"}; (* <<1 4 4]] *)
porcupine = {{{1,2,3},{0,3,5}},"co"}; (* <<3 5 1]] *)
augmented = {{{3,0,7},{0,1,0}},"co"}; (* <<3 0 -7]] *)
diminished = {{{4,0,3},{0,1,1}}, "co"}; (* <<4 4 -3]] *)
tetracot = {{{1,1,1},{0,4,9}},"co"}; (* <<4 9 5]] *)
dicot = {{{1,1,2},{0,2,1}},"co"}; (* <<2 1 -3]] *)
srutal = {{{2,0,11},{0,1,-2}},"co"}; (* <<2 -4 -11]] *)

temperamentSum[meantone,porcupine] (* should be tetracot, but is dicot *)
temperamentSum[meantone, augmented] (* should be diminished, but is... hmmm no I think this is totally broken*)
