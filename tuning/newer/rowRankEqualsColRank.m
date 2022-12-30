In[17]:= reverseEachRow[a_] := Reverse[a, 2];
reverseEachCol[a_] := Reverse[a];
antitranspose[a_] := reverseEachRow[reverseEachCol[a]];

P = {
  {1,1,0},
  {0,0,0},
  {0,1/4,1}
};
Last[HermiteDecomposition[P]]
Last[HermiteDecomposition[Transpose[P]]]

P = {
  {1, 4/3, 4/3},
  {0, -4/3, -1/3},
  {0, 4/3, 1/3}
};
Last[HermiteDecomposition[P]]
antitranspose[Last[HermiteDecomposition[antitranspose[P]]]]

In[124]:= M = {{12,19,28}};
G = PseudoInverse[M]
P = G.M
Last[HermiteDecomposition[P]]
antitranspose[Last[HermiteDecomposition[antitranspose[P]]]]
