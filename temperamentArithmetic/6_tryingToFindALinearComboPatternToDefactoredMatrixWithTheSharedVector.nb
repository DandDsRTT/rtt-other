shared = {7,11,16};

meantone = {{{1,0,-4},{0,1,4}},"co"}; (* <<1 4 4]] *) (*works for 1,1,1 *)
porcupine = {{{1,2,3},{0,3,5}},"co"}; (* <<3 5 1]] *)
dicot = {{{1,1,2},{0,2,1}},"co"}; (* <<2 1 -3]] *)

m = meantone;

m1 = First[getA[m]];
m2 = Last[getA[m]];

Do[
  Do[
    Do[
      mWithShared = {shared, extractGcd[(i-1)*shared + (j-1)*m1 + (k-1)*m2]};
      d = hnf[colHermiteDefactor[mWithShared]];
      h = hnf[mWithShared];
      If[d == h, Print[i-1,",",j-1,",",k-1,": ",mWithShared]],
      {k,10}
    ],
    {j,10}
  ],
  {i,10}
]
