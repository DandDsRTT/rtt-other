In[42]:= p = (*1+1/(2^3); *)1.00001;
NMinimize[
  Power[
    (Abs[x]^p +
        Abs[x-1]^p +
        Abs[x-3]^p +
        Abs[x-5]^p
    )/4,
    1/p],
  x,
  WorkingPrecision -> 128
]

In[198]:= Plot[
  First[Minimize[
    Power[
      (Abs[x]^p +
          Abs[x-1]^p +
          Abs[x-2]^p +
          Abs[x-4]^p
      )/4,
      1/p],
    x
  ]],
  {p,1,2}
]

In[29]:= Plot[
  First[Values[Last[Minimize[
    Power[
      (Abs[x+1]^(1+1/(2^p)) +
          Abs[x-1]^(1+1/(2^p)) +
          Abs[x-2]^(1+1/(2^p)) +
          Abs[x-5]^(1+1/(2^p))
      )/4,
      1/(1+1/(2^p))],
    x
  ]]]],
  {p,0,10},
  PlotPoints->10,
  Mesh -> All,
  PlotRange -> {1.5,1.7}
]
