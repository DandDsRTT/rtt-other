In[1]:= X = Transpose[{{a1,a2,a3},{b1,b2,b3},{c1,c2,c3}}];
Y = {{1,1,3},{0,1,0},{1,0,2}};
A = X.Y;


"A = "
A // MatrixForm
"X"
X // MatrixForm
"Y"
Y // MatrixForm

In[50]:= X = {
  {1,2},
  {4,0},
  {0,1},
  {1,1}
};
Y = {
  {y11,y12,y13},
  {y21,y22,y23}
};
X.Y // MatrixForm

X = {
  {x11,x12},
  {x21,x22},
  {x31,x32},
  {x41,x42}
};
Y = {
  {1,0,3},
  {0,2,1}
};
X.Y // MatrixForm



In[58]:= X = {
  {1,0},
  {4,0},
  {0,1},
  {1,1}
};
Y = {
  {1,0,3},
  {0,2,1}
};
A = X.Y;
A // MatrixForm
Last[HermiteDecomposition[A]] // MatrixForm
Transpose[Last[HermiteDecomposition[Transpose[A]]]] // MatrixForm
