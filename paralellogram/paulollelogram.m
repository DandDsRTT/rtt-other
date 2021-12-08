A = {1, 0, 1};
B = {0, 1, 2};
Cross[A, B]
Norm[%]
% // N
TensorWedge[A, B] // MatrixForm



a = Norm[A]
b = Norm[B]
theta = Sin[VectorAngle[A, B]]
a * b * theta



A = {4, -4, 1};
B = {7, 0, -3};
a = Norm[A]
b = Norm[B]
theta = Sin[VectorAngle[A, B]]
a * b * theta
