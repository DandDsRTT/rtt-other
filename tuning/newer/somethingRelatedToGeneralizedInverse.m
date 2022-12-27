i = Transpose[{{-1,-1,0,1}}];
M = {
  {1, 0, -4, -13},
  {0,1,4,10}
};

augmentedI = Transpose[{{-1,-1,0,1,-1}}]; (*7/6's augmented vector; -1+-1+1=-1 *)
augmentedM ={
  {1, 0, -4, -13,0},
  {0,1,4,10,0}
};

thing = {
  {1, 0, 0, 0},
  {0,1,0,0},
  {0,0,1,0},
  {0,0,0,1},
  {k,k,k,k}
};

M == augmentedM.thing
M.i == augmentedM.augmentedI

Out[30]= True

Out[31]= True

NullSpace[{{k,k,k,k,-1}}]

Out[32]= {{1,-1,0,0,0},{1,0,-1,0,0},{1,0,0,-1,0},{-1,0,0,0,-k}}

In[126]:= NullSpace[Transpose[antiTranspose[thing]]]

Out[126]= {{-1,k,k,k,k}}

In[127]:= fullyAugmentedMThing ={
  {1, 0, -4, -13,0},
  {0,1,4,10,0},
  {k,k,k,k,-1}
};
NullSpace[fullyAugmentedMThing]

Out[128]= {{3,-6,4,-1,0},{-4,4,-1,0,-k}}

In[342]:= g = {g1,g2};
M = {
  {1, 0, -4, -13},
  {0,1,4,10}
};
L = DiagonalMatrix[Log2[{2,3,5,7}]];
j = 1200*Table[1,4].L;
t = g.M;
r = t - j;
absPartiallyPretransformedR = Abs[r.Inverse[L]];
maxDamage = Max[absPartiallyPretransformedR];
proportions = Map[#/maxDamage&,absPartiallyPretransformedR];
sumProportions = Total[proportions];
z = 2*sumProportions;
a = 2/z;

inverseZ = {
  {2-a,-a,-a,-a,a},
  {-a,2-a,-a,-a,a},
  {-a,-a,2-a,-a,a},
  {-a,-a,-a,2-a,a}
};
result = NMinimize[
  {
    Max[absPartiallyPretransformedR.inverseZ](*,
z == 2.6388117*)
  },
  g
];
result

Print["r: ", r /. Last[result]];
Print["partiallyPretransformedR: ", absPartiallyPretransformedR /. Last[result]];
Print["maxDamage: ", maxDamage /. Last[result]];
Print["proportions: ", proportions /. Last[result]];
Print["sumProportions: ", sumProportions /. Last[result]];
Print["z: ", z /. Last[result]];
Print["a: ", a /. Last[result]];


Out[356]= {1.69853,{g1->1201.7,g2->1899.26}}

InterpretationBox[RowBox[{"\"r: \"", "\:f360", RowBox[{"{", RowBox[{"1.698517240683941`", ",", RowBox[{"-", "2.6921092866421077`"}], ",", "3.9437834874111104`", ",", "1.7222851894398445`"}], "}"}]}], SequenceForm["r: ", {1.698517240683941`, -2.6921092866421077`, 3.9437834874111104`, 1.7222851894398445`}], Editable -> False]

InterpretationBox[RowBox[{"\"partiallyPretransformedR: \"", "\:f360", RowBox[{"{", RowBox[{"1.698517240683941`", ",", "1.6985318488085368`", ",", "1.6984950981448999`", ",", "0.6134903627281741`"}], "}"}]}], SequenceForm["partiallyPretransformedR: ", {1.698517240683941`, 1.6985318488085368`, 1.6984950981448999`, 0.6134903627281741`}], Editable -> False]

InterpretationBox[RowBox[{"\"maxDamage: \"", "\:f360", "1.6985318488085368`"}], SequenceForm["maxDamage: ", 1.6985318488085368`], Editable -> False]

InterpretationBox[RowBox[{"\"proportions: \"", "\:f360", RowBox[{"{", RowBox[{"0.9999913995580325`", ",", "1.`", ",", "0.9999783632766952`", ",", "0.3611886130710573`"}], "}"}]}], SequenceForm["proportions: ", {0.9999913995580325`, 1`, 0.9999783632766952`, 0.3611886130710573`}], Editable -> False]

InterpretationBox[RowBox[{"\"sumProportions: \"", "\:f360", "3.3611583759057853`"}], SequenceForm["sumProportions: ", 3.3611583759057853`], Editable -> False]

InterpretationBox[RowBox[{"\"z: \"", "\:f360", "6.7223167518115705`"}], SequenceForm["z: ", 6.7223167518115705`], Editable -> False]

InterpretationBox[RowBox[{"\"a: \"", "\:f360", "0.2975164774050595`"}], SequenceForm["a: ", 0.2975164774050595`], Editable -> False]

In[215]:= g = {g1,g2};
M = {
  {1, 0, -4},
  {0,1,4}
};
L = DiagonalMatrix[Log2[{2,3,5}]];
j = 1200*Table[1,3].L;
t = g.M;
r = t - j;
absPartiallyPretransformedR = Abs[r.Inverse[L]];
maxDamage = Max[absPartiallyPretransformedR];
proportions = Map[#/maxDamage&,absPartiallyPretransformedR];
sumProportions = Total[proportions];
z = 2*sumProportions;
a = 2/z;
inverseZ = {
  {2-a,-a,-a,a},
  {-a,2-a,-a,a},
  {-a,-a,2-a,a}
};
minimaxDamage = Max[absPartiallyPretransformedR.inverseZ];
Z = {
  {1,0,0},
  {0,1,0},
  {0,0,1},
  {1,1,1}
}/2;

result = NMinimize[
  {
    minimaxDamage,
    (*g1 == 1200,*)
    g2 == 1896.578
  },
  {g1,g2}
];
result

Print["t: ", t /. Last[result]];
Print["r: ", r /. Last[result]];
Print["partiallyPretransformedR: ", absPartiallyPretransformedR /. Last[result]];
Print["maxDamage: ", maxDamage /. Last[result]];
Print["proportions: ", proportions /. Last[result]];
Print["sumProportions: ", sumProportions /. Last[result]];
Print["z: ", z /. Last[result]];
Print["a: ", a /. Last[result]];
Print["minimaxDamage: ", minimaxDamage /. Last[result]];
Print["checking: ",  IdentityMatrix[3] == inverseZ.Z /. Last[result]];

Out[231]= {3.39251,{g1->1200.93,g2->1896.58}}

InterpretationBox[RowBox[{"\"t: \"", "\:f360", RowBox[{"{", RowBox[{"1200.9252235267295`", ",", "1896.578`", ",", "2782.611105893082`"}], "}"}]}], SequenceForm["t: ", {1200.9252235267295`, 1896.578`, 2782.611105893082`}], Editable -> False]

InterpretationBox[RowBox[{"\"r: \"", "\:f360", RowBox[{"{", RowBox[{"0.9252235267294964`", ",", RowBox[{"-", "5.377000865387572`"}], ",", RowBox[{"-", "3.7026079717529683`"}]}], "}"}]}], SequenceForm["r: ", {0.9252235267294964`, -5.377000865387572`, -3.7026079717529683`}], Editable -> False]

InterpretationBox[RowBox[{"\"partiallyPretransformedR: \"", "\:f360", RowBox[{"{", RowBox[{"0.9252235267294964`", ",", "3.392509830952494`", ",", "1.5946264571696753`"}], "}"}]}], SequenceForm["partiallyPretransformedR: ", {0.9252235267294964`, 3.392509830952494`, 1.5946264571696753`}], Editable -> False]

InterpretationBox[RowBox[{"\"maxDamage: \"", "\:f360", "3.392509830952494`"}], SequenceForm["maxDamage: ", 3.392509830952494`], Editable -> False]

InterpretationBox[RowBox[{"\"proportions: \"", "\:f360", RowBox[{"{", RowBox[{"0.27272537821054066`", ",", "1.`", ",", "0.4700432825929239`"}], "}"}]}], SequenceForm["proportions: ", {0.27272537821054066`, 1`, 0.4700432825929239`}], Editable -> False]

InterpretationBox[RowBox[{"\"sumProportions: \"", "\:f360", "1.7427686608034645`"}], SequenceForm["sumProportions: ", 1.7427686608034645`], Editable -> False]

InterpretationBox[RowBox[{"\"z: \"", "\:f360", "3.485537321606929`"}], SequenceForm["z: ", 3.485537321606929`], Editable -> False]

InterpretationBox[RowBox[{"\"a: \"", "\:f360", "0.5737996226871399`"}], SequenceForm["a: ", 0.5737996226871399`], Editable -> False]

InterpretationBox[RowBox[{"\"minimaxDamage: \"", "\:f360", "3.3925098309524944`"}], SequenceForm["minimaxDamage: ", 3.3925098309524944`], Editable -> False]

InterpretationBox[RowBox[{"\"checking: \"", "\:f360", "True"}], SequenceForm["checking: ", True], Editable -> False]

In[7]:= A = {
  {1,0,0},
  {0,1,0},
  {0,0,1},
  {1,1,1}
};
A⁻ = {
  {0, -1, -1, 1},
  {-1,0,-1,1},
  {-1,-1,0,1}
};
A.A⁻.A == A
A⁻.A
A⁻.A.A⁻
Transpose[A.A⁻] == A.A⁻
Transpose[A⁻.A] == A⁻.A
    
    Out[9]= True
    
    Out[10]= {{1,0,0},{0,1,0},{0,0,1}}
    
    Out[11]= {{0,-1,-1,1},{-1,0,-1,1},{-1,-1,0,1}}
    
    Out[12]= False
    
    Out[13]= True