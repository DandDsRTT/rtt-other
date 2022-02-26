outer[u_, v_] := Transpose[u].v;
wedge[u_, v_] := outer[u, v] - outer[v, u];

(* examples *)

x = {{7, 11, 16}};
y = {{12, 19, 28}};
outer[x, y]
outer[y, x]
outer[Transpose[x], Transpose[y]]
outer[Transpose[y], Transpose[x]]
wedge[x, y]
