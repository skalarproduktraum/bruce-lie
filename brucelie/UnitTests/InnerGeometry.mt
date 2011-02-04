(* Mathematica Test File *)
Test[
	FirstFundamentalMatrix[{Sin[u]Cos[v], Sin[u]Sin[v], Cos[u]}, {u, v}]//FullSimplify,
	{{1, 0},{0, Sin[u]^2}},
	TestID->"InnerGeometry-FirstFundamentalMatrix-2Sphere"
]

Test[
	FirstFundamentalMatrix[
		{r Cos[phi], r Sin[phi], z}, 
		{r, phi, z}
	]// FullSimplify,
	{{1, 0, 0},{0, r^2, 0},{0, 0, 1}},
	TestID->"InnerGeometry-FirstFundamentalMatrix-PolarCoordinates"	
]

Test[
	SurfaceNormalVector[
		{u - (u - 3 v)^2 + 8 v^2 - 1,
 		 6 u v - Sin[u - v] + Cos[Sqrt[2] u] + Cos[Sqrt[2] v],
		 (5 - 3 v) Exp[v] + Cos[u] + v (3 u - 4) - 5
 		},
		{u, v}
	]/.{u->0, v->0},
	{2/3, 2/3, 1/3},
	TestID->"InnerGeometry-SurfaceNormalVector-LongExpressionForSurface"
]
