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
