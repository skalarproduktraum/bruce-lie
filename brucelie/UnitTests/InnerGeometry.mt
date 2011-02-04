(* Mathematica Test File *)
Test[
	FirstFundamentalMatrix[{Sin[u]Cos[v], Cos[u]Cos[v], Cos[u]}],
	{{1, 0},{0, Sin[u]^2}},
	TestID->"InnerGeometry-FirstFundamentalMatrix-2Sphere"
]
