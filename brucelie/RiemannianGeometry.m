(* Mathematica package *)

ChristoffelSymbols[metric_, x_] := Module[
	{InverseMetric, cs, dim},
	(*
	Christoffel Symbols
	\Gamma^{i}_ {jk}
	*)
	InverseMetric = Inverse[metric];
	dim = Dimensions[metric][[1]];
	
	cs = FullSimplify[Table[
		D[metric[[i, j]], x[[k]]] 
		+ D[metric[[k, i]], x[[j]]] 
		- D[metric[[j, k]], x[[i]]],
		{i, dim}, {j, dim}, {k, dim}
	]];
	
	Simplify[1/2*InverseMetric.cs]
];

RiemannTensor[metric_, x_] := Module[
	{dim, Christoffels, rt},
	dim = Dimensions[metric][[1]];
	Christoffels = ChristoffelSymbols[metric, x];
	
	rt = Table[
		D[Christoffels[[i, j, l]], x[[k]]] + 
		Sum[
			Christoffels[[u,j,l]]*Christoffels[[i,u,k]],
			{u, dim}
		],
		{i, dim}, {j, dim}, {k, dim}, {l, dim}
	];

	Simplify[
		Table[rt[[i,j,k,l]] - rt[[i,j,l,k]],
		{i, dim}, {j, dim}, {k, dim}, {l, dim}]
	]
];

RicciTensor[metric_, x_] := Module[
	{rt, dim},
	dim = Dimensions[metric][[1]];
	rt = RiemannTensor[metric, x];

	Simplify[
		Table[Sum[rt[[u,i,u,j]],{u, dim}],{i, dim},{j, dim}]
	]
];

RicciScalar[metric_, x_] := Module[
	{rict, InverseMetric, dim},
	dim = Dimensions[metric][[1]];
	rict = RicciTensor[metric, x];
	InverseMetric = Inverse[metric];

	Simplify[
		Sum[InverseMetric[[i,j]]*rict[[i,j]], {i, dim}, {j,dim}]
	]
];

ScalarCurvature[metric_, x_] := Module[
	{},
	RicciScalar[metric, x]
];

EinsteinTensor[metric_, x_] := Module[
	{},
	Simplify[RicciTensor[metric, x] - RicciScalar[metric, x]]
];

Commutator[form1_, form2_] := Simplify[form1.form2 - form2.form1]

CommutatorCoefficients[form1_, form2_]:= Module[
	{},
	Commutator[form1[[i]], form2[[j]]]
];

TorsionTensor[metric_, x_] := Module[
	{Christoffels, tt, dim},
	Christoffels = ChristoffelSymbols[metric, x];
	dim = Dimensions[metric][[1]];

	tt = Table[Christoffels[[]]-Christoffels[[]]-CommutatorCoefficients[],{i, dim}];
	Simplify[
		tt
	]
];