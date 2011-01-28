(* Mathematica package *)

Curvatures[Curve_, Variable_] := Block[
{c=Curve,x=Variable,curv={},entry,UnitVectors,curvature,torsion,cvec,i},
	UnitVectors = GramSchmidt[c,x];
	If[Length[c]==3,
		cvec=Cross[D[c,x],D[c,{x,2}]];
		curvature=FullSimplify[
			Sqrt[Sum[cvec[[i]]^2,{i,1,Length[cvec]}]]/Sqrt[Sum[D[c,x][[i]]^2,{i,1,Length[c]}]]^3
		];
		torsion=FullSimplify[
			Det[{D[c,{x,1}],D[c,{x,2}],D[c,{x,3}]}]/Sum[cvec[[i]]^2,{i,1,Length[cvec]}]
		];
		If[sophusDebug==True,Print["Det:",Det[{D[c,{x,1}],D[c,{x,2}],D[c,{x,3}]}]];
			Print["\\dot c x \\dot\\dot c: ", Cross[D[c,x],D[c,{x,2}]]];
			Print["||\\dot c||^3: ", Sqrt[Sum[D[c,x][[i]]^2,{i,1,Length[c]}]]^3]
		];
		Return[{curvature, torsion}]];
	
	For[i=1,i<Length[UnitVectors],i++,
		entry = 
			FullSimplify[
				Dot[
					D[UnitVectors[[i]],x],
					UnitVectors[[i+1]]
				]/Sqrt[Sum[D[c[[i]],x]^2,{i,1,Length[c]}]]
			];
		curv=Append[curv, entry];
	];
	curv
];

VectorNorm[vector_]:=Module[{v=vector},
	FullSimplify[
		Sqrt[
			Sum[v[[i]]^2, {i, 1, Length[v]}]
		]
	]
];

GramSchmidt[Curve_,Variable_] := Block[
	{UnitVectors={},vec,i},
	If[Length[UnitVectors]==0,
		UnitVectors=Append[
			UnitVectors, 
			FullSimplify[
				D[Curve, Variable]/Sqrt[Sum[D[Curve[[i]], Variable]^2, {i, 1, Length[Curve]}]]
			]
		]
	];
	For[i=2,i<=Length[Curve],i++,
		vec = D[Curve, {Variable, i}] - Sum[( D[Curve,{Variable, i}] . UnitVectors[[k]] ) UnitVectors[[k]], {k, 1, Length[UnitVectors]}];
		vec = FullSimplify[vec / Sqrt[Sum[vec[[i]]^2,{i,1,Length[vec]}]]];
		UnitVectors=Append[UnitVectors, vec];
	];
	UnitVectors
];

Curvature[Curve_, Variable_]:=Module[{},
	Curvatures[Curve, Variable][[1]]
];

Torsion[Curve_, Variable_]:=Module[{},
    If[Length[c]<3, Print["Torsion is only defined for dim>2."]];
    Curvatures[Curve, Variable][[2]]
];

UnitTangent[Curve_, Variable_] := GramSchmidt[Curve, Variable][[1]]

UnitNormal[Curve_, Variable_]:= GramSchmidt[Curve, Variable][[2]]

Binormal[Curve_, Variable_] := GramSchmidt[Curve, Variable][[3]]

OsculatingCircle[Curve_, Variable_, Point_]:=Module[{p},
	(Curve/.Variable->p)+(1/Abs[Curvatures[Curve, Variable][[1]]]//.Variable->p)*UnitNormal[Curve, Variable]//.Variable->p + (1/Abs[Curvatures[Curve, Variable][[1]]]//.Variable->p) {Sin[Variable], -Cos[Variable]}
];

Involute[Curve_, Variable_] := Curve + (1/Abs[Curvatures[Curve, Variable][[1]]])*UnitNormal[Curve, Variable]

FirstFundamentalMatrix[func_, vars_] := 
	Table[
 		Dot[D[func, vars[[i]]], D[func, vars[[j]]]], {i, Length[vars]}, {j, Length[vars]}
 	]

SurfaceNormalVector[func_, vars_] := (#/VectorNorm[#])&/@Apply[Cross, Transpose[D[func, {vars}]]]

SecondFundamentalMatrix[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2,h11,h12,h21,h22},
	h11 = Dot[
			D[SurfaceNormalVector[f, {u1[[1]],u1[[1]]}, {u2[[1]],u2[[1]]}], u1[[1]]],
			D[f, u1[[1]]]
		];
	h12 = Dot[
			D[SurfaceNormalVector[f, {u1[[1]],u1[[1]]}, {u2[[1]],u2[[1]]}], u1[[1]]],
			D[f, u2[[1]]]
		];
	h21 = Dot[
			D[SurfaceNormalVector[f, {u1[[1]],u1[[1]]}, {u2[[1]],u2[[1]]}], u2[[1]]],
			D[f, u1[[1]]]
		];
	h22 = Dot[
			D[SurfaceNormalVector[f, {u1[[1]],u1[[1]]}, {u2[[1]],u2[[1]]}], u2[[1]]],
			D[f, u2[[1]]]
		];

	-{{h11, h12},{h21, h22}}/.{u1[[1]]->u1[[2]], u2[[1]]->u2[[2]]}
];

WeingartenMatrix[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2},
	Dot[
		Inverse[FirstFundamentalMatrix[f, u1, u2]],
		SecondFundamentalMatrix[f, u1, u2]
	]
];

(* potentially buggy, do not use, may blow up the coffee machine and/or create black holes *)
NormalCurvature[surface_, var1_, var2_, function_]:=Module[{},
	Print[D[function[[1]], {function[[2]], 2}]];
	FullSimplify[
	    Dot[
		    D[function[[1]], {function[[2]], 2}],
		    SurfaceNormalVector[surface, Variable1, Variable2]
	    ]*SurfaceNormalVector[surface, Variable1, Variable2]
	]
];

MeanCurvature[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2},
	1/2*Tr[WeingartenMatrix[f, u1, u2]]
];

GaussCurvature[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2},
	Det[WeingartenMatrix[f, u1, u2]]
];

TangentialCurvature[curve_, surface_, VariableCurve_, Variable1Surface_, Variable2Surface_] := Block[{},
	{
		D[curve, {VariableCurve[[1]], 2}], 
		SurfaceNormalVector[
			surface, 
			{
				Variable1Surface[[1]], Variable2Surface[[1]], (curve/.{VariableCurve[[1]]->VariableCurve[[2]]})[[1]]
			},
			{
				Variable1Surface[[2]], Variable2Surface[[1]], (curve/.{VariableCurve[[1]]->VariableCurve[[2]]})[[2]]
			}
		],
		D[curve, {VariableCurve[[1]], 1}]
	}
];

GeodeticCurvature[curve_, surface_, VariableCurve_, Variable1Surface_, Variable2Surface_] := Block[{},
	VectorNorm[TangentialCurvature[curve, surface, VariableCurve, Variable1Surface, Variable2Surface]]
];

DarbouxFrame[curve_, variable_] := Block[{}, 
	GramSchmidt[curve, variable][[1;;3]]
];

PrincCurvatures[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2},
	Eigenvalues[WeingartenMatrix[f, u1, u2]]
];