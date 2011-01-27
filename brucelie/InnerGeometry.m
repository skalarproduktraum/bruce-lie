(* Mathematica package *)

Curvatures[Curve_,Variable_]:=Module[
{c=Curve,x=Variable,curv={},entry,UnitVectors,curvature,torsion,cvec,i},
	UnitVectors=GramSchmidt[c,x];
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
FullSimplify[Sqrt[Sum[v[[i]]^2,{i,1,Length[v]}]]]
];

GramSchmidt[Curve_,Variable_]:=Module[
{c=Curve,x=Variable,UnitVectors={},vec,i},
If[Length[UnitVectors]==0,
	UnitVectors=Append[
		UnitVectors, 
		FullSimplify[D[c,x]/Sqrt[Sum[D[c[[i]],x]^2,{i,1,Length[c]}]]
	]]
];
For[i=2,i<=Length[c],i++,
	vec = D[c,{x, i}] - Sum[( D[c,{x, i}] . UnitVectors[[k]] ) UnitVectors[[k]], {k, 1, Length[UnitVectors]}];
	vec = FullSimplify[vec / Sqrt[Sum[vec[[i]]^2,{i,1,Length[vec]}]]];
	UnitVectors=Append[UnitVectors, vec];
	];
UnitVectors
];

Curvature[Curve_, Variable_]:=Module[{},
	Curvatures[Curve, Variable][[1]]
];

Torsion[Curve_, Variable_]:=Module[{},
    If[Length[c]<3, Print["Torsion only exists for n>2."]];
    Curvatures[Curve, Variable][[2]]
];

UnitTangent[Curve_, Variable_]:=Module[{c=Curve,x=Variable,UnitVectors},
	UnitVectors=GramSchmidt[c,x];
UnitVectors[[1]]
];

UnitNormal[Curve_, Variable_]:=Module[{c=Curve,x=Variable,UnitVectors},
	UnitVectors=GramSchmidt[c,x];
UnitVectors[[2]]
];

Binormal[Curve_, Variable_]:=Module[{c=Curve,x=Variable,UnitVectors},
	UnitVectors=GramSchmidt[c,x];
UnitVectors[[3]]
];

OsculatingCircle[Curve_, Variable_, Point_]:=Module[{c=Curve,x=Variable,p=Point,UnitVectors,OCRadius,OCCenter,curvatures},
	UnitVectors=GramSchmidt[c,x];
	curvatures = Curvatures[Curve, x];
	OCRadius = (1/Abs[curvatures[[1]]]//.x->p);
	OCCenter = (c/.x->p)+OCRadius*UnitVectors[[2]]//.x->p;
OCCenter + OCRadius {Sin[x], -Cos[x]}
];

Involute[Curve_, Variable_]:=Module[{c=Curve,x=Variable,UnitVectors,OCCenter,OCRadius},
	UnitVectors=GramSchmidt[c,x];
	OCRadius = (1/Abs[Curvatures[Curve, x][[1]]]);
	OCCenter = c+OCRadius*UnitVectors[[2]];

OCCenter
];

FirstFundamentalMatrix[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2,Du11,Du12,Du21,Du22},
	Du11 = Dot[D[f, u1[[1]]], D[f, u1[[1]]]];
	Du12 = Dot[D[f, u1[[1]]], D[f, u2[[1]]]];
	Du21 = Dot[D[f, u2[[1]]], D[f, u1[[1]]]];
	Du22 = Dot[D[f, u2[[1]]], D[f, u2[[1]]]];
	
	{{Du11, Du12}, {Du21, Du22}}/.{u1[[1]]->u1[[2]], u2[[1]]->u2[[2]]}
];

SurfaceNormalVector[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2,CrossProduct,Du1,Du2},
	Du1 = D[f, u1[[1]]];
	Du2 = D[f, u2[[1]]];
	CrossProduct = Cross[Du1, Du2];

	(CrossProduct/Sqrt[Sum[CrossProduct[[i]]^2,{i, 1, Length[CrossProduct]}]])/.{u1[[1]]->u1[[2]], u2[[1]]->u2[[2]]}
];

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

PrincCurvatures[func_, Variable1_, Variable2_]:=Module[{f=func,u1=Variable1,u2=Variable2},
	Eigenvalues[WeingartenMatrix[f, u1, u2]]
];