(* Mathematica Package *)

(* Created by the Wolfram Workbench Jan 27, 2011 *)

BeginPackage["brucelie`"]
(* Exported symbols added here with SymbolName::usage *) 

brucelie::usage = "";

brucelie::versionString = "this is bruce lie, r14
(c) 2009 - 2011 ulrik guenther";

Curvatures::usage = "";

VectorNorm::usage = "";

GramSchmidt::usage = "";

Curvature::usage = "";

Torsion::usage = "";

UnitTangent::usage = "";

UnitNormal::usage = "";

Binormal::usage = "";

OsculatingCircle::usage = "";

PlotWithOsculatingCircle::usage = "";

Involute::usage = "";

PlotWithInvolute::usage = "";

FirstFundamentalMatrix::usage = "";

SecondFundamentalMatrix::usage = "";

WeingartenMatrix::usage = "";

SurfaceNormalVector::usage = "";

NormalCurvature::usage = "";

MeanCurvature::usage = "";

GaussCurvature::usage = "";

PrincCurvatures::usage = "";

ChristoffelSymbols::usage = "";

RiemannTensor::usage = "";

RicciTensor::usage = "";

RicciScalar::usage = "";

ScalarCurvature::usage = "";

EinsteinTensor::usage = "";

Commutator::usage = "";

CommutatorCoefficients::usage = "";

TorsionTensor::usage = "";

Begin["`Private`"]
(* Implementation of the package *)

If[
    TrueQ["sophusDebug"],
    blDebug = True,
    blDebug = False
]

Needs["InnerGeometry`"]
Needs["PlotFunctions`"]
Needs["RiemannianGeometry`"]

End[]

EndPackage[]
