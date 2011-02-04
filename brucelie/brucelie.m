(* Mathematica Package *)

(* Created by the Wolfram Workbench Jan 27, 2011 *)

(*
 bruce lie
 Differential Geometry Package, including stuff for General Relativity
 by Ulrik Guenther

The General Relativity/Ricci Calculus part is partly based on previous work by

 * Tristan Huebsch, Howard University, Physics Dept.
   http://homepage.mac.com/thubsch/default.html
 * Pekka Janhunen, Finnish Meteorological Institute, Geophysics Dept.
   http://www.space.fmi.fi/~pjanhune/

*)

BeginPackage["brucelie`"]
(* Exported symbols added here with SymbolName::usage *) 

brucelie::usage = "";

brucelie::versionString = "this is bruce lie, r14
(c) 2009 - 2011 ulrik guenther
based upon work by tristan huebsch and pekka janhunen";

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

PrincCurvatureQ::usage = "";

ChristoffelSymbols::usage = "";

RiemannTensor::usage = "";

RicciTensor::usage = "";

RicciScalar::usage = "";

ScalarCurvature::usage = "";

EinsteinTensor::usage = "";

Commutator::usage = "";

CommutatorCoefficients::usage = "";

TorsionTensor::usage = "";

DarbouxFrame::usage = "";

TangentialCurvature::usage = "";

GeodeticCurvature::usage = "";

UmbilicalQ::usage = "";

FlatUmbilicalQ::usage = "";

(* info messages *)

brucelie::InfoPackageOverride = "Info: using only custom packages.";

(* error messages *)

Begin["`Private`"]
(* Implementation of the package *)

blDebug = TrueQ["sophusDebug"];

Print[brucelie::versionString];

subpackages = If[ListQ[Global`bruceliePackages],
			Message[brucelie::InfoPackageOverride];
			Global`bruceliePackages,
			{
				"brucelie`InnerGeometry`",
				"brucelie`PlotFunctions`",
				"brucelie`RiemannianGeometry`"
			}
];

Do[
	Print["loading " <> p <> "..."];
	Get[p],
	{p, subpackages}
]; 


End[]

EndPackage[]
