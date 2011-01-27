(* Mathematica package *)

PlotWithOsculatingCircle[Curve_, Variable_, Interval_, Point_]:=Module[{},
	(*ParametricPlot[Evaluate[{c, OsculatingCircle[c, x, p]}], Evaluate[Flatten[{x,interval}]]]*)
		(*ParametricPlot[c, Evaluate[Flatten[{x,interval}]]]*)
	Show[
		ParametricPlot[OsculatingCircle[Curve, Variable, Point];, Evaluate[{Variable, -Pi, Pi}], PlotStyle->Red],
		ParametricPlot[Curve, Evaluate[Flatten[{Variable, Interval}]]], PlotRange->All
	]
];

PlotWithInvolute[Curve_, Interval_]:=Module[{c=Curve, interval=Interval,inv},
	inv = Involute[c, interval[[1]]];
	Show[
		ParametricPlot[inv, Evaluate[interval], PlotStyle->Orange],
		ParametricPlot[c, Evaluate[interval]], PlotRange->All
	]
];