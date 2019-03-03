(* ::Package:: *)

rCompiled := rCompiled = Block[
	{sol, conv , f, r, R, a, b},
	sol = Values /@ Solve[{(r a)^(2 / 3) + (r b)^(2 / 3) == 1}, r];
	conv = Flatten[sol] /. Thread[{a, b} -> {(1 + Sqrt[Abs[2 R^2 - 1]]) / (2 R), (1 - Sqrt[Abs[2 R^2 - 1]]) / (2 R)}];
	f = Simplify[conv[[2]], Assumptions -> R > 1 / Sqrt[2]];
	Compile[{{R, _Real}}, Evaluate@f, CompilationTarget -> "C", RuntimeOptions -> "Speed"]
];


rR[r_ /; Abs[r - 1] < 0.001] := 1.0;
rR[r_ /; Abs[r - 1 / Sqrt@2] < 0.001] := 0.5;
rR[r_?NumericQ] := rCompiled[Abs@r];
hyperbolicize[cR_, p_] := p /. Polygon[x_] :> Polygon[# / Norm[#] rR[Norm[#] / cR]& /@ N[x]];


faceBreak[p_] := p /. Polygon[x_] :> (Polygon[Append[#, Mean[x]]]&) /@ Partition[Append[x, First[x]], 2, 1];
edgeBreak[p_] := p /. Polygon[x_] :> Polygon /@ (Append[Partition[RotateRight[Riffle[x, #]], 3, 2, 1], #]&[Mean /@ Partition[x, 2, 1, 1]]);


faces = Block[
	{poly = "Dodecahedron"},
	hyperbolicize[
		PolyhedronData[poly, "Circumradius"],
		Nest[edgeBreak@faceBreak@#&, Normal@PolyhedronData[poly, "Faces", "Polygon"], 2]
	]
];
Export["Object.mx", Flatten[faces], "WXF", PerformanceGoal -> "Size"]
