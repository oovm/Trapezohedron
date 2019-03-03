(* ::Package:: *)

SetDirectory@NotebookDirectory[];


Begin["`MathematicaV2`"];
$ = Association[];
If[
	!FileExistsQ["Object.mx"],
	$["Build"] = VerificationTest[Get["source.m"], _String, SameTest -> MatchQ]
];
g = Graphics3D[
	faces,
	PlotRange -> All, Boxed -> False,
	SphericalRegion -> True, (*ViewVector\[Rule]{1,3,3},*)
	Lighting -> {
		{"Directional", Red, ImageScaled[{2, 2, 2}]},
		{"Directional", Green, ImageScaled[{-2, 2, 2}]},
		{"Directional", Blue, ImageScaled[{-2, -2, 2}]}
	}
]
End[]
