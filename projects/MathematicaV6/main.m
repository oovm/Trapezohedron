(* ::Package:: *)

SetDirectory@NotebookDirectory[];
Trapezohedron`fixUnicode@"source.m";
Trapezohedron`fixUnicode@"build.m";


Begin["`MathematicaV6`"];
$ = Association[];
If[
	!FileExistsQ["Object.mx"],
	$["Build"] = VerificationTest[Get["build.m"], _String, SameTest -> MatchQ]
];
Graphics3D[
	Import@"Object.mx",
	PlotRange -> All, Boxed -> False, ImageSize -> 300,
	Lighting -> {
		{"Ambient", RGBColor[0.2, 0, 0]},
		{"Point", RGBColor[0.4, 0.4, 0.4], {2, 0, 2}},
		{"Point", RGBColor[0.4, 0.4, 0.4], {2, 2, 2}},
		{"Point", RGBColor[0.4, 0.4, 0.4], {0, 2, 2}},
		{"Point", RGBColor[0.2, 0, 0], {-2, -2, -2}}
	}
]
End[]
