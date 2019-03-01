(* ::Package:: *)

SetDirectory@NotebookDirectory[];


Begin["`MathematicaV1`"];
$ = Association[];
If[
	!FileExistsQ["Object.mx"],
	$["Build"] = VerificationTest[Get["build.m"], _String, SameTest -> MatchQ]
];
End[]
