(* ::Package:: *)

dodecahedronTriangles = Block[
	{faces, f, v},
	faces = Polygon /@ PolyhedronData["Dodecahedron", "FaceIndices"];
	f[l_] := Module[
		{mp = 1 / 5 Plus @@ l[[1]]},
		(Polygon[Append[#, mp]]&) /@ Partition[Append[l[[1]], l[[1, 1]]], 2, 1]
	];
	v = PolyhedronData["Dodecahedron", "VertexCoordinates"][[#]]&;
	N[Flatten[f /@ N[Map[v, faces, {-1}], 100]]]
];
{Subscript[ρ, min], Subscript[ρ, max]} = {Min[#], Max[#]}&[Norm /@ Union[Level[dodecahedronTriangles, {-2}]]];


splitLIntoFourLs[Polygon[l_]] := Block[
	{f},
	f = {
		{2 #1 + 2 #2, 4 #2, 2 #2 + 2 #3, #2 + #3 + 2 #5, 2 #2 + 2 #5, #1 + #2 + 2 #5, 2 #1 + 2 #2},
		{#1 + #2 + 2 #6, 2 #2 + 2 #5, #2 + #3 + 2 #4, 2 #4 + 2 #5, 4 #5, 2 #5 + 2 #6, #1 + #2 + 2 #6},
		{2 #2 + 2 #3, 4 #3, 4 #4, 2 #4 + 2 #5, #2 + #3 + 2 #4, #2 + #3 + 2 #5, 2 #2 + 2 #3},
		{4 #6, 4 #1, 2 #1 + 2 #2, #1 + #2 + 2 #5, #1 + #2 + 2 #6, 2 #5 + 2 #6, 4 #6}
	} / 4&;
	Map[Polygon, f @@ l]
];
Ls[1] = Map[Polygon , {
	{{0, 2}, {0, 0}, {2, 0}, {2, 1}, {1, 1}, {1, 2}, {0, 2}},
	{{0, 3}, {2, 3}, {2, 1}, {1, 1}, {1, 2}, {0, 2}, {0, 3}}
} / 2];
Ls[k_] := Ls[k] = Flatten[splitLIntoFourLs /@ Ls[k - 1], 1];


mapPointToDodecahedronTriangle[{x_, y_}, tri : Polygon[{p1_, p2_, p3_}]] := 1 / 3 (p3 (3 - 2 y) + 2 (p1 - p1 x + p2 x) y);
mapLToDodecahedronTriangle[L_, tri : Polygon[{p1_, p2_, p3_}] ] := Map[mapPointToDodecahedronTriangle[#, tri ]&, L, {2}];
LsOnDodecahedron = Function[t, mapLToDodecahedronTriangle[#, t]& /@ Ls[4]] /@ dodecahedronTriangles;
contractL[L : Polygon[l_]] := With[
	{mp = (l[[2]] + l[[5]]) / 2, α = 0.8},
	Polygon[(mp + α (# - mp)&) /@ l]
];


baseColor[ξ_] := Hue[0.1 - 0.11ξ, 0.5 + 0.5ξ, 1];
baseOpacity[ξ_] := Opacity[0.35 + 0.65(1 - (1 - ξ^2)^(1 / 2))];
extensionColor[ξ_] := Hue[
	0.05 - 0.2 If[
		(1 - (1 - ξ^2)^(1 / 2)) > 0.3, 0.3,
		(1 - (1 - ξ^2)^(1 / 2))
	],
	0.5 + 0.8ξ,
	1 - 0.35(1 - (1 - ξ^2)^(1 / 2))
];
extensionOpacity[ξ_] := Opacity[0.4 + 0.4ξ];
extensionSpecularExponent[ξ_] := 2.5ξ;


\[ScriptF][r_] := Re[1 / 2 (ArcSin[2 r - 1] + π / 2)];
ℱ[α_, xyz_, f_] := α xyz f[(Sqrt[xyz.xyz] - Subscript[ρ, min]) / (Subscript[ρ, max] - Subscript[ρ, min])];
addHatToL[L : Polygon[l_]] := Module[
	{mp = Plus @@ l / 7, qs, \[ScriptR], φ, mpF, rs},
	qs = (ℱ[1, #, \[ScriptF]]&) /@ l;
	\[ScriptR] = Sqrt[#.#]&[Plus @@ l / 7];
	φ = (\[ScriptR] - Subscript[ρ, min]) / (Subscript[ρ, max] - Subscript[ρ, min]);
	mpF = ℱ[1.06, mp, \[ScriptF]];
	rs = (ℱ[1.06, mp + 0.6 (# - mp), \[ScriptF]]&) /@ l;
	{
		{
			extensionColor[φ],
			Specularity[extensionColor[φ], extensionSpecularExponent[φ]],
			extensionOpacity[φ],
			Polygon[Append[#1, mpF]]& /@ Partition[Append[rs, First[rs]], 2, 1]
		},
		{
			baseColor[RandomReal[{φ - 0.2, φ + 0.2}]],
			Specularity[baseColor[φ], 2.3],
			baseOpacity[φ],
			Polygon[Join[#1, Reverse[#2]]]& @@@ Transpose[Partition[Append[#, First[#]], 2, 1]& /@ {qs, rs}]
		}
	}
];


faces = Flatten@{
	EdgeForm[],
	addHatToL /@ Take[Flatten[Take[LsOnDodecahedron, All]], All]
};
Export["Object.mx", Flatten[faces], "WXF", PerformanceGoal -> "Size"]