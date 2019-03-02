(* ::Package:: *)

(* ::Title:: *)
(*The Cover Image *)


(* ::Subtitle:: *)
(*Making the perforated hyperbolic dodecahedron symbolizing Mathematica 4*)


(* ::Item:: *)
(*Michael Trott*)
(*Copyright Michael Trott 1998*)


(* ::Section:: *)
(*Outline of the Procedure*)


(* ::Subsubsection:: *)
(*Step 1*)


(* ::Text:: *)
(*We start by building a regular dodecahedron with its center at the origin and with each vertex a unit distance from the origin.*)


(* ::Subsubsection:: *)
(*Step 2*)


(* ::Text:: *)
(*The faces of the dodecahedron are then subdivided into right triangles, each of which is then scaled down a bit so that gaps are introduced between them.*)


(* ::Subsubsection:: *)
(*Step 3*)


(* ::Text:: *)
(*Each triangle is further subdivided so that, when the surfaces are transformed in the next step, the surfaces which result will remain comparatively smooth.*)


(* ::Subsubsection:: *)
(*Step 4*)


(* ::Text:: *)
(*All coordinates are transformed by  \!\(\*OverscriptBox[\(r\), \(\[RightVector]\)]\)\[LongRightArrow]\!\(\*OverscriptBox[\(r\), \(\[RightVector]\)]\) f(r), where the function f(r) maps each distance from the origin onto a new distance. In particular, f(r) moves the center of each face of the dodecahedron toward the origin but moves the vertices further away, up to a fixed maximum distance. Since f(r) is scalar, this transformation does not change the direction between the origin and each point, just the distance; we'll call it the radial transformation.*)


(* ::Subsubsection:: *)
(*Step 5*)


(* ::Text:: *)
(*Each triangle is given depth by connecting it to a slightly smaller copy moved closer to the origin.*)


(* ::Subsubsection:: *)
(*Step 6*)


(* ::Text:: *)
(*The faces of the resulting body are assigned colors.*)


(* ::Section:: *)
(*Step 1: The Regular Dodecahedron*)


(* ::Text:: *)
(*First we load the standard Mathematica package giving the properties of the Platonic solids.*)


Needs["Graphics`Polyhedra`"]


(* ::Text:: *)
(*Here is our starting point, the regular dodecahedron.*)


Show[Polyhedron[Dodecahedron]]


(* ::Section:: *)
(*Step 2: Dividing into Triangles*)


(* ::Text:: *)
(*SolidToTriangles is a function which breaks the faces of a Platonic solid into right triangles.*)


SolidToTriangles[solid_] :=
	Flatten[Function[{x}, {{First[#], Plus @@ # / 2, Last[x]},
		{Last[#], Plus @@ # / 2, Last[x]}}& /@ First[x]] /@ ({Partition[Append[#, First[#]], 2, 1], Plus @@ # / Length[#]}&) /@
		Map[# / Sqrt[#.#]&, First /@ First[N[Polyhedron[solid]]], {-2}], 2];


(* ::Text:: *)
(*Applied to the dodecahedron, here is the result.*)


Show[Graphics3D[Polygon /@ (Subscript[\[ScriptH], 1] = SolidToTriangles[Dodecahedron])]]


(* ::Text:: *)
(*Triangulate divides a right triangle into 2^n smaller right triangles.*)


Triangulate[triangles_, n_Integer] :=
	Nest[Flatten[Apply[Function[{x, y, z}, {{x, #, y}, {z, #, y}}&[
		(x + (y - x).# #&)[# / Sqrt[#.#]&[z - x]]]], #, {1}], 1]&,
		triangles, n]


(* ::Text:: *)
(*This shows the result with n=2.*)


Show[Graphics3D[Polygon /@ (Subscript[\[ScriptH], 2] = Triangulate[Subscript[\[ScriptH], 1], 2])]]


(* ::Text:: *)
(*ShrinkTriangles scales each triangle toward its center of gravity\[LongDash]that is, toward the average of its vertices\[LongDash]by a given factor. A scaling factor of 1 means no change; a factor of 1/2 halves the size.*)


ShrinkTriangles[triangles_, \[Sigma]_] := Function[x, (Function[x, # + \[Sigma] (x - #)] /@ x&)[Plus @@ x / 3]] /@ triangles


(* ::Text:: *)
(*Shrinking the triangles by a factor 0.8 gives us this:*)


Show[Graphics3D[Polygon /@ (Subscript[\[ScriptH], 3] = ShrinkTriangles[Subscript[\[ScriptH], 2], 0.8])]]


(* ::Section:: *)
(*Step 3: Subdividing the Triangles*)


(* ::Text:: *)
(*Subdivide subdivides any triangle\[LongDash]not just right triangles\[LongDash]into m^2 smaller triangles.*)


Subdivide[triangles_, m_] :=
	Flatten[Apply[Function[{p, q, r}, MapThread[Append, {Flatten[Join[#1, Rest[#1]], 1]&[Partition[#1, 2, 1]& /@ Drop[#1, -1]],
		Flatten[Join[Rest[#1], Drop[Rest[#1], -1]& /@ Drop[#1, -1]], 1]}&[Table[ p + (i (q - p) + j (r - p)) / m, {j, 0, m}, {i, 0, m - j}]]]],
		triangles, {1}], 1]


(* ::Text:: *)
(*For example, we can extract the list of triangles making one face of the dodecahedron.*)


Subscript[\[ScriptH], 4] = Take[Subscript[\[ScriptH], 3], {41, 80}];


(* ::Text:: *)
(*Here is the face after subdividing each triangle into 4^2==16 smaller triangles.*)


Show[Graphics3D[Polygon /@ (Subscript[\[ScriptH], 5a] = Subdivide[Subscript[\[ScriptH], 4], 4]) ]]


(* ::Text:: *)
(*SubdivideEdges takes the edges of the larger triangles\[LongDash]the ones produced in step 2\[LongDash]and subdivides them.*)


SubdivideEdges[triangles_, m_] :=
	Apply[Function[{p, q, r}, Join @@ Apply[Table[#1 + i / m(#2 - #1), {i, 0, m}]&,
		{{p, q}, {q, r}, {r, p}}, {1}]], triangles, {1}]


(* ::Text:: *)
(*In this diagram the points of subdivision are marked.*)


Show[Graphics3D[{{Hue[0], Line /@ ( Subscript[\[ScriptH], 5b] = SubdivideEdges[Subscript[\[ScriptH], 4], 3])},
	{PointSize[.02], Map[Point, Subscript[\[ScriptH], 5b], {2}]}} ]]


(* ::Section:: *)
(*Steps 4 and 5: Radial Transformation and Thickening*)


(* ::Text:: *)
(*The next task is to create a function for calculating the radial transformation.*)


attract[x_, \[Eta]_, \[Phi]_, triangles_] :=
	Module[{xMin, xMax, X, \[Delta]x},
		{xMin, xMax} = Sqrt[{\[Phi] Max[#], Min[#]}&[#.#& /@ Level[triangles, {-2}]]];X = (xMin + xMax) / 2;\[Delta]x = xMax - X;Evaluate //@ Re[If[x < X, Sqrt[1 - (x - X)^2 / \[Delta]x^2], \[Eta] (1 - Sqrt[1 - (x - X)^2 / \[Delta]x^2]) + 1]]];


(* ::Text:: *)
(*As the following plot shows, this function has one of three different behaviors, depending on the initial distance of a point from the origin. For points nearest the origin, the transformation draws them closer still. All points beyond a certain distance from the origin are moved to a fixed limit. And points between these two ranges are moved away from the origin.*)


Plot[attract[x, 0.6, 0.96, Subscript[\[ScriptH], 2]], {x, 0.795, 1},
	PlotRange -> All, Frame -> True, Axes -> False, PlotStyle -> {Hue[0]}];


(* ::Text:: *)
(*Each polygon has zero thickness, so the ThickenTransform builds solid 3D shapes by matching each polygon with a slightly offset, slightly smaller copy of itself, and then joining the corresponding edges with new polygons.*)


ThickenTransform[f_, triangles_, polys_, edges_, \[Xi]_, \[Eta]_] :=
	Module[
		{l = Map[f, edges, {-2}], p = Map[f, polys, {-2}], outerPolys, innerPolys, radialPolys, outerEdges, innerEdges, radialEdges},
		{outerPolys, innerPolys} = {Polygon /@ p, Polygon /@ Map[\[Xi] #&, p, {-2}]};
		innerPolys = Polygon /@ Map[\[Xi] #&, p, {-2}];
		radialPolys = Polygon /@ (Join[#[[1]], Reverse[#[[2]]]]&) /@
			Transpose[{Flatten[Partition[#, 2, 1]& /@ l, 1],
				Flatten[Partition[#, 2, 1]& /@ Map[\[Xi] #&, l, {-2}], 1]}];
		{outerEdges, innerEdges} = {Line /@ l, Line /@ Map[\[Xi] #&, l, {-2}]};
		radialEdges = Line /@ Transpose[{#, Map[\[Xi] #&, #, {-2}]}&[
			Map[f, Flatten[triangles, 1], {-2}]]];{outerPolys, innerPolys, radialPolys, outerEdges, innerEdges, radialEdges}]


(* ::Text:: *)
(*Here is the result of radial transformation and thickening.*)


Show[Graphics3D[ThickenTransform[
	Evaluate[attract[Sqrt[#.#], 0.6, 0.94, Subscript[\[ScriptH], 2]] # / Sqrt[#.#]]&,
	Subscript[\[ScriptH], 4], Subscript[\[ScriptH], 5 a], Subscript[\[ScriptH], 5 b], 0.88, 0.6]],
	PlotRange -> All, Axes -> True];


(* ::Text:: *)
(*And here is a cross-section through the resulting surface.*)


Show[%, PlotRange -> {All, {0, 1}, All}, ViewPoint -> {0, -4, 0}];


(* ::Section:: *)
(*Final Assembly and Coloring the Faces Individually*)


(* ::Text:: *)
(*Incorporating the individual functions defined above leads us directly to the function CoverImage.*)


CoverImage[solid_, {n_Integer, m_Integer, \[Xi]_, \[Sigma]_}, \[Eta]_, \[Phi]_] :=
	Module[{l = Length[First[Polyhedron[solid]]], h1, h2, h3, h4a, h4b, h5},
		h1 = SolidToTriangles[solid];
		h2 = Triangulate[h1, n];
		h3 = ShrinkTriangles[h2, \[Sigma]];
		h4a = Subdivide[h3, m];
		h4b = SubdivideEdges[h3, m];
		h5 = Apply[ThickenTransform[GG = Evaluate[attract[Sqrt[#.#], \[Eta], \[Phi], h2] # / Sqrt[#.#]]&, ##, \[Xi], 0.6]&,
			Transpose[{Partition[h3, Length[h3] / l], Partition[h4a, Length[h4a] / l],
				Partition[h4b, Length[h3] / l]}], {1}]]


(* ::Text:: *)
(*This example generates a slightly simplified version of the final cover image.*)


CoverImage[Dodecahedron, {3, 4, 0.96, .85}, 0.6, 0.955];


With[{colors = {0.63, 0.97, 0.7, 0.7, 0.38, 0.15, 0.7, 0.7, 0.7, 0.85, 0.6, 0.7}},
	Show[Graphics3D[{EdgeForm[], Thickness[0.00023],
		Transpose[{SurfaceColor[Hue[#, 0.85, 1], Hue[#, 0.2, 1], 3.2]& /@ colors,
			CoverImage[Dodecahedron, {3, 4, 0.96, .85}, 0.6, 0.955]}]}],
		Boxed -> False, PlotRange -> All, ViewPoint -> {2, -3, 3.3}]];


(* ::Text:: *)
(*By taking out one of the faces, you can have a look inside the previous example.*)


Show[MapAt[Delete[#, 2]&,%, {1, 3}], ViewPoint -> {0.9206, 0, 0.4604}];


(* ::Text:: *)
(*Here is the actual input used for the cover image. Since the faces of the dodecahedron have been divided into a great many polygons, the calculation of the graphic may be beyond the memory capacity of your computer.*)


With[{colors = {0.63, 0.97, 0.7, 0.7, 0.38, 0.15, 0.7, 0.7, 0.7, 0.85, 0.6, 0.7}},
	Show[Graphics3D[{EdgeForm[], Thickness[0.00001], Transpose[{SurfaceColor[Hue[#, 0.85, 1], Hue[#, 0.2, 1], 3.2]& /@ colors, CoverImage[Dodecahedron, {6, 6, 0.96, 0.85}, 0.6, 0.955]}]}],
		Boxed -> False, PlotRange -> All, ViewPoint -> {2, -3, 3.3}]];


(* ::Section:: *)
(*Some Variants*)


(* ::Text:: *)
(*The above implementation can be used with other Platonic solids, radial transformations, and coloring schemes. *)
(*Here is a cube (hexahedron) with its vertices pushed far outwards and without a fixed outer limit. Each face is assigned a randomly chosen color.*)


With[{colors = Table[Random[], {6}]},
	Show[Graphics3D[{EdgeForm[], Thickness[0.00023], Transpose[{SurfaceColor[Hue[#, 0.35, 0.8], Hue[#, 0.28, 0.9], 2.7]& /@ colors, CoverImage[Hexahedron, {4, 4, 0.9, 0.9}, 1.4, 1]}]}],
		Boxed -> False, PlotRange -> All]];


(* ::Text:: *)
(*Here is a view into the last example.*)


Show[%, PlotRange -> {All, {0, 1.2}, All}, ViewPoint -> {0.3, -2.6, 0.4}];


(* ::Text:: *)
(*Here is an icosahedron.*)


With[{colors = Range[20] / 20},
	Show[Graphics3D[{EdgeForm[], GrayLevel[1], Thickness[0.0001], Transpose[{SurfaceColor[Hue[#, 0.6, 1], Hue[#, 0.7, 0.7], 3.4]& /@ colors, CoverImage[Icosahedron, {3, 3, 0.95, 0.8}, 0.7, 0.99]}]}],
		Boxed -> False, PlotRange -> All, Background -> GrayLevel[0]]];


(* ::Text:: *)
(*Here is a tetrahedron, with all faces given the same color.*)


With[{colors = Table[0.18, {4}]},
	Show[Graphics3D[{EdgeForm[], Hue[0], Thickness[0.0001], Transpose[{SurfaceColor[Hue[#], Hue[#], 2.1]& /@ colors, CoverImage[Tetrahedron, {3, 3, 0.98, 0.92}, 1.6, 1]}]}],
		Boxed -> False, PlotRange -> All, Background -> Hue[0.72]]];


(* ::Text:: *)
(*The last example can be rotated and stretched.*)


Module[{cv = CoverImage[Tetrahedron, {3, 5, 0.96, 0.86}, 1.6, 1], zMin, zMax, \[ScriptCapitalR]},
	{zMin, zMax} = {Min[#], Max[#]}&[Last[Transpose[Level[Cases[cv, _Line, \[Infinity]], {-2}]]]];
	\[ScriptCapitalR][{x_, y_, z_}] := Module[{\[CurlyPhi] = (z - zMin) / (zMax - zMin) 0.8\[Pi]}, {{Cos[\[CurlyPhi]], Sin[\[CurlyPhi]], 0}, {-Sin[\[CurlyPhi]], Cos[\[CurlyPhi]], 0}, {0, 0, 1}}.{x, y, 2z}];
	With[{colors = Table[0.28, {4}]},
		Show[Graphics3D[{EdgeForm[], Hue[0], Thickness[0.0001], Transpose[{SurfaceColor[Hue[#], Hue[#], 2.1]& /@ colors, cv /. (lp : (Line | Polygon))[l_] :> lp[\[ScriptCapitalR] /@ l]}]}],
			Boxed -> False, PlotRange -> All, Background -> Hue[0.72]]]];
