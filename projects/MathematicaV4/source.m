(* ::Package:: *)

(* ::Title:: *)
(*The Cover Image *)


(* ::Subtitle:: *)
(*Making the perforated hyperbolic dodecahedron symbolizing Mathematica 4*)


(* ::Item:: *)
(*Michael Trott*)
(*Copyright Michael Trott 1998*)


(* ::Section::Closed:: *)
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
(*All coordinates are transformed by  \!\(\*OverscriptBox[\(r\), \(⇀\)]\)⟶\!\(\*OverscriptBox[\(r\), \(⇀\)]\) f(r), where the function f(r) maps each distance from the origin onto a new distance. In particular, f(r) moves the center of each face of the dodecahedron toward the origin but moves the vertices further away, up to a fixed maximum distance. Since f(r) is scalar, this transformation does not change the direction between the origin and each point, just the distance; we'll call it the radial transformation.*)


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


(* ::Text:: *)
(*Here is our starting point, the regular dodecahedron.*)


PolyhedronData["Dodecahedron"]


(* ::Section:: *)
(*Step 2: Dividing into Triangles*)


(* ::Text:: *)
(*SolidToTriangles is a function which breaks the faces of a Platonic solid into right triangles.*)


SolidToTriangles[solid_] := Block[
	{faces, f, g},
	faces = Map[# / Sqrt[#.#]&, N@PolyhedronData[solid, "Faces", "Polygon"][[1, All, 1]], {-2}];
	f = {Partition[Append[#, First[#]], 2, 1], Plus @@ # / Length[#]}&;
	g[x_] := {{First[#], Plus @@ # / 2, Last[x]}, {Last[#], Plus @@ # / 2, Last[x]}}& /@ First[x] ;
	Flatten[g /@ f /@ faces, 2]
];


(* ::Text:: *)
(*Applied to the dodecahedron, here is the result.*)


Graphics3D@Map[Polygon, Subscript[h, 1] = SolidToTriangles["Dodecahedron"]]


(* ::Text:: *)
(*Triangulate divides a right triangle into 2^n smaller right triangles.*)


Triangulate[triangles_, n_Integer] := Block[
	{f},
	f[x_, y_, z_] := {{x, #, y}, {z, #, y}}&[(x + (y - x).# #&)[# / Sqrt[#.#]&[z - x]]];
	Nest[Flatten[Apply[f, #, {1}], 1]&, triangles, n]
]


(* ::Text:: *)
(*This shows the result with n=2.*)


Graphics3D@Map[Polygon, Subscript[h, 2] = Triangulate[Subscript[h, 1], 2]]


(* ::Text:: *)
(*ShrinkTriangles scales each triangle toward its center of gravity—that is, toward the average of its vertices—by a given factor. A scaling factor of 1 means no change; a factor of 1/2 halves the size.*)


ShrinkTriangles[triangles_, σ_] := Block[
	{f},
	f[x_] := (Function[y, # + σ (y - #)] /@ x&)[Plus @@ x / 3];
	f /@ triangles
]


(* ::Text:: *)
(*Shrinking the triangles by a factor 0.8 gives us this:*)


Graphics3D@Map[Polygon, Subscript[h, 3] = ShrinkTriangles[Subscript[h, 2], 0.8]]


(* ::Section:: *)
(*Step 3: Subdividing the Triangles*)


(* ::Text:: *)
(*Subdivide subdivides any triangle not just right triangles into m^2 smaller triangles.*)


SubdivideTriangle[triangles_, m_] := Block[
	{f, g},
	f[x_] := {
		Flatten[Join[#1, Rest[#1]], 1]&[Partition[#1, 2, 1]& /@ Drop[x, -1]],
		Flatten[Join[Rest@x, Drop[Rest[#1], -1]& /@ Drop[x, -1]], 1]
	};
	g[p_, q_, r_] := MapThread[
		Append,
		f@Table[ p + (i (q - p) + j (r - p)) / m, {j, 0, m}, {i, 0, m - j}]
	];
	Flatten[Apply[g, triangles, {1}], 1]
]


(* ::Text:: *)
(*For example, we can extract the list of triangles making one face of the dodecahedron.*)


Subscript[h, 4] = Take[Subscript[h, 3], {41, 80}];


(* ::Text:: *)
(*Here is the face after subdividing each triangle into 4^2==16 smaller triangles.*)


Graphics3D@Map[Polygon, Subscript[h, 5a] = SubdivideTriangle[Subscript[h, 4], 4] ]


(* ::Text:: *)
(*SubdivideEdges takes the edges of the larger triangles the ones produced in step 2 and subdivides them.*)


SubdivideEdges[triangles_, m_] := Block[
	{f, g},
	f[x_, y_] := Table[x + i / m(y - x), {i, 0, m}];
	g[p_, q_, r_] := Join @@ Apply[f, {{p, q}, {q, r}, {r, p}}, {1}];
	Apply[g, triangles, {1}]
];


(* ::Text:: *)
(*In this diagram the points of subdivision are marked.*)


Graphics3D@{
	{Hue[0], Line /@ ( Subscript[h, 5b] = SubdivideEdges[Subscript[h, 4], 3])},
	{PointSize[.02], Map[Point, Subscript[h, 5b], {2}]}
}


(* ::Section:: *)
(*Steps 4 and 5: Radial Transformation and Thickening*)


(* ::Text:: *)
(*The next task is to create a function for calculating the radial transformation.*)


Attract[x_, η_, ϕ_, triangles_] := Block[
	{xMin, xMax, X, δx},
	{xMin, xMax} = Sqrt[{ϕ Max[#], Min[#]}&[#.#& /@ Level[triangles, {-2}]]];
	X = (xMin + xMax) / 2;
	δx = xMax - X;
	Evaluate //@ Re[If[x < X, Sqrt[1 - (x - X)^2 / δx^2], η (1 - Sqrt[1 - (x - X)^2 / δx^2]) + 1]]
];


(* ::Text:: *)
(*As the following plot shows, this function has one of three different behaviors, depending on the initial distance of a point from the origin. For points nearest the origin, the transformation draws them closer still. All points beyond a certain distance from the origin are moved to a fixed limit. And points between these two ranges are moved away from the origin.*)


Plot[
	Attract[x, 0.6, 0.96, Subscript[h, 2]], {x, 0.795, 1},
	PlotRange -> All, Frame -> True, Axes -> False, PlotStyle -> {Hue[0]}
]


(* ::Text:: *)
(*Each polygon has zero thickness, so the ThickenTransform builds solid 3D shapes by matching each polygon with a slightly offset, slightly smaller copy of itself, and then joining the corresponding edges with new polygons.*)


ThickenTransform[f_, triangles_, polys_, edges_, ξ_, η_] := Block[
	{l = Map[f, edges, {-2}], p = Map[f, polys, {-2}], outer, inner, radial, outerEdges, innerEdges, radialEdges},
	{outer, inner} = {Polygon /@ p, Polygon /@ Map[ξ #&, p, {-2}]};
	inner = Polygon /@ Map[ξ #&, p, {-2}];
	radial = Polygon /@ (Join[#[[1]], Reverse[#[[2]]]]&) /@ Transpose[
		{
			Flatten[Partition[#, 2, 1]& /@ l, 1],
			Flatten[Partition[#, 2, 1]& /@ Map[ξ #&, l, {-2}], 1]
		}
	];
	{outerEdges, innerEdges} = {Line /@ l, Line /@ Map[ξ #&, l, {-2}]};
	radialEdges = Line /@ Transpose[{#, Map[ξ #&, #, {-2}]}&[Map[f, Flatten[triangles, 1], {-2}]]];
	{outer, inner, radial, outerEdges, innerEdges, radialEdges}
];


(* ::Text:: *)
(*Here is the result of radial transformation and thickening.*)


Graphics3D[
	ThickenTransform[
		Evaluate[Attract[Sqrt[#.#], 0.6, 0.94, Subscript[h, 2]] # / Sqrt[#.#]]&,
		Subscript[h, 4], Subscript[h, 5 a], Subscript[h, 5 b], 0.88, 0.6
	],
	PlotRange -> All, Axes -> True
]


(* ::Text:: *)
(*And here is a cross-section through the resulting surface.*)


Show[%, PlotRange -> {All, {0, 1}, All}, ViewPoint -> {0, -4, 0}]


(* ::Section:: *)
(*Step 6: Final Assembly and Coloring the Faces Individually*)


(* ::Text:: *)
(*Incorporating the individual functions defined above leads us directly to the function CoverImage.*)


Spikey[solid_, {n_Integer, m_Integer, ξ_, σ_}, η_, ϕ_] := Block[
	{l, h2, h3, h4a, h4b, h5, f},
	l = Length[First[PolyhedronData[solid, "Faces", "Polygon"]]];
	h2 = Triangulate[SolidToTriangles@solid, n];
	h3 = ShrinkTriangles[h2, σ];
	h4a = SubdivideTriangle[h3, m];
	h4b = SubdivideEdges[h3, m];
	f = ThickenTransform[Evaluate[Attract[Sqrt[#.#], η, ϕ, h2] # / Sqrt[#.#]]&, ##, ξ, 0.6]&;
	f @@@ Transpose[{
		Partition[h3, Length[h3] / l],
		Partition[h4a, Length[h4a] / l],
		Partition[h4b, Length[h3] / l]
	}]
];


(* ::Text:: *)
(*This example generates a slightly simplified version of the final cover image.*)


Block[
	{
		cv = Spikey["Dodecahedron", {3, 4, 0.96, .85}, 0.6, 0.955],
		colors = {0.63, 0.97, 0.7, 0.7, 0.38, 0.15, 0.7, 0.7, 0.7, 0.85, 0.6, 0.7},
		sf = SurfaceColor[Hue[#, 0.85, 1], Hue[#, 0.2, 1], 3.2]&
	},
	Graphics3D[{
		EdgeForm[], Thickness[0.00023],
		Transpose@{sf /@ colors, cv}
	},
		Boxed -> False, PlotRange -> All,
		ViewPoint -> {2, -3, 3.3}
	]
]


(* ::Text:: *)
(*By taking out one of the faces, you can have a look inside the previous example.*)


Show[MapAt[Delete[#, 2]&,%, {1, 3}], ViewPoint -> {0.9206, 0, 0.4604}]


(* ::Text:: *)
(*Here is the actual input used for the cover image. Since the faces of the dodecahedron have been divided into a great many polygons, the calculation of the graphic may be beyond the memory capacity of your computer.*)


With[
	{
		cv = Spikey["Dodecahedron", {3, 4, 0.96, .85}, 0.6, 0.955],
		colors = {0.63, 0.97, 0.7, 0.7, 0.38, 0.15, 0.7, 0.7, 0.7, 0.85, 0.6, 0.7},
		sf = SurfaceColor[Hue[#, 0.85, 1], Hue[#, 0.2, 1], 3.2]&
	},
	Graphics3D[{
		EdgeForm[], Thickness[0.00001],
		Transpose[{sf /@ colors, cv}]
	},
		Boxed -> False, PlotRange -> All,
		ViewPoint -> {2, -3, 3.3}
	]
]


(* ::Section:: *)
(*Some Variants*)


(* ::Text:: *)
(*The above implementation can be used with other Platonic solids, radial transformations, and coloring schemes. *)
(*Here is a cube (hexahedron) with its vertices pushed far outwards and without a fixed outer limit. Each face is assigned a randomly chosen color.*)


With[
	{
		sf = SurfaceColor[Hue[#, 0.35, 0.8], Hue[#, 0.28, 0.9], 2.7]&,
		colors = Table[RandomReal[], {6}],
		poly = Spikey["Hexahedron", {4, 4, 0.9, 0.9}, 1.4, 1]
	},
	Graphics3D[{
		EdgeForm[], Thickness[0.00023],
		Transpose[{sf /@ colors, poly}]
	},
		Boxed -> False, PlotRange -> All
	]
];


(* ::Text:: *)
(*Here is a view into the last example.*)


Show[%, PlotRange -> {All, {0, 1.2}, All}, ViewPoint -> {0.3, -2.6, 0.4}];


(* ::Text:: *)
(*Here is an icosahedron.*)


With[
	{
		sf = SurfaceColor[Hue[#, 0.6, 1], Hue[#, 0.7, 0.7], 3.4]&,
		colors = Range[20] / 20,
		poly = Spikey["Icosahedron", {3, 3, 0.95, 0.8}, 0.7, 0.99]
	},
	Graphics3D[{
		EdgeForm[], GrayLevel[1], Thickness[0.0001],
		Transpose[{sf /@ colors, poly}]
	},
		Boxed -> False, PlotRange -> All
	]
]


(* ::Text:: *)
(*Here is a tetrahedron, with all faces given the same color.*)


With[
	{
		sf = SurfaceColor[Hue[#], Hue[#], 2.1]& ,
		colors = Table[0.18, {4}],
		poly = Spikey["Tetrahedron", {3, 3, 0.98, 0.92}, 1.6, 1]
	},
	Graphics3D[{
		EdgeForm[], Hue[0], Thickness[0.0001],
		Transpose[{sf /@ colors, poly}]
	},
		Boxed -> False, PlotRange -> All, Background -> Hue[0.72]
	]
]


(* ::Text:: *)
(*The last example can be rotated and stretched.*)


Block[
	{
		colors = Table[0.28, {4}],
		cv = Spikey["Tetrahedron", {3, 5, 0.96, 0.86}, 1.6, 1],
		zMin, zMax, ℛ
	},
	{zMin, zMax} = {Min[#], Max[#]}&[Last[Transpose[Level[Cases[cv, _Line, Infinity], {-2}]]]];
	ℛ[{x_, y_, z_}] := Block[
		{φ = (z - zMin) / (zMax - zMin) 0.8π},
		{{Cos[φ], Sin[φ], 0}, {-Sin[φ], Cos[φ], 0}, {0, 0, 1}}.{x, y, 2z}
	];
	Graphics3D[{
		EdgeForm[], Hue[0], Thickness[0.0001],
		Transpose@{
			SurfaceColor[Hue[#], Hue[#], 2.1]& /@ colors,
			cv /. (lp : (Line | Polygon))[l_] :> lp[ℛ /@ l]
		}
	},
		Boxed -> False, PlotRange -> All,
		Background -> Hue[0.72]
	]
]