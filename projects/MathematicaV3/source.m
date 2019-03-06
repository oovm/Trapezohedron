(* ::Package:: *)

(* ::Chapter:: *)
(*The Cover Image : Hyperbolic Platonic Bodies*)


(* ::Text:: *)
(*In this section, we construct the image on the cover of Wolfram's book Mathematica : A System for Doing Mathematics by Computer, Second Edition, the cover of The Mathematica Book, Third Edition, and the cover of The Mathematica Book, Fourth Edition.*)


(* ::Text:: *)
(*It involves a dodecahedron in hyperbolic geometry.*)


(* ::Text:: *)
(*It is not in the spirit of this book to go into the details of non-Euclidean geometry.*)
(* (Non - Euclidean geometries are those in which Euclid' s fifth axiom, the parallel postulate, does not hold.)*)


(* ::Text:: *)
(*Instead, we want to construct this body in a more heuristic way.*)
(*(it is actually not a "real" hyperbolic dodecahedron)*)


(* ::Text:: *)
(*Let us start with Igor Rivin' s picture from the second edition book.*)


(* ::Text:: *)
(*We proceed in the following steps to generate a picture that looks like the cover graphic of the second edition :*)


(* ::Item:: *)
(* Using the package Graphics`Polyhedra`, we create the faces and vertices of a classical (Euclidean) dodecahedron.*)


(* ::Item:: *)
(*Because of the fivefold symmetry of the faces, we work on just 1110 of such a face, and triangulate into 18 subtriangles.*)


(* ::Item:: *)
(*Depending on the distance of the vertices of these subtriangles from the center of an external sphere indenting the face, we reduce their distances to the center of the dodecahedron. *)
(*(This model of a sphere gives us some freedom in searching for a nice - looking result.)*)


(* ::Item:: *)
(*We reflect this 1/10 part of a face around the line between the center of the face of the dodecahedron and the center of the edge of the 1110 part of the face.*)


(* ::Item:: *)
(*By rotating the resulting part of a face by i 2 rr I 5 (i = 1, 2, 3, 4 ), we obtain the complete face.*)


(* ::Item:: *)
(*We determine the 11 rotation matrices that transform the constructed face into the other 11 faces.*)


(* ::Item:: *)
(*Using these rotation matrices, we rotate the constructed warped face to create the other 11 faces.*)


(* ::Item:: *)
(*Finally, we display the resulting 12 faces.*)


(* ::Text:: *)
(*We first read in the package.*)


Needs["Graphics`Polyhedra`"]


(* ::Text:: *)
(*We have seen this dodecahedron earlier. Here is the explicit list of its faces.*)


Short[dode = PolyhedronData["Dodecahedron"][[1]], 6]


(* ::Text:: *)
(*We denote the center of the dodecahedron by pa, a vertex by pc, and the midpoint of an edge of the first polygon of dode by pb.*)


pa = Plus @@ dode[[1, 1]] / 5
pc = dode[[1, 1, 1]]
pb = (dode[[1, 1, 2]] + dode[[1, 1, 1]]) / 2


(* ::Text:: *)
(*We begin with the triangulation of a face. *)


(* ::Text:: *)
(*We implement this ourselves, although we could also use the command GeodesateFace in the package Graphics`Polyhedra`. *)


(* ::Text:: *)
(*Here are the other points in the triangle pa-pb-pc.*)


ab = pb - pa; ac=pc - pa; bc=pc - pb;
p1 = pa + ab / 4; p2 = pa + ab / 2; p3 = pa + 3 ab / 4;
p4 = pa + ac / 4; p5 = pa + ac / 2; p6=pb + bc / 2;
p7 = pb + 3 bc / 4; p8 :=p8= p9 + (pc - p9) / 2;p9 = pa + 3 ac / 4;
p10 := p10=p9 + (p11 - p9) / 2;p11 = p9 + (pb - p9) / 2;
p12 = p11 + (p6 - p11) / 2; p13 = p11 + (pc - p11) / 2;
p14 = p5 + (pb - p5) / 2;


(* ::Text:: *)
(*Now we construct a list of vertices.*)


polys = {
	{pa, p1, p4}, {p4, p5, p1}, {p1, p2, p5},
	{p5, p2, p3}, {p5, p9, p11}, {pb, p6, p11},
	{p8, p9, p10}, {p10, p11, p13}, {p11, p12, p13},
	{p7, p12, p13}, {p6, p7, p12}, {p8, pc, p13},
	{pc, p7, p13}, {p8, p10, p13}, {p3, p5, p14},
	{p3, pb, p14}, {pb, p11, p14}, {p5, p11, p14}
};


(* ::Text:: *)
(*To better view this, we define a function shrink that shrinks the individual subtriangles.*)


shrink[poly_List, factor_] :=Module[
	{mp = (Plus @@ poly)/Length[poly]},
	mp + factor (# - mp) & /@ poly
]


(* ::Text:: *)
(*Here are the individual subtriangles in the first 1110 part of a face viewed from the top.*)


Graphics3D[
	Polygon /@ (shrink[#, 0.7] & /@ polys),
	ViewPoint -> {0, 0, 3}, Axes -> {True, True, False}
]


(* ::Text:: *)
(*Now, we must use these planar triangles, consisting of small subtriangles, to construct the inward - curved surface of the hyperbolic dodecahedron. We do this by pulling the vertices of the subtriangles toward the center. The boundary vertex pc (a vertex of the initial dodecahedron) remains in its original position. To ensure that the resulting curved edges can later be joined without problems, we move the points along the lines connecting them to the center (0, 0, 0} of the dodecahedron. We determine the size of the contraction factors using a sphere that indents the face.*)


intersectionPoint[point_, rad_] :=Module[
	{zk},
(* sphere midpoint distance *)
	zk = Sqrt[rad^2 + (pc - pa).(pc - pa)] + pa[[3]];
	point*((-Sqrt[rad^2 - point[[1]]^2 -point [ [2]]^2] + zk) /pc [[3]])
]


(* ::Text:: *)
(*Here, rad denotes the radius of this sphere. It must be greater than the following number. 2.3 Some More Complicated Three - Dimensional Graphics*)


Sqrt[(pc - pa).(pc - pa)]


(* ::Text:: *)
(* Here are the vertices on the concave surface formed from the vertices of the subtriangles. In order to avoid writing 14 equations in the following input, we use the construction Evaluate [ToExpression [ 11 \[Bullet] \[Bullet] \[Bullet] 11 <>*)
(*                         ToString [i]]].*)


p3da intersectionPoint[pa, 0.65];
p3db \[Bullet] intersectionPoint[pb, 0.65];
p3dc = intersectionPoint[pc, 0.65];
Do[Evaluate[ToExpression["p3d" <> ToString[i]]] \[Bullet]
	intersectionPoint[ToExpression[\[Bullet]p\[Bullet] <> ToString[i]], 0.65],
	{i, 14} 1


(* ::Text:: *)
(*Here are the triangles that are analogous to poly, and that approximate the curved surface.*)
	
	
	polys 3 D = (* just analogous to 2 D *)
		Map[Hold, (* r euse input from above *)
			Cases[DownValues[In], HoldPattern[ :> (polys = ;)]],
{-1} 1 [[1, 2, 1, 2]] /. Hold[p_] :> Blockl {p},
ToExpression[Stringinsert[ToString[p], "3d", 2]]];


(* ::Text:: *)
(*Here is what they look like. (We name them firstPart.)*)


ln[23 l := firstPart = Show [Graphics3D [Polygon /@ polys3D] ,
	PlotRange -> All, Axes -> True];




(* ::Text:: *)
(*To convert the first 1110 face into the first 1/5 face, we need to reflect in the "line" pa - pc. Because pa, pb, and pc all lie in a plane parallel to the x, y - plane, this is relatively simple.*)


ln[24 l := reflect[{pa_, pc_}, point_] :=
	Module[{pa2, pc2, point2, projection, perpendicular},
		pa2 = Drop[pa, -1]; pc2 = Drop[pc, -1];
		point2 = Drop[point, -1];
		(* divide in orthogonal and paral l el compo nents *)
		projection = 1/(pc2 - pa2). (pc2 - pa2)*
			(pc2 - pa2) . (point2 - pa2) (pc2 - pa2) ;
		perpendicular = (point2 - pa2) - projection;
		N[Append[projection - perpendicular, point[[3 lllll
		
		
		
		
		(* ::Text:: *)
		(*  Here is the second part, produced by reflection of firstPar t by applying reflect.*)
			
			
			secondPart = Show [Graphics3D [Polygon /@
				Map[ref1ect[{p3da, p3db}, #] &, polys3D, {2} 111;
				
				
				
				
				(* ::Text:: *)
				(* We now put first Part and secondPart together to get part [5].*)
				
				
				part [5] \[Bullet] Join [firstPart [ [1]], secondPart [ [1]]];
				Show[Graphics3D[part[S]]];
				
				
				(* ::Text:: *)
				(*We now define rotation matrices rnat [ i] to construct the four missing parts of the first face.*)
				
				
				Do [mat [i] = { { Cos [i 2 Pi/5] , Sin [i 2 Pi/5] , 0},
					{-Sin[i 2 Pi/5], Cos[i 2 Pi/5], 0},
					{ 0, 0, 1}} // N, {i, 1, 4}]
				
				
				(* ::Text:: *)
				(*   Next we use mat [i] to rotate part [5] . We call the union of all five parts face [1].*)
					
					
					Do[part[i] = Map[(mat[i].#) & :, part[S], {3}], {i, 4} l
						face[1] \[Bullet] Flatten[Tab1e[part[i], {i, S} ll;
						Show[Graphics3D[face[1]]];
						
						
						
						
						(* ::Text:: *)
						(*To implement the rotations of the first face into the positions of the remaining 11 faces, we need to do a little more. Again, we need to find 3 x 3 rotation matrices. We do this by moving three vertices of the first face of our classical (Platonic) dodecahedron to coincide with three vertices of each of the 11 faces. This results in a system of equations in nine variables whose solution gives the desired rotation matrices. *)
						
						
						A = {{all [i] , a12 [i] , a13 [i]}, {a21 [i], a22 (i], a23 [i]},
{a31 [i], a32 [i], a33 [i]} };
Do[~ [i] = (A I.
	Solve[ (* solve the equat ions *)
		Flatten[Tab1e[(* make e q uati o ns by rotating points * )
         Thread[(* the rotation matrix to be determined * )
                A.dode[[1, 1, j]] == dode[[i, 1, j]]], {j, 3}11.
              Flatten [A]]) [ [1]], {i, 12}1
             'R [ 7] is one of these rotation matrices.




(* ::Text:: *)
(* We now rotate face[1] into the other positions.*)


Do[face [i] = Map [ (~ [i]. I+)&:, face [1], {3}], {i, 2, 12}1




(* ::Text:: *)
(* We look at the resulting object.*)


Show[Graphics3D[Table[face[i], {i, 12}11. Boxed-> False];




(* ::Text:: *)
(*Using the default viewpoint, we cannot see all surfaces, so we modify the display by showing the visible surfaces of the dodecahedron in color; the invisible ones are not displayed at all.*)


Needs ["Graphics'Colors' \[Bullet]]


 Show [Graphics 3D [ ( * o nly t h e visible f aces * )
                    {EdgeForm[Thickness[0.0001]],
                    {OrangeRed, face[l]}, {LightBlue, face[2]}, {Yellow, face[6]},
                    {Green, face[S]}, {Violet, face[10]}, {Orange, face[l1l}}l,
                    Boxed-> False, Lighting-> False];




(* ::Text:: *)
(*Now, let us deal with the cover of the third edition. It is of course not necessary to first subdivide and deform a face, and then rotate it into the positions of the other surfaces. We could just as well have performed both operations (subdivision and deformation) on every face individually. *)


(* ::Text:: *)
(*This approach leads to the following very short implementation. The subdivision is done iteratively, and the resulting surfaces are displayed with finite thickness. The variables in the formal functions are shortened to $ (a little bit in violation of the general convention). *)
(*  (By increasing the number of iteration steps of Nest, which is currently three, we can get an arbitrarily fine subdivision of the faces of the dodecahedron.)*)


Show[Graphics3D [{EdgeForm[{GrayLevel [0 .25],
                    Thickness[0.001l}l,SurfaceColor[GrayLevel[0.8],
                    RGBColor[1,0.4,1],3l,{Polygon/@#l,Polygon/@#2,
                    MapThread[Polygon[Join[#1,#2]]&,{Reverse/@Flatten[
                    Partition[#,2,1]&/@(Append[#,First[#]]&/@#2),
                    1],Flatten[Partition[#,2,1]&/@(Append[#,First[#]]&/@
                    #1),11}1}& @@({Map[0.92#&,#,{-l}l,#}&[
                    (Function[$,Function[$,#+0.8($-#)]/@$&[Plus@@ $/3]
                    ]/@Map[#((1.07-Sqrt[1.07-(#-0.85065l)A2])&[Sqrt[
                    #.#]])&,Nest[Flatten[Apply[Function[{$1,$2,$3},
                    {{$1,#,$2},{$3,#,$2}}&[$1+(($2-$1).#)#&[#/Sqrt[#.#
                    ]&[($3-$1)]]]],#,{1}l,l]&,Flatten[
                    Function[{$},{{First[#],Plus@@ #/2,Last[$]},
                    {Last[#],Plus@@#/2,Last[$l}}&/@First[$]]/@
                    ({Partition[Append[#,First[#]],2,1],Plus@@
                    #/5}&/@(First/@First[Polyhedron[Dodecahedron]
                    ] ) ) , 2], 3] , { -2}] ) ] ) } ] , Boxed->False] ;


(* ::Text:: *)
(*The last Matherrultica input showed how Matherrultica code should not be formatted; it is rather unreadable in this form. The way this codes works can be understood better by looking at it in Full Form and arranging the brackets carefully. *)


(* ::Text:: *)
(*In the last function, the main work was the recursive subdivision of the right - angled triangles in two right - angled triangles. We extract this part and call it orthogonalSubdi vision.*)


orthogonalSubdivision [1 _, n_] : =
                    Nest[Flatten[Apply[Function[{pl, p2, p3},
                    < {{pl, #, p2}, {p3, #, p2}} &) [
                    (* project *) (pl + (p2 - pl) .# # &) [ (* new point on edge *)
                    (#/Sqrt[#.#] &)[p3 - pl]]]], #1, {1} 1, 1] &,
                    Flatten[(Function[{p}, < {{First[#], Plus\[RegisteredTrademark]\[RegisteredTrademark] #/2, Last[pl},
                    {Last[#], Plus @@ #/2, Last[pl}} &) /\[RegisteredTrademark]
                    First[p]]) /\[RegisteredTrademark] ({(*cyclic*) Partition[Append[#, First[#]], 2, 1],
                    (*center *)Plus @@ #/3} &) /\[RegisteredTrademark] 1, 2], n]


(* ::Text:: *)
(*We demonstrate the resulting triangle structures by starting with an icosahedron, then subdividing its faces into new triangles and finally carry out four times the orthogonal subdivision using orthogonalSubdi vision.*)


 (* split triangle into four triangles *)
                    triangleTo4Triangles[Polygon[{pl_, p2_, p3_} ll :=
                    With[{pl2 = (pl + p2)/2, p23 = (p2 + p3)/2, p31 = (p3 + pl)/2},
                    {Polygon[{pl, pl2, p31}], Polygon[{pl2, p2, p23} l,
                    Polygon[{p23, p3, p3l} l, Polygon[{pl2, p23, p3l} l} l
                    ln[431 := triangleList = Polygon /\[RegisteredTrademark] orthogonalSubdivision [
                    First /\[RegisteredTrademark] Nest[Flatten[triangleTo4Triangles /\[RegisteredTrademark] #] &,
                    First[Polyhedron[Icosahedron]], 2], 4];


(* ::Text:: *)
(*Coloring the resulting triangles according to their longest edge and mapping all polygons to a sphere results in the following picture.*)


( * length of the longest edge of a triangle *)
                    maxEdgeLength[Polygon[{pl_, p2_, p3_} ll :=

                    Sqrt[Max[#.# & /\[RegisteredTrademark] {pl - p2, p2 - p3, p3 - pl} ll
                    (* contract a polygon *)
                    contract[Polygon[{pl_, p2_, p3_} 1, f_l :=
                    Module[{mp = (pl + p2 + p3)/3}, mp + f (# - mp) & /\[RegisteredTrademark] {pl, p2, p3} l;
                    ln[48] := addColor[p : Polygon[{pl_, p2_, p3_} ll :=
                    With[{A = (*large number*) 10 A6 maxEdgeLength[p]},
                    {SurfaceColor[Hue[A], Hue[A], 3],
                    Polygon[#/Sqrt[#.#] & /\[RegisteredTrademark] (contract[p, 0.7]) 1} 1
                    ln[49] := Show[Graphics3D[{EdgeForm[], addColor /\[RegisteredTrademark] triangleList} l, Boxed -> False];


(* ::Text:: *)
(*We could now refine the last cover picture by adding curvature to the individual pieces of the last picture. Here, this is implemented.*)


HyperbolicDodecahedron [
                    n : (faceSubdivision_Integer?Positive),
                    m : (invisibleSubdivisionOfTriangles_Integer?Positive),
                    d : (radialContractionFactor_?(0 < # < 1 &)),
                    e : (1 ateralContractionFactor_?(0 < # < 1 &)),
                    opts ___ ?OptionQ] : \[Bullet]
                    Show[Graphics3D[{BdgeForm[], SurfaceColor[RGBColor[0.7, 0.5, 0.2],
                    RGBColor[O . S , 0.4, 0 . 7], 2] , Thickness[0 . 0001], Function[{t, f}, Function[{l, o},
                    {Polygon/80, Polygon /@ Map[d # &, o, {-2}], Polygon /@ (Join[#[[1]],
                    Reverse[#[[2]]]] & /@ Transpose[{Flatten[Partition[#, 2, 1] & /@ 1, 1],
                    Flatten[Partition[#, 2, 1] &/0 Map[d # &, l, {-2} l, 1 l} Jl,
                    Line /@ l, Line /@ Map[d # &, l , {-2} J, Line /@ Transpose[{#, Map[d # &, #, {-2} 1} & 1
                    Map [f, Flatten [t, 11, { -2} 1 11 } 1 [Map [f , Apply [Function l {p, q, r} ,
                    Function ({a, b, c}, Join[##, {p} 1 & 00 (Apply[(Function ($1, #1 + $1 #21/0
                    Range[O, m - 11) &, {{p, a}, {q, b} , {r, c}}, {1} 1 ) 1 [(q - p)/m, (r - q)/m, (p - r)/m1],
                    t, {1} l, {-2} l , Map[f, Flatten[Apply[Function[{p, q, r}, Function[s,
                    Join[Flattan[Apply[s[[##11 &, Mapindexed[{# , # + {1, 0}, # + {0, 1}} & 1
                    Reverse[#2]] &, Range[m - # + 11 & /@ Range[ml, {2} l, {3} l, 1], F1atten[Apply[
                    s[[##]] &, Mapindexed[{# + {0, 1}, # + {1, 1}, # + {l, O}} &[Reverse[#2]] &,
                    Range [m - #] &/IIRange [m1, {2} 1, {3} 1, 1]] 1 [Function l {a, b},
                    Mapindexed[p + (l2 - {1, 1} l . {a, b} &, Thread/II ({#, Range[O, m - 11} & /@
                    Range [0 , m]) , {2} ]] [ (q - p) /m, (r - p) /mlll , t , {1} 1 , 11 , { -2} l]] [Function[$,
                Function[$, # + e ($ - #)] /@ $ &[Plus @@ $/3]] /@ Nest[Flatten[Apply[
                  Function[{$1, $2, $3}, {{$1, #, $2}, {$3, #, $2}} &[$1 + (($2 - $1) . #) # &[
                    #/Sqrt[# . #1 &[($3 - $1lll11 , #, {1} 1, 11 &, Flatten[Function[{$},
                    {{First[11 , Plus00 #/2, Last[$1}, {Last[#1, Plus00 #/2, Last[$1}} &/0
        First[$11 /@ ({Partition[Append[#, First[#J1, 2, 1], Plus00 #/5} & /@ (First /@
        First[Polyhedron[Dodecahedron] 1)), 2], n1, # ((1.07 - Sqrt[
           1 . 07 - (I - 0 . 850651) A2]) &[Sqrt[#.I]]) & J} 1 , opts,
Boxed -> Falsa, PlotRange -> All, ViewPoint - > {2 , 2, 2} 1;


(* ::Text:: *)
(*By using one - letter variables for the built - in functions that are used more than one time, we can shorten this code, although this does not increase its readability. So the following code is another example of how we should not write and format Mathematica programs.*)


(* ::Text:: *)
(*Now the individual triangular pieces appear more naturally colored.*)


HyperbolicDodecahedron[2, 3, 0.89, 0.72];


(* ::Text:: *)
(*With only a few changes to the above - developed code, we can write a code to display the other hyperbolic*)
(*Platonic bodies (and others). We could, of course, also rewrite the short code.*)


(* ::Item:: *)
(*In general, the first face does not lie parallel to the x, y - plane, and therefore we have to generalize the reflection code.*)


(* ::Item:: *)
(*The center of the polyhedron is not, in general, {0, 0, 0} . Thus, the process of rotation of the individual parts of a face to get the whole face has to be generalized.*)


(* ::Item:: *)
(* The radius of the external sphere that indents the faces has to be defined automatically from the vertices. *)


(* ::Text:: *)
(*Here, we can introduce a free parameter radRed. It defines the ratio of the radius of the indenting sphere to the length of the line segment from the center point of the face to the vertex. In this connection, we now create an exact spherical shell.*)


(* ::Text:: *)
(*Taking account of these generalizations, removing the display of the intermediate pieces, and putting everything  into a Module, we get the following function : HyperbolicPlatonicSolid [ platonicSolid, depth, graphicsOptions].*)


(* ::Text:: *)
(*For the sake of elegance, we make some of the calculations inside the Module more efficient as compared with the above implementation (which was easier to present and discuss).*)


   ln[52] := Needs [ 0 Graphics' Polyhedra' "];
   ln[S3]:= Remove [HyperbolicPlatonicSolid] ;
   HyperbolicPlatonicSolid[
   plato: (Hexahedron I Tetrahedron I Octahedron I Dodecahedron I
   Icosahedron), radRed_?((NumberQ[#] && N[#] > 1) &), opts __ ] :=
   Module[
   {polyhedron, numFaces, numVertices, mpPlato, pa, pb, pc,
   pl, p2, p3, p4, pS, p6, p7, p8, p9, plO, pll, p12, p13, p14,
   from, ac, be, polys, rad, mpSphere, intersectionPoint,
   polys3D, firstPart, reflect, secondPart, part, aMatA, mat, surface},
   (* extract data from polyhedron *)
   polyhedron= Polyhedron[plato] [[1]];
   numFaces = Length[polyhedron];
   numVertices = Length[polyhedron[[l, 1]]];
   mpPlato =Plus\[RegisteredTrademark]\[RegisteredTrademark] Flatten[First /@polyhedron, 1]/numVertices;
   pa Plus\[RegisteredTrademark]\[RegisteredTrademark] polyhedron[[!, 1]]/numvertices;
   pc polyhedron[[!, 1, 1]];
   pb (polyhedron[[!, 1, 2]] +polyhedron[[!, 1, 1]])/2;
   ab pb - pa; ac = pc - pa; be = pc - pb;
   (* form new points *)
   pl pa + ab/4; p2 pa + ab/2; p3 pa + 3ab/4;
   p4 = pa + ac/4; pS = pa + ac/2; p6 pb + bc/2;
   p9 = pa + 3ac/4; p7 = pb + 3bc/4; p8 = p9 + (pc - p9)/2;
   pll p9 + (pb - p9)/2; plO = p9 + (pll - p9)/2;
   p12 = pll + (p6 - pll)/2; pl3 = pll + (pc - pll)/2;
   p14 = pS + (pb - pS)/2;
   ( * the triangulated part *)
			polys = {{pa, pl, p4}, {p4, pS, pl}, {pl, p2, pS},
				{pS, p2, p3}, {pS, p9, pll}, {pb, p6, pll},
				{pB, p9, plO}, {plO, pll, p13}, {pll, p12, p13},
				{p7, p12, p13}, {p6, p7, p12}, {p8, pc, pl3},
				{pc, p7, p13}, {p8, plO, p13}, {p3, pS, pl4},
				{p3, pb, p14}, {pb, pll, p14}, {ps, pll, p14}};
			(* move toward center *)
			rad = N[radRed] Sqrt[#.#]&[(pc- pa)];
			mpSphere = pa + Sqrt[N[radRed]A2 - 1A2] Sqrt[(pc - pa). (pc - pa)] *
				#/Sqrt[#.#]&[pa- mpPlato];
			intersectionPoint[point_, rad_] :=
				Module[{kkq, mppq, kmpq, s},
					kkq = mpSphere.mpSphere;
					mppq = (point- mpPlato). (point- mpPlato);
					kmpq = mpSphere.(point- mpPlato);
					s = kmpq/mppq- Sqrt[(kmpq/mppq)A2 + (radA2- kkq)/mppq];
					s (point- mpPlato)];
			polys3D Map[intersectionPoint[#, rad]&, polys, {2}];
			firstPart = Polygon /@ polys3D;
			reflect[perp_, point_] :=point - 2point.perp/(perp.perp) perp;
			(* double existing piece *)
			8S2 Three-Dimensional Graphics
			secondPart = Polygon /@ Map[reflect[pc - pb, #]&, polys3D, {2}1;
			part[numVertices] = Join[firstPart, secondPart];
			aMatA = Table[a[i, j], {i, 3}, {j, 3}] ;
(* calculate rotation matrices * )
Do[mat[i] \[Bullet] {aMatA /. Solve[
Flatten[Table[Thread[aMatA.polyhedron[[l, 1, j]] == polyhedron[[l, 1,
RotateLeft[Table[~. {~. numVertices}], j] [[i]]]]],
{j, 3}]], Flatten[aMatA]] //. {0.0 _ -> 0.0}
(* " for numerical reasons " * ) ) [ [1]],
{i, numVertices - 1}1 ;
(* rotate pieces inside one face *)
Do[part[i] = Map[{mat[i] .#)&, part[numVertices], {3}],
{i, 1, numVertices- 1}];
face\[Sterling]1] \[Bullet] Flatten[Table[part[i], {i, numVertices}ll;
Do[rot[i] = {aMatA /.
Solve[Flatten[Table[Thread[aMatA.
polyhedron[\[Sterling]1, 1, j]] == polyhedron[[i, 1, j]]],
{j, 3}]], F1atten[aMatA]] //. {0.0 _ -> O.O}J [[1]], {i, numFaces}];
(* generate other faces * )
Do[face[i] = Map[{rot[i].#)&, face[1], {3}], {i, 2, numFaces}J;
Show[Graphics3D[{EdgeForm[Thickness[0.0001]],
Table[face[i], {i, numFaces}J}J, opts, Boxed-> False]]


(* ::Text:: *)
(*Here are the remaining hyperbolic Platonic bodies, each indented differently. (Because of the different forms of the bodies, SphericalRegion - > True does not help to make all bodies of the same size; the bounding box not shown will always fit into the displayed volume.)*)


ln[55]:= Show [ GraphicsArray [
Block\[Sterling]{$DisplayFunction = Identity},
{HyperbolicP1atonicSolid[Tetrahedron, 3],
HyperbolicPlatonicSolid[Octahedron, 2],
HyperbolicPlatonicSolid[Hexahedron, 2.1],
HyperbolicPlatonicSolid[Icosahedron, 1.6]}]]];


(* ::Text:: *)
(*We can also adapt the above given short implementation to the other Platonic solids. This results in the following code.*)


Needs [\[Bullet]Graphics'Polyhedra'\[Bullet]]
Clear[HyperbolicPlato];
HyperbolicPlato[plato: {Tetrahedron I Hexahedron I Octahedron
Dodecahedron I Icosahedron),
x: {radia1ContractionExponent_?{Im[#]a=0&)),
n: {faceSubdivision_Integer?Positive),
m:{invisibleSubdivisionOfTriangles_ Integer?Positive),
d:{radialContractionFactor_ ?{O<#<l&)),
e: {lateralContractionFactor_?{0<#<1&)),
h: (hueCo1orSpecification_?(0<=#<\[Bullet]1&)),
opts ___ ?OptionQ] :=
Show[Graphics3D[{EdgeFo~[],SurfaceColor[Hue[h],Hue[h],2.2],
Thickness[O.OOl],Function[{t,f},Function[{l,o},
{Polygon/eo,Polygon/OMap[di&,o,{-2}],Polygon/O(Join[#[[l]],
Reverse[#[[2]]]]&/eTranspose[{Flatten[Partition[#,2,1]&/0l, l],
Flatten[Partition[i,2,1]&/0Map[d #&,l,{-2}l,ll}l),
Line/Ol,Line/0Map[di&,l,{-2}],Line/\[RegisteredTrademark]Transpose[{#,Map[di&,#,{-2}l}&[
Map [f ,Flatten [t, 1], { -2}111 }l [Map [f,Apply [Function l{p, q, r},
Function[{a,b,c},Join[##,{p}l&O\[RegisteredTrademark](Apply[(Function[$l,il+$1 #2]/\[RegisteredTrademark]
Range [0 ,m-1]) &, { {p, a}, {q,b}, {r, c}}, {1}])] [ (q-p) /m, (r-q) /m, (p-r) /mll,
t,{l}l,{-2}],Map[f,Flatten[Apply[Function[{p,q,r},Function[s,
Join[Flatten[Apply[s[(ii]]&,Mapindexed[{i,#+{1,0},i+{0,1}}&[
Reversa[i2]]&,Range[m-i+1]&/0Ranga[m],{2}l,{3}],1],
Flatten[Apply[s[[##]]&,Mapindexed[{#+{0,1},#+{1,1},i+{l,O}}&[
Reverse[i2]]&,Range[m-#]&/\[RegisteredTrademark]Range[ml,{2}l,{3}l,1lll [Function[{a,b},
Mapindexed[p+(#2-{1,1}).{a,b}&,Thread/\[RegisteredTrademark]({#,Range[O,m-il}&/\[RegisteredTrademark]
Range [O,m]), {2}11 [ (q-p) /m, (r-p) /m]]) ,t,{1}1 ,1] ,{ -2}111 [Function[$,
Function[$,#+e($-#)]/@$&[Plus0\[RegisteredTrademark]$/3]]/@
Nest[Flatten[Apply[Function[{$1,$2 , $3},{{$1,#,$2},{$3,#,$2}}&[
$1+(($2-$1).#)#&[#/Sqrt[#.#]&[($3-$1)]]]],i,{1}],1]&,F1atten[
Function[{$},{{First[#],Plus0\[RegisteredTrademark] #/2 , Last[$J},{Last[#],Plus\[RegisteredTrademark]\[RegisteredTrademark] #/2,
Last[$J}}&/@First[$]]/O({Partition[Append[#,First[i]],2,1],
Plus\[RegisteredTrademark]\[RegisteredTrademark]i/Length[#J}&/O(First/\[RegisteredTrademark]First[Map[#/Sqrt[#.#]&,Take[
Polyhedron[plato],1],{-2}]])),2],n],i/(#.#)Ax&l}J,opts,
Boxed->False,PlotRange->All,SphericalRegion->True];


(* ::Text:: *)
(*The last HyperbolicPlato code also allows us to view the transition between a " hyperbolic " and a " parabolic " Platonic solid.*)


(CellPrint[Cell[" o \[Bullet] <> ToString[#] <> "transition:", "Text\[Bullet]]];
Show[GraphicsArray[ill& /@Partition[
Table[HyperbolicPlato[#, x, 2, 3, 0.91, 0.74,
(* better contrast for printed version *)
If[Options[Graphics3D, ColorOutput] [[1, 2]] z\[Bullet]\[Bullet] GrayLavel,
0.25 + 0.15 x, (x + 1)/2 0.8],
DisplayFunction ->Identity], {x, -1, 1 , 2/8}1, 3])& /0
{Tetrahedron, Hexahedron, Octahedron, Dodecahedron, Icosahedron};


(* ::Text:: *)
(*  o Tetrahedron transition: *)
