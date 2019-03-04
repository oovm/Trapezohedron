(* ::Package:: *)

(* ::Title:: *)
(*The Cover Image *)


(* ::Subtitle:: *)
(*Making the Escher-style hyperbolic dodecahedron symbolizing Mathematica 5*)
(**)


(* ::Author:: *)
(*Michael Trott*)
(*Copyright Michael Trott 2003*)


(* ::Section:: *)
(*Outline of the Procedure*)


(* ::Text:: *)
(*Step 1:*)
(*We construct a regular dodecahedron centered at the origin.*)


(* ::Text:: *)
(*Step 2:*)
(*The faces of the dodecahedron are then subdivided into right triangles (similar to M.\[NonBreakingSpace]C.\[NonBreakingSpace]Escher's Quadratlimit work), each of which is then scaled down so that gaps are introduced between them.*)


(* ::Text:: *)
(*Step 3:*)
(*Each triangle is further subdivided so that, when the surfaces are transformed in the next step, the resulting surfaces are relatively smooth.*)


(* ::Text:: *)
(*Step 4:*)
(*Outlines of all triangles are constructed.*)


(* ::Text:: *)
(*Step 5:*)
(*A radial transformation is applied to each triangle. Then each triangle is given depth by connecting it to a slightly smaller copy moved closer to the origin.*)


(* ::Section:: *)
(*Step 1: The Regular Dodecahedron*)


(* ::Commentary:: *)
(*This loads the standard package that contains primitives for rendering the Platonic solids.*)


Needs["Graphics`Polyhedra`"]


(* ::Commentary:: *)
(*Here is a regular dodecahedron.*)


Show[Polyhedron[Dodecahedron]];


(* ::Section:: *)
(*Step 2: Dividing into Triangles*)


(* ::Commentary:: *)
(*The function SolidToTriangles breaks the faces of a Platonic solid into right triangles.*)


SolidToTriangles[solid_]:=With[{f=Length[Faces[solid][[1]]]},Flatten[Function[l,({Polygon[{#[[1]],#[[2]],Plus@@l[[1]]/f}]}&)/@Partition[Append[l[[1]],First[l[[1]]]],2,1]]/@Polyhedron[solid][[1]]]];


(* ::Commentary:: *)
(*Applied to the dodecahedron, here is the result.*)


Show[Graphics3D[Subscript[\[ScriptH], 1]=SolidToTriangles[Dodecahedron]]];


(* ::Section:: *)
(*Step 3: Subdividing the Triangles*)


(* ::Commentary:: *)
(*Here is a list of triangles, that subdivide a triangle in an Escher-style way.*)


TriangulatedTriangle=Polygon[Append[#,0]& /@ #]&/@({{{0,0},{-2,-2},{0,-4}},{{0,-4},{-4,-4},{-2,-2}},{{0,0},{0,-4},{2,-2}},{{0,-4},{2,-2},{4,-4}},{{-4,-4},{-5,-5},{-4,-6}},{{-4,-6},{-6,-6},{-5,-5}},{{0,-4},{-2,-4},{-2,-6}},{{0,-4},{-1,-5},{0,-6}},{{0,-6},{-2,-6},{-1,-5}},{{-4,-4},{-2,-4},{-2,-6}},{{-4,-4},{-3,-5},{-4,-6}},{{-4,-6},{-2,-6},{-3,-5}},{{4,-4},{2,-4},{2,-6}},{{4,-4},{3,-5},{4,-6}},{{4,-6},{2,-6},{3,-5}},{{0,-4},{2,-4},{2,-6}},{{0,-4},{1,-5},{0,-6}},{{0,-6},{2,-6},{1,-5}},{{4,-4},{4,-6},{5,-5}},{{4,-6},{5,-5},{6,-6}}}/6);


Show[Graphics3D[TriangulatedTriangle]];


(* ::Commentary:: *)
(*The function MapTo3DTriangle maps this triangulation onto all triangles of the above triangulation of the dodecahedron.*)


MapTo3DTriangle[Polygon[{p1_,p2_,p3_}]]:=Module[{mp=(p1+p2)/2,dirx,diry},diry=p3-mp;dirx=p1-mp;Map[p3+#[[1]] dirx+#[[2]] diry&,N[TriangulatedTriangle],{-2}]]


Show[Graphics3D[Subscript[\[ScriptH], 2]=Polygon/@(N[First/@Flatten[MapTo3DTriangle/@Take[Subscript[\[ScriptH], 1],All]]])],PlotRange->All,Axes->False];


(* ::Section:: *)
(*Steps 4: Triangle Edge Construction*)


(* ::Commentary:: *)
(*The function TriangleBorders constructs the outlines of all triangles.*)


TriangleBorders[{plato_, lateralContractionFactor_,{\[Delta]ppl_,ppq_}},Polygon[{p1_,p2_,p3_}]]:=
Module[{mp= (p1+p2+p3)/3,p1i,p2i,p3i,ppl},{p1i,p2i,p3i}=(mp+lateralContractionFactor (#-mp)&)/@{p1,p2,p3};
ppl=Ceiling[1/\[Delta]ppl Max[Sqrt[#.#]&/@Apply[Subtract,Partition[{p1i,p2i,p3i,p1i},2,1],{1}]]];
{Apply[Function[{q1,q1i,q2i,q2},Function[pn,Table[Polygon[{pn[[i,j]],pn[[i+1,j]],pn[[i+1,j+1]],pn[[i,j+1]]}],{i,ppl},{j,ppq}]][Table[q1+(ix (q2-q1))/ppl+((1-ix/ppl) iy (q1i-q1))/ppq+(ix iy (q2i-q2))/(ppl ppq),{ix,0,ppl},{iy,0,ppq}]]],{{p1,p1i,p2i,p2},{p2,p2i,p3i,p3},{p3,p3i,p1i,p1}},{1}],Line[Join@@(Table[#[[1]]+(i (#[[2]]-#[[1]]))/ppl,{i,0,ppl}]&)/@{{p1i,p2i},{p2i,p3i},{p3i,p1i}}]}]


Show[Graphics3D[{EdgeForm[], Subscript[\[ScriptH], 3]=Transpose[TriangleBorders[{Dodecahedron,0.8,{0.08,1}},#]&/@Subscript[\[ScriptH], 2]]}]];


(* ::Section:: *)
(*Steps 5: Radial Transformation and Thickening*)


(* ::Commentary:: *)
(*The next task is to create a function for calculating the radial transformation.*)


attract[r_]:=Re[1/2 (ArcSin[2 r-1]+\[Pi]/2)];
makeContractFunction[]:=( 
contract[x_]=Module[{xMin,xMax,xm,dx},{xMax,xMin}=Sqrt[({Max[#],Min[#]}&)[(#.#&)/@Level[Subscript[\[ScriptH], 2],{-2}]]];attract[(x-xMin)/(xMax-xMin)]]);
makeContractFunction[];
contract[x_List]:=(x contract[Sqrt[x.x]])/Sqrt[x.x];


(* ::Commentary:: *)
(*The following is a plot of the function contract.*)


Plot[contract[x],{x,0.7,1.2},Frame->True,Axes->False,PlotRange->All];


(* ::Commentary:: *)
(*Applying contract to Subscript[\[ScriptH], 2] gives the following hyperbolic dodecahedron.*)


Show[Graphics3D[Map[contract,Subscript[\[ScriptH], 2],{-2}]],PlotRange->All,Axes->False];


(* ::Commentary:: *)
(*The function Hyperbolicize takes the triangle outlines in Subscript[\[ScriptH], 3] and applies the function contract. In addition, it does this with a smaller copy of Subscript[\[ScriptH], 3] and connects the edges of the resulting two hyperbolic dodecahedra.*)


Hyperbolicize[f_,{polys_,edges_},radialContractionFactor_]:=Module[{l=First/@Map[f,edges,{-2}],p=First/@Map[f,Flatten[polys],{-2}],outerPolys,innerPolys,radialPolys,outerEdges,innerEdges},outerPolys=Polygon/@p;innerPolys=Polygon/@Map[radialContractionFactor #1&,p,{-2}];radialPolys=Polygon/@(Join[#[[1]],Reverse[#[[2]]]]&)/@Transpose[{Flatten[(Partition[#,2,1]&)/@l,1],Flatten[(Partition[#1,2,1]&)/@Map[radialContractionFactor #1&,l,{-2}],1]}];{outerPolys,innerPolys,radialPolys}];


(* ::Commentary:: *)
(*This generates the final cover image. The edges of the triangles in Subscript[\[ScriptH], 3] are thickened and contracted.*)


Subscript[\[ScriptH], 4]=Hyperbolicize[contract,Subscript[\[ScriptH], 3],0.9];


CoverGraphicsMathematicaV5=Show[Graphics3D[{EdgeForm[],SurfaceColor[Hue[0.08],Hue[0.12],2.4],Subscript[\[ScriptH], 4]}],PlotRange->All,Boxed->False];


(* ::Section:: *)
(*A Variant*)


(* ::Commentary:: *)
(*The above implementation can be used with other Platonic solids, radial transformations, and coloring schemes. Here it is done with a cube (hexahedron).*)


Subscript[\[ScriptH], 1]=SolidToTriangles[Hexahedron];
Subscript[\[ScriptH], 2]=Polygon/@(N[First/@Flatten[MapTo3DTriangle/@Take[Subscript[\[ScriptH], 1],All]]]);
 Subscript[\[ScriptH], 3]=Transpose[TriangleBorders[{Hexahedron,0.8,{0.08,1}},#]&/@Subscript[\[ScriptH], 2]];
makeContractFunction[];
Subscript[\[ScriptH], 4]=Hyperbolicize[contract,Subscript[\[ScriptH], 3],0.9];


rainbowColor[p:Polygon[l_]]:={(SurfaceColor[#,#,2.2]&)[Hue[2Sqrt[#.#]&[Plus@@l/Length[l]]/\[Pi]]],p}


Show[Graphics3D[{EdgeForm[],rainbowColor/@Flatten[Subscript[\[ScriptH], 4]]}],PlotRange->All,Boxed->False];