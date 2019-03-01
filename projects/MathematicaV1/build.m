(* ::Package:: *)

faces = PolyhedronData["MathematicaPolyhedron", "Faces", "Polygon"];
Export["Object.mx", Flatten[faces], "WXF", PerformanceGoal -> "Size"]
