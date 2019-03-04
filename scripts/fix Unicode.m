(* ::Package:: *)

Begin["Trapezohedron`"]
$unicode = Block[
	{path, line},
	path = FrontEnd`FindFileOnPath["UnicodeCharacters.tr", "PrivatePathsTextResources"] // FrontEndExecute;
	line = Rest[StringSplit@StringSplit[Import[path, "String"], "\n"]];
	#[[2]] -> ToExpression["\"" <> StringReplace[#[[1]], {"x" -> ":", StartOfString ~~ "0" -> "\\"}] <> "\""]& /@ line
];
fixUnicode[path_] := Block[
	{text},
	text = StringReplace[Import["source.m", "Text"], $unicode];
	Export["source.m", text, "Text", CharacterEncoding -> "UTF8"];
];
End[]
