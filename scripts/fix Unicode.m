(* ::Package:: *)
Begin["Trapezohedron`"]
$unicodeTR = FrontEnd`FindFileOnPath["UnicodeCharacters.tr", "PrivatePathsTextResources"] // FrontEndExecute;
$unicodeReplacements = #[[2]] -> ToExpression["\"" <> StringReplace[#[[1]], {"x" -> ":", StartOfString ~~ "0" -> "\\"}] <> "\""]& /@ Rest[StringSplit@StringSplit[Import[$unicodeTR, "String"], "\n"]];
fixUnicode[path_] := Block[
	{text},
	text = StringReplace[Import["source.m", "Text"], $unicodeReplacements];
	Export["source.m", text, "Text", CharacterEncoding -> "UTF8"];
];
End[]