(* ::Package:: *)

(* ::Subchapter:: *)
(*Program I*)


upper = With[
	{
		w = 3 * Sqrt[1 - (x / 7)^2], l = (6 / 7) * Sqrt[10] + (3 + x) / 2 - (3 / 7) * Sqrt[10] * Sqrt[4 - (x + 1)^2],
		h = (1 / 2) * (3 * (Abs[x - 1 / 2] + Abs[x + 1 / 2] + 6) - 11 * (Abs[x - 3 / 4] + Abs[x + 3 / 4])),
		r = (6 / 7) * Sqrt[10] + (3 - x) / 2 - (3 / 7) * Sqrt[10] * Sqrt[4 - (x - 1)^2]
	},
	w + (l - w) * UnitStep[x + 3] + (h - l) * UnitStep[x + 1] + (r - h) * UnitStep[x - 1] + (w - r) * UnitStep[x - 3]
];
lower = With[
	{
		a = 3 * Sqrt[1 - (x / 7)^2] + Sqrt[1 - (Abs[Abs[x] - 2] - 1)^2] + Abs[x / 2] - ((3 * Sqrt[33] - 7) / 112) * x^2 - 3,
		b = (x + 4) / Abs[x + 4] - (x - 4) / Abs[x - 4]
	},
	a * b / 2 - 3 * Sqrt[1 - (x / 7)^2]
];
Plot[
	{upper, lower},
	{x, -7, 7}, AspectRatio -> Automatic
]


(* ::Subchapter:: *)
(*Program II*)


plots = {
	ContourPlot[((x / 7)^2 + (y / 3)^2 - 1) == 0, {x, -8, 8}, {y, -5, 5}, RegionFunction -> ((Abs[#1] > 3 && #2 > -(3 Sqrt[33]) / 7)&)],
	ContourPlot[(Abs[x / 2] - ((3 Sqrt[33] - 7) / 112) x^2 - 3 + Sqrt[1 - (Abs[Abs[x] - 2] - 1)^2] - y) == 0, {x, -7, 7}, {y, -3, 3}],
	ContourPlot[(9 - 8 Abs[x] - y) == 0, {x, -7, 7}, {y, -3, 3}, RegionFunction -> ((3 / 4 < Abs[#] < 1)&)],
	ContourPlot[(3 Abs[x] + 3 / 4 - y) == 0, {x, -7, 7}, {y, -3, 3}, RegionFunction -> ((1 / 2 < Abs[#1] < 3 / 4)&)],
	ContourPlot[(9 / 4 - y) == 0, {x, -7, 7}, {y, -3, 3}, RegionFunction -> ((Abs[#1] < 1 / 2)&)],
	ContourPlot[((6 Sqrt[10]) / 7 + (3 / 2 - Abs[x] / 2) - (6 Sqrt[10]) / 14 Sqrt[4 - (Abs[x] - 1)^2] - y) == 0, {x, -7, 7}, {y, -3, 3}, RegionFunction -> ((Abs[#1] > 1)&)]
};
Show[plots, AspectRatio -> Automatic, PlotRange -> {{-7, 7}, {-3, 3}}]
