(* ::Package:: *)

(* Visualization.wl — simple plotting helpers *)

BeginPackage["KI`Visualization`"]

PhaseMapPlot::usage = "PhaseMapPlot[pm] draws concentration and participation vs grid parameter.";

Begin["`Private`"]

PhaseMapPlot[pm_Association] := Module[
  {xs = Keys@pm["Results"], conc, pr},
  conc = Values@pm["Results"][All, "Concentration"];
  pr = Values@pm["Results"][All, "Participation"];
  Grid[{
    {ListLinePlot[Thread[{Range[Length@xs], conc}], Frame -> True, PlotLegends -> {"Concentration"}]},
    {ListLinePlot[Thread[{Range[Length@xs], pr}], Frame -> True, PlotLegends -> {"Participation"}]}
  }]
]

End[]
EndPackage[]