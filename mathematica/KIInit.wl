(* ::Package:: *)

(* KIInit.wl — central loader for the KI Mathematica stack *)

BeginPackage["KI`"]

KI`Load::usage = "KI`Load[] loads all KI packages in dependency order."

Begin["`Private`"]

SetDirectory@NotebookDirectory[] /; $Notebooks;

Clear[KI`Load]
KI`Load[] := Module[{},
  Get["KI`Groupoid`"];
  Get["KI`Metrics`"];
  Get["KI`Morphospace`"];
  Get["KI`Canonicalize`"];
  Get["KI`Kernels`"];
  Get["KI`FIcurvature`"];
  Get["KI`FlowCentrality`"];
  Get["KI`DriftEngine`"];
  Get["KI`Contracts`"];
  Get["KI`Experiments`"];
  Get["KI`PhaseMap`"];
  Get["KI`Visualization`"];
]

End[]
EndPackage[]