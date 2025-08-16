(* ::Package:: *)

(* Metrics.wl — behavioral and Wasserstein-ish distances *)

BeginPackage["KI`Metrics`", {"KI`Groupoid`"}]

BehavioralDistance::usage = "BehavioralDistance[π1, π2, obsSampler, n] ≈ E TV(π1(.|h), π2(.|h)).";
BarD::usage = "BarD[π1, π2, candidates] = inf over transforms of BehavioralDistance.";
TotalVariation::usage = "TotalVariation[p, q] returns 0.5 * L1 distance.";

Begin["`Private`"]

TotalVariation[p_List, q_List] := 0.5*Total@Abs[p - q]

BehavioralDistance[π1_, π2_, obsSampler_, n_Integer: 256] := Module[
  {obs = obsSampler /@ Range[n], d},
  d = Mean@Table[
     With[{p = If[Head[π1] === Association, π1["Apply"][obs[[i]]], π1[obs[[i]]]],
           q = If[Head[π2] === Association, π2["Apply"][obs[[i]]], π2[obs[[i]]]]},
       TotalVariation[p, q]
     ]
    , {i, n}];
  d
]

(* Search over a small finite set of plausible arrows *)
BarD[π1_, π2_, transformCandidates_List, obsSampler_, n_Integer: 128] := Module[
  {vals},
  vals = Table[
     BehavioralDistance[
       Function[o, ApplyTransform[t1, π1, o]],
       Function[o, ApplyTransform[t2, π2, o]],
       obsSampler, n
     ],
     {t1, transformCandidates}, {t2, transformCandidates}
  ];
  Min@Flatten@vals
]

End[]
EndPackage[]