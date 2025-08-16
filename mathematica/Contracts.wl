(* ::Package:: *)

(* Contracts.wl — compile-time-ish checks on decision kernels *)

BeginPackage["KI`Contracts`", {"KI`Groupoid`"}]

CheckMeasurable::usage = "CheckMeasurable[π] tries numeric continuity on sample points.";
CheckSupport::usage = "CheckSupport[π, nAct] ensures probability vector length nAct and sums to 1 within tolerance.";
CheckGaugeEquivariance::usage = "CheckGaugeEquivariance[π, t, obs] checks invariance under logit gauge mapping.";

Begin["`Private`"]

CheckMeasurable[π_, sampler_, n_Integer:64] := Module[
  {obs = sampler /@ Range[n], vals},
  Quiet@Check[
    vals = (If[Head[π] === Association, π["Apply"][#], π[#]] & /@ obs);
    VectorQ[Flatten@vals, NumericQ],
    False
  ]
]

CheckSupport[π_, sampler_, nAct_Integer, n_Integer:64, tol_:1.*^-6] := Module[
  {obs = sampler /@ Range[n], ok},
  ok = And @@ Table[
     With[{p = If[Head[π] === Association, π["Apply"][obs[[i]]], π[obs[[i]]]]},
       Length[p] == nAct && Abs[Total[p] - 1.] <= tol && Min[p] >= -tol
     ],
     {i, n}
  ];
  ok
]

CheckGaugeEquivariance[π_, t_, sampler_, n_Integer:32, tol_:5.*^-3] := Module[
  {obs = sampler /@ Range[n], lhs, rhs},
  lhs = If[Head[π] === Association, π["Apply"] /@ obs, π /@ obs];
  rhs = (ApplyTransform[t, π, #] & /@ obs);
  Mean@MapThread[TotalVariation, {lhs, rhs}] <= tol
]

End[]
EndPackage[]