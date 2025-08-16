(* ::Package:: *)

(* Groupoid.wl — transform groupoid + logit gauge *)

BeginPackage["KI`Groupoid`"]

ApplyTransform::usage = "ApplyTransform[t, policy, obs] applies groupoid arrow t to policy.";
ComposeTransform::usage = "ComposeTransform[t2, t1] composes two arrows when domains/codomains match.";
IdentityTransform::usage = "IdentityTransform[] returns the identity arrow.";
LogitGaugeMap::usage = "LogitGaugeMap[{a_, b_}, τ] returns the rescaled inverse temperature a τ.";

Begin["`Private`"]

(* Representation:
   t is an Association with keys:
   <| \"Th\" -> history map function, \"PiA\" -> action permutation (list of indices),
      \"Gauge\" -> {a,b} with a>0, \"Coarse\" -> coarse-graining function |>
*)

IdentityTransform[] := <|"Th" -> Function[h, h], "PiA" -> None, "Gauge" -> {1., 0.}, "Coarse" -> Function[o, o]|>

ComposeTransform[t2_, t1_] := Module[{th, pia, gau, crs},
  th = Composition[t2["Th"], t1["Th"]];
  pia = Which[
    t2["PiA"] === None, t1["PiA"],
    t1["PiA"] === None, t2["PiA"],
    True, t2["PiA"][[t1["PiA"]]]
  ];
  gau = {t2["Gauge"][[1]] t1["Gauge"][[1]], t2["Gauge"][[1]] t1["Gauge"][[2]] + t2["Gauge"][[2]]};
  crs = Composition[t2["Coarse"], t1["Coarse"]];
  <|"Th" -> th, "PiA" -> pia, "Gauge" -> gau, "Coarse" -> crs|>
]

LogitGaugeMap[{a_?Positive, b_}, τ_?NumericQ] := a τ

(* Policies are represented as pure functions or associations:
   policy[obs_] -> ProbabilityVector over actions
   If an association is used, expect keys: \"Type\", \"Apply\", \"Params\" *)

permuteProb[p_List, None] := p
permuteProb[p_List, perm_List] := p[[perm]]

ApplyTransform[t_, policy_, obs_] := Module[
  {h2, o2, p2, aperm},
  h2 = t["Th"][obs];
  o2 = t["Coarse"][h2];
  p2 = Which[
    Head[policy] === Association, policy["Apply"][o2],
    True, policy[o2]
  ];
  aperm = t["PiA"];
  permuteProb[p2, aperm]
]

End[]
EndPackage[]