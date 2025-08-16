(* ::Package:: *)

(* Kernels.wl — selection/learning kernels on policy sets *)

BeginPackage["KI`Kernels`"]

ReplicatorKernel::usage = "ReplicatorKernel[pols, fitness, η, τ] returns a function that maps a population distribution to the next one.";
MutatePolicies::usage = "MutatePolicies[pols, η, policyFamily] returns a mutated policy set.";

Begin["`Private`"]

softmax[v_List, τ_?NumericQ] := Module[{mx = Max@v}, With[{s = Exp[(v - mx)/Max[τ, 10^-8]]}, s/Total@s]]

ReplicatorKernel[pols_List, fitnessFun_, η_:0.02, τ_:0.2] := Module[
  {m = Length@pols},
  Function[{dist},
    Module[{fit = fitnessFun /@ pols, sel, mut},
      sel = softmax[fit, τ] * dist; sel = sel/Total@sel;
      mut = (1 - η) sel + η ConstantArray[1./m, m];
      mut/Total@mut
    ]
  ]
]

MutatePolicies[pols_List, η_:0.02, family_String:"Generic"] := Module[
  {noise = RandomVariate[NormalDistribution[0, η], {Length@pols}]},
  Switch[family,
    "Threshold",
      pols /. Association[p___, "Type" -> "Threshold", "Params" -> params_] :>
        Association[p, "Params" -> ReplacePart[params, "θ" -> (params["θ"] + RandomVariate[NormalDistribution[0, η]])]],
    _, pols
  ]
]

End[]
EndPackage[]