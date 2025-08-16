(* ::Package:: *)

(* DriftEngine.wl — continuity checks and bifurcation alarms *)

BeginPackage["KI`DriftEngine`", {"KI`Metrics`"}]

ContinuityResidual::usage = "ContinuityResidual[μE, μEΔ, L0, λ, Δ] returns residual = W1(μE, μEΔ) - (L0/(1-λ))||Δ||.";
RollingAlarm::usage = "RollingAlarm[residuals, thresh] returns True when trend breaches threshold.";

Begin["`Private`"]

(* Represent discrete measures as vectors on same support; W1 is L1/2 here as a proxy *)

w1Proxy[p_List, q_List] := 0.5*Total@Abs[p - q]

ContinuityResidual[μE_List, μEΔ_List, L0_?NumericQ, λ_?NumericQ, Δ_?NumericQ] :=
  w1Proxy[μE, μEΔ] - (L0/(1 - λ)) * Abs[Δ]

RollingAlarm[residuals_List, thresh_:0.] := Module[
  {trend = MovingAverage[residuals, 5]},
  AnyTrue[trend, # > thresh &]
]

End[]
EndPackage[]