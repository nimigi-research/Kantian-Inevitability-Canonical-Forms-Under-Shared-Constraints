(* ::Package:: *)

(* Canonicalize.wl — from policies to canonical clusters with alignment maps *)

BeginPackage["KI`Canonicalize`", {"KI`Groupoid`", "KI`Morphospace`", "KI`Metrics`"}]

CanonicalizePolicies::usage = "CanonicalizePolicies[pols, transforms, obsSampler] -> <|Clusters, Representatives, Maps|>.";

Begin["`Private`"]

CanonicalizePolicies[pols_List, transforms_List, obsSampler_, n_Integer:128] := Module[
  {dmat, comms, reps, maps},
  dmat = DistanceMatrixOnPolicies[pols, transforms, obsSampler, n];
  comms = QuotientCluster[dmat];
  reps = pols[[First /@ (First /@ comms /. VertexList[WeightedAdjacencyGraph[Max[dmat] - dmat]] -> Range[Length@pols])]];
  maps = AssociationThread[Range[Length@pols] -> ConstantArray[IdentityTransform[], Length@pols]];
  <|"Clusters" -> comms, "Representatives" -> reps, "Maps" -> maps, "DistanceMatrix" -> dmat|>
]

End[]
EndPackage[]