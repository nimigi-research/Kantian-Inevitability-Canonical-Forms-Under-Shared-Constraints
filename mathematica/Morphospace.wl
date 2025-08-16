(* ::Package:: *)

(* Morphospace.wl — equivalence, distance matrices, clustering helpers *)

BeginPackage["KI`Morphospace`", {"KI`Groupoid`", "KI`Metrics`"}]

EquivalentQ::usage = "EquivalentQ[π1, π2, transforms, obsSampler] tests if inf distance ~ 0.";
DistanceMatrixOnPolicies::usage = "DistanceMatrixOnPolicies[Π, transforms, obsSampler, n] returns symmetric matrix.";
QuotientCluster::usage = "QuotientCluster[dmat, k] clusters policies in morphospace.";

Begin["`Private`"]

EquivalentQ[π1_, π2_, transforms_List, obsSampler_, n_Integer:128, eps_:1.*^-3] :=
  BarD[π1, π2, transforms, obsSampler, n] <= eps

DistanceMatrixOnPolicies[pols_List, transforms_List, obsSampler_, n_Integer:128] := Module[
  {m = Length@pols, dmat = ConstantArray[0., {Length@pols, Length@pols}]},
  Do[
    If[j > i,
      dmat[[i, j]] = dmat[[j, i]] = BarD[pols[[i]], pols[[j]], transforms, obsSampler, n]
    ],
    {i, m}, {j, m}
  ];
  dmat
]

QuotientCluster[dmat_?MatrixQ, k_Integer:Automatic] := Module[
  {g, w, cl},
  w = Max[dmat] - dmat; w = w - DiagonalMatrix[Diagonal[w]];
  g = WeightedAdjacencyGraph[w, VertexLabels -> "Name"];
  cl = If[k === Automatic, FindGraphCommunities[g], FindGraphCommunities[g, k]];
  cl
]

End[]
EndPackage[]