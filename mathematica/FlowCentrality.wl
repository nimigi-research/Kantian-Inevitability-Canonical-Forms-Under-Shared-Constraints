(* ::Package:: *)

(* FlowCentrality.wl — probability-flow graph metrics *)

BeginPackage["KI`FlowCentrality`"]

FlowGraph::usage = "FlowGraph[trajectories, k] builds a graph over k clusters with edge weights from transitions.";
EdgeCentralityScores::usage = "EdgeCentralityScores[g] returns current-flow or betweenness centrality.";

Begin["`Private`"]

FlowGraph[trajectories_List, k_Integer] := Module[
  {trans = ConstantArray[0., {k, k}]},
  Scan[
    (Scan[(If[1 <= #[[1]] <= k && 1 <= #[[2]] <= k, trans[[#[[1]], #[[2]]]] += 1.]) &, Partition[#, 2, 1]]) &,
    trajectories
  ];
  WeightedAdjacencyGraph[trans, VertexLabels -> "Name", EdgeWeight -> Automatic]
]

EdgeCentralityScores[g_Graph] := AssociationThread[EdgeList[g] -> EdgeBetweennessCentrality[g]]

End[]
EndPackage[]