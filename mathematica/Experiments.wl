(* ::Package:: *)

(* Experiments.wl — task generators T1–T4 and simple policy families *)

BeginPackage["KI`Experiments`", {"KI`Groupoid`"}]

GenNoisyMajority::usage = "GenNoisyMajority[σ] returns <|ObsSampler, Reward, NActions|> with majority classification.";
GenSparseValueBandit::usage = "GenSparseValueBandit[b] returns a K-armed bandit with b active high-value arms.";
GenZeroSumGrid::usage = "GenZeroSumGrid[α] returns tiny adversarial matrix game parameterized by α.";
GenAliasPOMDP::usage = "GenAliasPOMDP[alias] returns a POMDP with controllable aliasing.";

MakePolicyFamily::usage = "MakePolicyFamily[type, params] returns an association policy with Apply and optional Grad.";

Begin["`Private`"]

(* T1: Noisy majority of 5 bits with Gaussian channel *)
GenNoisyMajority[σ_?NumericQ] := Module[
  {pObs, nAct = 2},
  pObs[] := Module[{x = RandomInteger[{0,1}, 5], y},
    y = If[Total[x] >= 3, 1, 0];
    <|"x" -> x + RandomVariate[NormalDistribution[0, σ], 5], "y" -> y|>
  ];
  <|
    "ObsSampler" -> (Function[{}, pObs[]]),
    "Reward" -> (Function[{act, obs}, If[act == obs["y"] + 1, 1., 0.]]),
    "NActions" -> nAct
  |>
]

(* T2: Sparse-value bandit with K=10 arms and b high-value arms *)
GenSparseValueBandit[b_Integer?Positive] := Module[
  {K = 10, vals, nAct},
  vals = ConstantArray[0., K];
  Scan[(vals[[#]] = 1.) &, RandomSample[Range[K], Min[b, K]]];
  nAct = K;
  <|
    "ObsSampler" -> (Function[{}, None]),
    "Reward" -> (Function[{act, _}, vals[[act]] + RandomVariate[NormalDistribution[0, 0.05]]]),
    "NActions" -> nAct
  |>
]

(* T3: Tiny 3x3 zero-sum with α controls payoff contrast *)
GenZeroSumGrid[α_?NumericQ] := Module[
  {A = {{0, -1, 1}, {1, 0, -1}, {-1, 1, 0}}},
  <|
    "ObsSampler" -> (Function[{}, None]),
    "Reward" -> (Function[{act, _}, α*(A[[act, RandomInteger[{1,3}]]])]),
    "NActions" -> 3
  |>
]

(* T4: Alias POMDP: two states alias to same observation with prob alias *)
GenAliasPOMDP[alias_?NumericQ] := Module[
  {nAct = 2},
  <|
    "ObsSampler" -> (Function[{},
      With[{z = RandomInteger[{0,1}]},
        <|"zTrue" -> z, "zObs" -> If[RandomReal[] < alias, 1 - z, z]|>
      ]
    ]),
    "Reward" -> (Function[{act, obs}, If[act == obs["zTrue"] + 1, 1., 0.]]),
    "NActions" -> nAct
  |>
]

(* Policy families *)
MakePolicyFamily["Threshold", params_Association] := Module[
  {θ = params["θ"] /. Missing[_, _] -> 0.},
  Association[
    "Type" -> "Threshold",
    "Params" -> <|"θ" -> θ|>,
    "Apply" -> Function[obs,
      With[{s = If[KeyExistsQ[obs, "x"], Total@obs["x"], If[KeyExistsQ[obs, "zObs"], obs["zObs"], 0.]]},
        With[{p = 1./(1. + Exp[-(s - θ)])}, {1 - p, p}]
      ]
    ],
    "Grad" -> Function[{θAssoc, obs},
      With[{s = If[KeyExistsQ[obs, "x"], Total@obs["x"], If[KeyExistsQ[obs, "zObs"], obs["zObs"], 0.]],
            θ0 = θAssoc["θ"]},
        With[{p = 1./(1. + Exp[-(s - θ0)])},
          {0., 0.} + (p*(1 - p))*(1) (* scalar derivative; lifted to 2-dim via log-ratio trick if needed *)
        ]
      ]
    ]
  ]
]

MakePolicyFamily["SoftmaxBandit", params_Association] := Module[
  {w = params["w"] /. Missing[_, _] -> ConstantArray[0., params["K"]]},
  Association[
    "Type" -> "SoftmaxBandit",
    "Params" -> <|"w" -> w|>,
    "Apply" -> Function[_,
      With[{s = w - Max@w}, With[{p = Exp[s]/Total@Exp[s]}, p]]
    ]
  ]
]

End[]
EndPackage[]