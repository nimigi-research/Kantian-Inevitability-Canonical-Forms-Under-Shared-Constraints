(* RunPhaseMap.wls — minimal demo sweep for T1 and T2 *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "KIInit.wl"}]];
KI`Load[];

(* T1: noisy majority sweep *)
t1Grid = Range[0., 0.6, 0.1];
t1Seeds[env_] := Table[KI`Experiments`MakePolicyFamily["Threshold", <|"θ" -> RandomReal[{-1, 6}]|>], 20];
t1PM = KI`PhaseMap`PhaseMapSweep[t1Grid, KI`Experiments`GenNoisyMajority, t1Seeds, NSamples -> 128];

(* T2: sparse bandit sweep *)
t2Grid = {1,2,4,8};
t2Seeds[env_] := {KI`Experiments`MakePolicyFamily["SoftmaxBandit", <|"w" -> RandomReal[{-0.5, 0.5}, env["NActions"]]|>]}~Join~
                 {KI`Experiments`MakePolicyFamily["SoftmaxBandit", <|"w" -> RandomReal[{-0.5, 0.5}, env["NActions"]]|>]}~Join~
                 Table[KI`Experiments`MakePolicyFamily["SoftmaxBandit", <|"w" -> UnitVector[env["NActions"], i]*0.5|>], {i, env["NActions"]}];
t2PM = KI`PhaseMap`PhaseMapSweep[t2Grid, KI`Experiments`GenSparseValueBandit, t2Seeds, NSamples -> 128];

Print["T1 Phase Map Summary:"]; Print[Keys@t1PM["Results"]];
Print["T2 Phase Map Summary:"]; Print[Keys@t2PM["Results"]];