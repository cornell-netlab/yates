open Core.Std
open Kulfi_Util
open Kulfi_Types

type solver_type =
  | Mcf | MwMcf | Vlb | Ecmp | Edksp | Ksp | Spf | Raeke
  | AkMcf | AkVlb | AkRaeke | AkEcmp | AkKsp
  | Ffc | Ffced
  | SemiMcfMcf | SemiMcfMcfEnv | SemiMcfMcfFTEnv | SemiMcfVlb | SemiMcfRaeke | SemiMcfRaekeFT  | SemiMcfEcmp | SemiMcfKsp | SemiMcfKspFT
  | OptimalMcf

type network_iter_state = {
  ingress_link_traffic  : ((edge Array.t * int * float) list) EdgeMap.t [@default EdgeMap.empty];
  delivered             : float SrcDstMap.t                 [@default SrcDstMap.empty];
  latency               : (float LatencyMap.t) SrcDstMap.t  [@default SrcDstMap.empty];
  utilization           : (float list) EdgeMap.t            [@default EdgeMap.empty];
  scheme                : scheme    [@default SrcDstMap.empty];
  failures              : failure   [@default EdgeSet.empty];
  failure_drop          : float     [@default 0.0];
  congestion_drop       : float     [@default 0.0];
  real_tm               : demands   [@default SrcDstMap.empty];
  predict_tm            : demands   [@default SrcDstMap.empty];
} [@@deriving make]

type sim_tm_stats = {
  throughput        : throughput SrcDstMap.t  [@default SrcDstMap.empty];
  latency           : throughput LatencyMap.t [@default LatencyMap.empty];
  congestion        : (congestion * congestion) EdgeMap.t    [@default EdgeMap.empty];
  failure_drop      : throughput              [@default 0.];
  congestion_drop   : throughput              [@default 0.];
  flash_throughput  : throughput              [@default 0.];
  aggregate_demand  : demand                  [@default 0.];
  recovery_churn    : float                   [@default 0.];
  scheme            : scheme                  [@default SrcDstMap.empty];
  solver_time       : float                   [@default 0.];
} [@@deriving make]

