open Core
open Kulfi_Util
open Kulfi_Types

type solver_type =
  | Ac | Cspf | Ecmp | Edksp | Ksp | Mcf | MwMcf | Raeke | Spf | Vlb
  | AkMcf | AkRaeke | AkEcmp | AkKsp | AkVlb
  | Ffc | Ffced
  | SemiMcfAc | SemiMcfEcmp | SemiMcfEdksp | SemiMcfKsp | SemiMcfKspFT
  | SemiMcfMcf | SemiMcfMcfEnv | SemiMcfMcfFTEnv | SemiMcfRaeke
  | SemiMcfRaekeFT | SemiMcfVlb
  | OptimalMcf

type network_iter_state = {
  ingress_link_traffic  : ((edge Array.t * int * float) list) EdgeMap.t;
  delivered       : float SrcDstMap.t;
  latency         : (float LatencyMap.t) SrcDstMap.t;
  utilization     : (float list) EdgeMap.t;
  scheme          : scheme;
  failures        : failure;
  failure_drop    : float;
  congestion_drop : float;
  real_tm         : demands;
  predict_tm      : demands;
}

type sim_tm_stats = {
  throughput    : throughput SrcDstMap.t;
  latency       : throughput LatencyMap.t;
  congestion    : (congestion * congestion) EdgeMap.t;
  failure_drop  : throughput;
  congestion_drop   : throughput;
  flash_throughput  : throughput;
  aggregate_demand  : demand;
  recovery_churn    : float;
  scheme            : scheme;
  solver_time       : float;
}
