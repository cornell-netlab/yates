open Yates_Types

val solve : topology -> demands -> scheme

val initialize : scheme -> unit

val local_recovery : scheme -> topology -> failure -> demands -> scheme

val scheme_and_flows : ((uid * float) list) -> uid_path_map -> (scheme * float SrcDstMap.t)

val normalize : scheme -> float SrcDstMap.t -> scheme
