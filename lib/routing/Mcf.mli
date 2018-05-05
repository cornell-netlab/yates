open Yates_types.Types

val solve : topology -> demands -> scheme

val initialize : scheme -> unit

val local_recovery : scheme -> topology -> failure -> demands -> scheme

(* Helper functions *)
val recover_paths : topology -> flow_table -> scheme


