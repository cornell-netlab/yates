open Kulfi_Types

val solve : topology -> demands -> scheme

val initialize : scheme -> unit

val local_recovery : scheme -> topology -> failure -> demands -> scheme

(* Other useful functions *)
val recover_paths : topology  -> flow_table -> scheme
