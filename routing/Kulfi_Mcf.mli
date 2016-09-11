open Kulfi_Types

val solve : topology -> demands -> scheme

val initialize : scheme -> unit

val local_recovery : scheme -> topology -> failure -> demands -> scheme
