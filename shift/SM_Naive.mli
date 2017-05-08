open Kulfi_Types
open SM_LP_Lang

val model : topology -> vertex -> demands -> demands

(* Reuse some of the constraint generators *)
val capacity_constraints : topology -> vertex -> demands -> constrain list -> constrain list
val conservation_constraints : topology -> vertex -> demands -> constrain list -> constrain list
