open Yates_solvers
open Yates_types

module Make(A:Types.Algorithm) : sig
  val start : string -> string -> string -> Solvers.solver_type  -> bool -> unit -> unit
end
