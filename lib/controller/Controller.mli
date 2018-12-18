open Core

open Yates_routing
open Yates_solvers

module Make(A:Yates_Routing.Algorithm) : sig
  val start : string -> string -> string -> Solvers.solver_type  -> bool -> unit -> unit
end
