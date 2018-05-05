open Core

open Yates_routing

module Make(A:Yates_Routing.Algorithm) : sig
  val start : string -> string -> string -> string option -> bool -> unit -> unit
end
