open Core.Std

module Make(A:Kulfi_Routing.Algorithm) : sig
  val start : Kulfi_Types.topology -> string -> string -> string -> unit -> unit
end
