open Core.Std

module Make(A:Kulfi_Routing.Algorithm) : sig
  val start : Kulfi_Types.topology -> unit -> unit
end
