open Core.Std

module Make(A:Kulfi_Routing.Algorithm) : sig
  val start : Kulfi_Types.topology -> Kulfi_Types.demands -> Kulfi_Types.demands -> unit -> unit
end
