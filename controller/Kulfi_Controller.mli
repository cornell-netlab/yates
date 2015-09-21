open Core.Std

module Make(A:Kulfi_Routing.Algorithm) : sig
  val start : string -> string -> string -> string option -> unit -> unit
end
