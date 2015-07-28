
open Kulfi_RoutingScheme
open Kulfi_Types
                       
module type ALGORITHM = sig
    val solve : topology -> demands -> scheme -> scheme  
end

(** Given an ALGORITHM module, build a solver that will 
   return a RoutingScheme when invoked *)
module Make : functor (Algorithm:ALGORITHM) -> sig

   val solve : topology -> demands -> scheme -> scheme  

end
