
open Kulfi_RoutingScheme
open Kulfi_Types
                       
module type Algorithm = sig
    val solve : topology -> demands -> scheme -> scheme  
end

module Spf : Algorithm

(* module Ecmp : ALGORITHM *)

module Mcf : Algorithm

(* module Oblivious: ALGORITHM *)
