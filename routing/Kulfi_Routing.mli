
open Kulfi_RoutingScheme
open Kulfi_Types
                       
module type ALGORITHM = sig
    val solve : topology -> demands -> scheme -> scheme  
end

module Spf : ALGORITHM

module Ecmp : ALGORITHM

module Mcf : ALGORITHM

module Oblivious: ALGORITHM
