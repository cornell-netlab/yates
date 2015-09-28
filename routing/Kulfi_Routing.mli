
open Kulfi_Types
                       
module type Algorithm = sig
    val solve : ?deloop:bool -> topology -> demands -> scheme -> scheme  
end

module Ecmp : Algorithm 

module Mcf : Algorithm

module MwMcf : Algorithm

module SemiMcf : Algorithm

module Raeke : Algorithm

module Spf : Algorithm

module Vlb : Algorithm

module Ak : Algorithm
