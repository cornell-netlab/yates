
open Kulfi_Types
                       
module type Algorithm = sig
    val solve : topology -> demands -> scheme -> scheme  
end

module Ecmp : Algorithm 

module Mcf : Algorithm

module Mw : Algorithm

module Raeke : Algorithm

module Spf : Algorithm

module Vlb : Algorithm
