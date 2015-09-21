open Kulfi_Types
                 
module type Algorithm = sig
  val solve : topology -> demands -> scheme -> scheme
end

module Ecmp = Kulfi_Ecmp 

module Mcf = Kulfi_Mcf

module SemiMcf = Kulfi_SemiMcf

module Raeke = Kulfi_Raeke

module Spf = Kulfi_Spf
               
module Vlb = Kulfi_Vlb

module Ak = Kulfi_Ak



