open Kulfi_Types
                 
module type ALGORITHM = sig
    val solve : topology -> demands -> scheme -> scheme
end

module Spf = Kulfi_Spf

module Ecmp = Kulfi_Ecmp

module Mcf = Kulfi_Mcf

module Oblivious = Kulfi_Oblivious

