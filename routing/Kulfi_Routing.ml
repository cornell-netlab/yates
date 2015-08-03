open Kulfi_Types
                 
module type Algorithm = sig
    val solve : topology -> demands -> scheme -> scheme
end

module Spf = Kulfi_Spf

(* module Ecmp = Kulfi_Ecmp *)

module Mcf = Kulfi_MCF

(* module Oblivious = Kulfi_Oblivious *)

