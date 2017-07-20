open Kulfi_Types

module type Algorithm = sig
  val solve : topology -> demands -> scheme
  val initialize : scheme -> unit
  val local_recovery : scheme -> topology -> failure -> demands -> scheme
end

module Ac = Kulfi_AC

module Ak = Kulfi_Ak

module Cspf  = Kulfi_Cspf

module Ecmp = Kulfi_Ecmp

module Edksp = Kulfi_Edksp

module Ffc = Kulfi_Ffc

module Ksp = Kulfi_Ksp

module Mcf = Kulfi_Mcf

module MwMcf = Kulfi_MwMcf

module Raeke = Kulfi_Raeke

module SemiMcf = Kulfi_SemiMcf

module Spf = Kulfi_Spf

module Vlb = Kulfi_Vlb
