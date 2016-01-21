open Kulfi_Types

module type Algorithm = sig
  val solve : topology -> demands -> scheme
  val initialize : scheme -> unit
  val local_recovery : scheme -> topology -> failure -> demands -> scheme
end

module Ecmp = Kulfi_Ecmp

module Mcf = Kulfi_Mcf

module MwMcf = Kulfi_MwMcf

module SemiMcf = Kulfi_SemiMcf

module Raeke = Kulfi_Raeke

module Spf = Kulfi_Spf

module Vlb = Kulfi_Vlb

module Ak = Kulfi_Ak

module Ksp = Kulfi_Ksp
