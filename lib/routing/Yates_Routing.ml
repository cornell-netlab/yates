open Yates_Types

module type Algorithm = sig
  val solve : topology -> demands -> scheme
  val initialize : scheme -> unit
  val local_recovery : scheme -> topology -> failure -> demands -> scheme
end

module Ac = Yates_AC

module Ak = Yates_Ak

module Cspf  = Yates_Cspf

module Ecmp = Yates_Ecmp

module Edksp = Yates_Edksp

module Ffc = Yates_Ffc

module Ksp = Yates_Ksp

module Mcf = Yates_Mcf

module MwMcf = Yates_MwMcf

module Raeke = Yates_Raeke

module SemiMcf = Yates_SemiMcf

module Spf = Yates_Spf

module Vlb = Yates_Vlb
