open Kulfi_Types

module type Algorithm = sig
    val solve : topology -> demands -> scheme
    val initialize : scheme -> unit
    val local_recovery : scheme -> topology -> failure -> demands -> scheme
end

module Ac : Algorithm

module Ak : Algorithm

module Ecmp : Algorithm

module Edksp : Algorithm

module Ffc : Algorithm

module Ksp : Algorithm

module Mcf : Algorithm

module MwMcf : Algorithm

module Raeke : Algorithm

module SemiMcf : Algorithm

module Spf : Algorithm

module Vlb : Algorithm
