
open Kulfi_Types

module type Algorithm = sig
    val solve : topology -> demands -> scheme
    val initialize : scheme -> unit
    val local_recovery : scheme -> topology -> failure -> demands -> scheme
end

module Ecmp : Algorithm

module Ksp : Algorithm

module Mcf : Algorithm

module MwMcf : Algorithm

module SemiMcf : Algorithm

module Raeke : Algorithm

module Spf : Algorithm

module Vlb : Algorithm

module Ak : Algorithm
