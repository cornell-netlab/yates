open Yates_types.Types

module type Algorithm = sig
    val initialize : scheme -> unit
    val local_recovery : scheme -> topology -> failure -> demands -> scheme
    val solve : topology -> demands -> scheme
end

module AC : Algorithm

module Ak : Algorithm

module Cspf : Algorithm

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
