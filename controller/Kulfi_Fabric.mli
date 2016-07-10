open Core.Std
open Frenetic_OpenFlow
open Frenetic_OpenFlow0x01
open Kulfi_Types

val create : topology -> (switchId, flowMod list) Hashtbl.t * (edge, int) Hashtbl.t
