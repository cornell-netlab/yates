open Core
open Frenetic_kernel.OpenFlow0x01
open Yates_types.Types

val create : topology -> (switchId, flowMod list) Hashtbl.t * (edge, int) Hashtbl.t
