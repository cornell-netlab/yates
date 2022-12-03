open Core
open Frenetic_kernel.OpenFlow0x01
open Yates_types.Types

val add_paths_from_scheme : scheme -> Tag.t PathMap.t -> Tag.t PathMap.t
val create_sw_flows_map : topology -> Tag.t PathMap.t -> (switchId, flowMod list) Hashtbl.t
