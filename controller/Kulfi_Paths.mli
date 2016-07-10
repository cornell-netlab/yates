open Core.Std
open Frenetic_Network
open Frenetic_OpenFlow
open Frenetic_OpenFlow0x01
open Kulfi_Types

val add_paths_from_scheme : scheme -> Tag.t PathMap.t -> Tag.t PathMap.t
val create_sw_flows_map : topology -> Tag.t PathMap.t -> (switchId, flowMod list) Hashtbl.t
