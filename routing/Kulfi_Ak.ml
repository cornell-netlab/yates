open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net

let mu = ref max_float

let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  (* First build HashMaps, keyed by edges, containing the
     values f(e), f_i(e), from the pseudocode. *)
  assert false

