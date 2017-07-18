open Frenetic_Network
open Net
open Net.Topology
open Core
open Kulfi_Apsp
open Kulfi_Globals
open Kulfi_Types

let prev_scheme = ref SrcDstMap.empty

let solve (topo:topology) (_:demands) : scheme =
  let new_scheme =
  if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
  else
  let host_set = get_hosts_set topo in
  let all_ksp = all_pair_k_shortest_path topo (min !Kulfi_Globals.budget 100) host_set in
  SrcDstMap.fold
    all_ksp
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(v1,v2) ~data:paths acc ->
      if (v1 = v2) then acc
      else
      let path_map = List.fold_left
          paths
          ~init:PathMap.empty
          ~f:(fun acc path ->
              let prob = 1.0 /. Float.of_int (List.length paths) in
              PathMap.add acc ~key:path ~data:prob) in
      SrcDstMap.add acc ~key:(v1,v2) ~data:path_map) in
  prev_scheme := new_scheme;
  new_scheme

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = Kulfi_Types.normalization_recovery
