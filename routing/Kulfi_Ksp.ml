open Kulfi_Types
open Frenetic_Network
open Net
open Net.Topology
open Core.Std
open Kulfi_Apsp

let solve (topo:topology) (_:demands) (_:scheme) : scheme =
  let all_ksp = all_pair_k_shortest_path topo 3 in
  SrcDstMap.fold
    all_ksp
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(v1,v2) ~data:paths acc ->
      let path_map = List.fold_left
          paths
          ~init:PathMap.empty
          ~f:(fun acc path ->
              let prob = 1.0 /. Float.of_int (List.length paths) in
              PathMap.add acc ~key:path ~data:prob) in
      SrcDstMap.add acc ~key:(v1,v2) ~data:path_map)
