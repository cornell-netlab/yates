open Kulfi_Types
open Frenetic_Network
open Net
open Net.Topology
open Core.Std
open Kulfi_Apsp

module PQueue = Core.Heap.Removable
module VertexSet = Topology.VertexSet
module EdgeSet = Topology.EdgeSet


let solve (topo:topology) (_:demands) (_:scheme) : scheme =
  let host_set =
    VertexSet.filter
      (vertexes topo)
      ~f:(fun v ->
          let lbl = vertex_to_label topo v in
          Node.device lbl = Node.Host) in
  let paths_hash = all_shortest_paths_multi topo host_set in
  Hashtbl.Poly.fold
    paths_hash
    ~init:SrcDstMap.empty
    ~f:(fun ~key:v1 ~data:dst_paths acc ->
        List.fold_left
          dst_paths
          ~init:acc
          ~f:(fun acc (v2,paths) -> 
              let prob = 1.0 /. Float.of_int (List.length paths) in 
              let path_dist =
                List.fold_left
                  paths
                  ~init:PathMap.empty
                  ~f:(fun acc path ->
                      PathMap.add acc ~key:path ~data:prob) in
              SrcDstMap.add acc ~key:(v1,v2) ~data:path_dist))
