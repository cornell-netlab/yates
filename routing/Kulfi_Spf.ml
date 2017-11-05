open Core

open Kulfi_Apsp
open Kulfi_Types
open Kulfi_Util

let solve (topo:topology) (_:demands) : scheme =
  let device v =
    let lbl = Topology.vertex_to_label topo v in
    (Node.device lbl) in
  let mpapsp = all_pairs_multi_shortest_path topo in
  SrcDstMap.fold mpapsp
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(u,v) ~data:_ acc ->
        match (device u, device v) with
        | (Node.Host, Node.Host) ->
          begin
            match (get_random_path u v topo mpapsp) with
            | None -> acc
            | Some rand_path ->
              SrcDstMap.add acc ~key:(u,v) ~data:(PathMap.singleton rand_path 1.0)
          end
        | _ -> acc)

let initialize _ = ()

let local_recovery = normalization_recovery
