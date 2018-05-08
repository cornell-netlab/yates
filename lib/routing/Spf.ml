open Core

open Apsp
open Yates_types.Types

(***************)
(* local state *)
(***************)
let prev_scheme = ref SrcDstMap.empty


(***********************)
(* algorithm interface *)
(***********************)

(* Initialization not needed *)
let initialize _ : unit = ()

(* Recovery: normalization recovery *)
let local_recovery = Util.normalization_recovery


(* Solve: shortest paths *)
let solve (topo:topology) (_:demands) : scheme =
  let new_scheme =
    if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
    else
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
                  SrcDstMap.set acc ~key:(u,v) ~data:(PathMap.singleton rand_path 1.0)
              end
            | _ -> acc) in
  prev_scheme := new_scheme;
  new_scheme
