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

(* Solve: Uniform distributoin over k-shortest paths *)
let solve (topo:topology) (_:demands) : scheme =
  let new_scheme =
    if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
    else
      let host_set = Util.get_hosts_set topo in
      let all_ksp = all_pair_k_shortest_path topo
          (min !Globals.budget 1000) host_set in
      SrcDstMap.fold all_ksp ~init:SrcDstMap.empty
        ~f:(fun ~key:(u, v) ~data:paths acc ->
            if u = v then acc
            else
              let path_map =
                List.fold_left paths ~init:PathMap.empty
                  ~f:(fun acc path ->
                      let prob = 1.0 /. Float.of_int (List.length paths) in
                      PathMap.set acc ~key:path ~data:prob) in
              SrcDstMap.set acc ~key:(u, v) ~data:path_map) in
  prev_scheme := new_scheme;
  new_scheme
