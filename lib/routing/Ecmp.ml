open Core

open Apsp
open Util
open Yates_types.Types

let prev_scheme = ref SrcDstMap.empty

let rec get_all_shortest_paths (u:Topology.vertex) (v:Topology.vertex)
          (topo:topology)
          (apsp: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t)
          (max_num_paths: int) : (Topology.edge List.t) List.t =
  if u = v then [[]]
  else
    let _,_,nhop_list= SrcDstMap.find_exn apsp (u, v) in
    List.fold_left nhop_list ~init: []
        ~f:(fun acc (nhop,_)  ->
          let remains = max_num_paths - (List.length acc) in
          if remains < 0 then
            acc
          else
            let nh_edge = Topology.find_edge topo u nhop in
            let nh_paths = get_all_shortest_paths nhop v topo apsp remains in
            List.fold_left nh_paths
              ~init:acc
              ~f:(fun acc nh_path ->
                (nh_edge::nh_path)::acc))

let solve_thresh (topo:topology) (_:demands) : scheme =
  (* Not used *)
  (* run ksp with budget; find shortest path;
   * select paths with weight <= 1.2 times shortest *)
  let new_scheme =
    if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
    else
      let host_set = get_hosts_set topo in
      let all_ksp =
        all_pair_k_shortest_path topo (min !Globals.budget 100) host_set in
      let thresh = 1.2 in
      SrcDstMap.fold all_ksp
        ~init: SrcDstMap.empty
        ~f:(fun ~key:(u,v) ~data:paths acc ->
          if (u = v) then acc
          else
          if List.is_empty paths then acc
          else
            let sorted_paths = List.sort ~compare:(fun x y -> Pervasives.compare (get_path_weight topo x) (get_path_weight topo y)) paths in
            let shortest_path_weight = get_path_weight topo
                                         (match List.hd sorted_paths with
                                          | None -> assert false
                                          | Some x -> x) in
            let selected_paths =
              List.filter sorted_paths
                ~f:(fun p ->
                  (get_path_weight topo p) <= (thresh *. shortest_path_weight)) in
            let prob = 1. /. Float.of_int (List.length selected_paths) in
            let path_dist =
              List.fold_left selected_paths ~init: PathMap.empty
                ~f:(fun acc p ->
                  PathMap.set ~key:p ~data:prob acc) in
            SrcDstMap.set ~key:(u,v) ~data:path_dist acc) in
  prev_scheme := new_scheme;
  new_scheme


let solve (topo:topology) (_:demands) : scheme =
  let new_scheme =
    if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
    else
      (* Compute all pair multiple shortest paths, *)
      (* For each next hop in that, reconstruct shortest paths recursively  *)
      let apmsp = all_pairs_multi_shortest_path topo in
      let host_set = get_hosts_set topo in

      (* Limit number of paths to 100 or budget, whichever is minimum *)
      let max_num_paths = min !Globals.budget 100 in
      SrcDstMap.fold apmsp ~init: SrcDstMap.empty
        ~f:(fun ~key:(u, v) ~data:(_,n,probs) acc ->
          if (VertexSet.mem host_set u) && (VertexSet.mem host_set v) then
            let paths = get_all_shortest_paths u v topo apmsp max_num_paths in
            let prob = 1.0 /. Float.of_int (List.length paths) in
            let path_dist =
              List.fold_left paths
                ~init: PathMap.empty
                ~f:( fun acc path ->
                  PathMap.set acc ~key:path ~data:prob;) in
            SrcDstMap.set acc ~key:(u, v) ~data: path_dist
          else
            acc) in
  prev_scheme := new_scheme;
  new_scheme

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = normalization_recovery
