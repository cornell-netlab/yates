open Core

open Util
open Yates_types.Types

module PQueue = Core_kernel.Heap

(**************************************************************)
(* All pair multiple shortest paths *)
(**************************************************************)
let abs_fl (n:float) =
  if n > 0.0 then n else -.n
let rec calc_num_paths (i:Topology.vertex) (j:Topology.vertex)
          (topo:topology) (dist: float SrcDstMap.t)
          (numpath: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t)
  : (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t =
  (* Count the number of shortest i-j paths, and store the corresponding next
     hop nodes *)
  let  (_, n, l) = SrcDstMap.find_exn numpath (i,j) in
  let numpath = SrcDstMap.set numpath ~key:(i,j) ~data:(true, n, l) in
  let d_ij = SrcDstMap.find_exn dist (i,j) in
  if d_ij = Float.infinity then
    SrcDstMap.set numpath ~key:(i,j) ~data:(true, 0, [])
  else
    let neighbors = Topology.neighbors topo i in
    let numpath =
      (* For each neighbor, find the shortest paths through it and check if
         there is a shortest i-j path. *)
      VertexSet.fold neighbors ~init:numpath
        ~f:(fun acc neighbor ->
            let label = Topology.vertex_to_label topo neighbor in
            if Node.device label = Node.Host && neighbor <> j then acc
            else
              (* Neighbor must not be a host, unless it is the destination. *)
              let out_edge = Topology.find_edge topo i neighbor in
              let wt_in = Link.weight (Topology.edge_to_label topo out_edge) in
              let d_nj = SrcDstMap.find_exn dist (neighbor,j) in
              if (abs_fl (wt_in +. d_nj -. d_ij) < 0.00001) then
                (* if it is in a shortest i-j path *)
                let t_visited,_,_ = SrcDstMap.find_exn acc (neighbor,j) in
                let numpath =
                  if t_visited then acc
                  else calc_num_paths neighbor j topo dist acc  in
                let _, np_nj, _ = SrcDstMap.find_exn numpath (neighbor,j) in
                let _, np_ij, next_hops_ij = SrcDstMap.find_exn numpath (i,j) in
                (* Extend i-j paths by number of shortest paths through this
                   neighbor *)
                SrcDstMap.set numpath ~key:(i,j)
                  ~data:(true, np_ij + np_nj, List.append next_hops_ij
                           [(neighbor, Float.of_int np_nj)])
              else acc) in

    let _, num_ij_paths, next_hops_ij = SrcDstMap.find_exn numpath (i,j) in
    let path_probs,_ =
      List.fold_left next_hops_ij
        ~init:([], 0.0)
        ~f:(fun acc (next_hop, np_h) ->
            let l, preprob = acc in
            let n_prob = np_h /. Float.of_int num_ij_paths in
            (List.append l [(next_hop, preprob +. n_prob)],
             preprob +. n_prob)) in
    SrcDstMap.set numpath ~key:(i,j) ~data:(true, num_ij_paths, path_probs)


let all_pairs_multi_shortest_path (topo:topology) :
  (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t =
  (* topology -> (visited_bool, normalizer, list(next_hop, prob)) SrcDstMap *)

  let dist_mat = Topology.fold_vertexes
    (fun i acc -> Topology.fold_vertexes
        (fun j acc ->
          let dist =
            if i = j then 0.0
            else Float.infinity in
          SrcDstMap.set acc ~key:(i, j) ~data:dist)
        topo
        acc)
    topo
    SrcDstMap.empty in

  (* Floyd-Warshall *)
  let dist_mat = Topology.fold_edges
  (fun e acc ->
    let src,_ = Topology.edge_src e in
    let dst,_ = Topology.edge_dst e in
    let weight = Link.weight (Topology.edge_to_label topo e) in
    SrcDstMap.set acc ~key:(src, dst) ~data:weight)
  topo dist_mat in

  (* Only switches can be intermediate nodes*)
  let switches = get_switch_set topo in
  let dist_mat_sp = VertexSet.fold switches ~init:dist_mat
    ~f:(fun acc k ->
      Topology.fold_vertexes
        (fun i acc ->
          Topology.fold_vertexes
          (fun j acc ->
            let dij = SrcDstMap.find_exn acc (i,j)  in
            let dik = SrcDstMap.find_exn acc (i,k)  in
            let dkj = SrcDstMap.find_exn acc (k,j)  in
            if (dik +. dkj < dij) then
              SrcDstMap.set acc ~key:(i, j) ~data:(dik +. dkj)
            else acc)
          topo acc)
        topo acc) in
  (* Floyd-Warshall complete *)

  (* initialize visited to be true for i,i *)
  let init_vis_npath_pathlist_map =
    Topology.fold_vertexes
      (fun i acc ->
         Topology.fold_vertexes
           (fun j acc ->
              (* if i == j, then visited is true, and num_paths = 1 *)
              let visited = (i = j) in
              let num_paths = if (i = j) then 1 else 0 in
              SrcDstMap.set acc ~key:(i, j) ~data:(visited, num_paths, []))
           topo acc)
      topo SrcDstMap.empty in

  Topology.fold_vertexes
    (fun i acc ->
       Topology.fold_vertexes
         (fun j acc ->
            let visited,np,_ = SrcDstMap.find_exn acc (i,j) in
            if visited then acc
            else calc_num_paths i j topo dist_mat_sp acc)
         topo acc)
    topo init_vis_npath_pathlist_map

let print_mpapsp
  (numpath : (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t)
  (topo : topology) =
  Topology.fold_vertexes
  (fun i acc_i ->
    Topology.fold_vertexes
    (fun j acc_j ->
      let v,np,pathlist = SrcDstMap.find_exn numpath (i,j) in
      Printf.printf "%s\t" (Node.name (Topology.vertex_to_label topo i));
      Printf.printf "%s\t" (Node.name (Topology.vertex_to_label topo j));
      Printf.printf "%B\t" v;
      Printf.printf "%d\t" np;
      Printf.printf "%d\n" (List.length pathlist);
      acc_j)
    topo [])
  topo []


let get_random_path (i:Topology.vertex) (j:Topology.vertex) (topo:topology)
    (numpath: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t) : path option =
  let _,num_path,_ = SrcDstMap.find_exn numpath (i,j) in
  if num_path = 0 then None
  else
    let p = ref [] in
    let curr = ref i in
    let stop_cond = ref false in
    while not !stop_cond do
      let _,_,nhop_list = SrcDstMap.find_exn numpath (!curr,j) in
      let rand = Random.float 1.0 in
      let chosen_hop,_ = List.fold_left nhop_list
          ~init:(i,false)
          ~f:(fun acc (nhop, prob) ->
              if prob < (rand -. 0.00001) then acc
              else let _, found_bool = acc in
                if not found_bool then (nhop,true)
                else acc) in
      p := List.append !p [chosen_hop];
      curr := chosen_hop;
      stop_cond := if !curr = j then true else false;
    done;
    let v_path = !p in

    let e_path = ref [] in
    let _ =
      List.fold_left v_path
        ~init:i
        ~f:(fun last_v curr_v ->
            if last_v = curr_v then curr_v
            else let e = Topology.find_edge topo last_v curr_v in
              e_path := List.append !e_path [e];
              curr_v) in
    Some !e_path

(**************************************************************)
(* k-shortest paths *)
(**************************************************************)

(* Yen's algorithm to compute k-shortest paths *)
let k_shortest_paths (full_topo:topology) (s:Topology.vertex) (t:Topology.vertex)
      (k:int) : path list =
  if s = t then []
  else
    let hosts = get_hosts_set full_topo in
    let topo = VertexSet.fold hosts ~init:full_topo
      ~f:(fun acc h ->
          if s = h || t = h then acc
          else Topology.remove_vertex acc h) in
    let bheap =
      PQueue.create
        ~min_size:(Topology.num_vertexes topo)
        ~cmp:(fun (dist1,_) (dist2,_) -> compare dist1 dist2) () in
    let pq_tokens = Hashtbl.Poly.create () in
    let rec yens_explore j ksp =
      if j = k then ksp
      else
        let prev_path = List.hd_exn ksp in
        let _ =
          List.fold prev_path ~init:[]
            ~f:(fun root_path spur_edge ->
              let spur_node,_ = Topology.edge_src spur_edge in
              let root_path_len = List.length root_path in
              let restricted_topo =
                List.fold ksp ~init:topo
                  ~f:(fun acc path ->
                    let sub_path =
                      List.slice path 0 (min root_path_len (List.length path)) in
                    if root_path = sub_path then
                      match List.nth path root_path_len with
                      | None -> acc
                      | Some e ->
                        Topology.remove_edge acc e
                    else
                      acc) in
              let restricted_topo =
                List.fold root_path ~init:restricted_topo
                  ~f:(fun acc e ->
                    let root_path_node,_ = Topology.edge_src e in
                    if root_path_node = spur_node then acc
                    else Topology.remove_vertex acc root_path_node) in

              let new_root_path = root_path@[spur_edge] in
              match NetPath.shortest_path restricted_topo spur_node t with
              | None -> new_root_path
              | Some p ->
                let total_path = root_path@p in
                let path_weight = get_path_weight topo total_path in
                let new_token =
                  match Hashtbl.Poly.find pq_tokens total_path with
                  | None ->
                    PQueue.add_removable bheap (path_weight, total_path)
                  | Some token ->
                    PQueue.update bheap token (path_weight, total_path) in
                Hashtbl.Poly.set pq_tokens total_path new_token;
                new_root_path) in
        if PQueue.is_empty bheap then ksp
        else
          (* Yen's algorithm can generate duplicate spur paths. Avoid
             duplication when storing k-shortest paths. *)
          let rec find_non_dup () =
            match PQueue.pop bheap with
            | None -> None
            | Some (_, path) ->
              if List.mem ksp path ~equal:(=) then find_non_dup ()
              else Some path in
          match find_non_dup () with
          | None -> ksp
          | Some path ->
            let ksp = path::ksp in
            yens_explore (j + 1) ksp in

    let shortest_path = match NetPath.shortest_path topo s t with
      | Some x -> x
      | _ -> failwith "Couldn't find shortest path" in

    yens_explore 1 [shortest_path]

(* Compute k-shortest paths for every pair of hosts *)
let all_pair_k_shortest_path (topo:topology) (k:int) hosts =
  VertexSet.fold hosts ~init:SrcDstMap.empty
    ~f:(fun acc src ->
      VertexSet.fold hosts ~init:acc
        ~f:(fun acc dst ->
          let ksp = k_shortest_paths topo src dst k in
          SrcDstMap.set acc ~key:(src, dst) ~data:ksp))
