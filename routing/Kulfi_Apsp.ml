open Core
open Frenetic_Network
open Net
open Net.Topology

open Kulfi_Types

module PQueue = Core_kernel.Heap.Removable

(**************************************************************)
(* All pair multiple shortest paths *)
(**************************************************************)

let rec calc_num_paths (i:Topology.vertex) (j:Topology.vertex)
          (topo:topology) (dist: float SrcDstMap.t)
          (numpath: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t)
  : (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t =
  (* Count the number of shortest i-j paths, and store the corresponding next
     hop nodes *)
  let  (_, n, l) = SrcDstMap.find_exn numpath (i,j) in
  let numpath = SrcDstMap.add numpath ~key:(i,j) ~data:(true, n, l) in
  let d_ij = SrcDstMap.find_exn dist (i,j) in
  let neighbors = Topology.neighbors topo i in
  let numpath =
    (* For each neighbor, find the shortest paths through it and check if there
    is a shortest i-j path *)
    VertexSet.fold neighbors ~init:numpath
      ~f:(fun acc neighbor ->
        let out_edge = Topology.find_edge topo i neighbor in
        let wt_in = Link.weight (Topology.edge_to_label topo out_edge) in
        let d_nj = SrcDstMap.find_exn dist (neighbor,j) in
        if wt_in +. d_nj = d_ij then
          (* if it is in a shortest i-j path *)
          let t_visited,_,_ = SrcDstMap.find_exn acc (neighbor,j) in
          let numpath =
            if t_visited then acc
            else calc_num_paths neighbor j topo dist acc  in
          let _, np_nj, _ = SrcDstMap.find_exn numpath (neighbor,j) in
          let _, np_ij, next_hops_ij = SrcDstMap.find_exn numpath (i,j) in
          (* Extend i-j paths by number of shortest paths through this neighbor *)
          SrcDstMap.add numpath ~key:(i,j)
            ~data:(true, np_ij + np_nj,
                   List.append next_hops_ij [(neighbor, Float.of_int np_nj)])
        else acc) in

  let _, num_ij_paths, next_hops_ij = SrcDstMap.find_exn numpath (i,j) in
  let path_probs,_ =
    List.fold_left next_hops_ij
      ~init:([], 0.0)
      ~f:(fun acc (next_hop, np_h) ->
        let l, preprob = acc in
        let n_prob = np_h /. Float.of_int num_ij_paths in
        (List.append l [(next_hop, preprob +. n_prob)], preprob +. n_prob)) in
  SrcDstMap.add numpath ~key:(i,j) ~data:(true, num_ij_paths, path_probs)


let all_pairs_multi_shortest_path (topo:topology) :
  (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t =
  (* topology -> (visited_bool, normalizer, list(next_hop, prob)) SrcDstMap *)

  let dist_mat = Topology.fold_vertexes
    (fun i acc -> Topology.fold_vertexes
        (fun j acc ->
          let dist =
            if i = j then 0.0
            else Float.infinity in
          SrcDstMap.add acc ~key:(i, j) ~data:dist)
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
    SrcDstMap.add acc ~key:(src, dst) ~data:weight)
  topo dist_mat in

  let dist_mat_sp = Topology.fold_vertexes
    (fun k acc ->
      Topology.fold_vertexes
        (fun i acc ->
          Topology.fold_vertexes
          (fun j acc ->
            let dij = SrcDstMap.find_exn acc (i,j)  in
            let dik = SrcDstMap.find_exn acc (i,k)  in
            let dkj = SrcDstMap.find_exn acc (k,j)  in
            if (dik +. dkj < dij) then
              SrcDstMap.add acc ~key:(i, j) ~data:(dik +. dkj)
            else acc)
          topo acc)
        topo acc)
    topo dist_mat in
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
              SrcDstMap.add acc ~key:(i, j) ~data:(visited, num_paths, []))
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
      Printf.printf "%s\t" (Node.name (Net.Topology.vertex_to_label topo i));
      Printf.printf "%s\t" (Node.name (Net.Topology.vertex_to_label topo j));
      Printf.printf "%B\t" v;
      Printf.printf "%d\t" np;
      Printf.printf "%d\n" (List.length pathlist);
      acc_j)
    topo [])
  topo []


let get_random_path (i:Topology.vertex) (j:Topology.vertex) (topo:topology)
      (numpath: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t) =
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
  !e_path


(**************************************************************)
(* k-shortest paths *)
(**************************************************************)

let k_shortest_path (topo:topology) (s:Topology.vertex) (t:Topology.vertex)
      (k:int) : path list =
  if s = t then []
  else
    let paths = ref [] in
    let count = Hashtbl.Poly.create () in
    Topology.iter_vertexes
      (fun u -> Hashtbl.Poly.add_exn count u 0;) topo;

    let bheap = (* store paths, priority = cost of path *)
      PQueue.create
        ~min_size:(Topology.num_vertexes topo)
        ~cmp:(fun (dist1,_) (dist2,_) -> compare dist1 dist2) () in

    let _ = PQueue.add_removable bheap (0.0, [s]) in
    let rec explore () =
      let cost_path_u = PQueue.pop bheap in
      match cost_path_u with
      | None -> ()
      | Some (cost_u, path_u) ->
        match List.hd path_u with (* path_u contains vertices in reverse order *)
        | None -> ()
        | Some u ->
          let count_u = Hashtbl.Poly.find_exn count u in
          let _  = Hashtbl.Poly.set count u (count_u + 1) in
          if u = t then paths := List.append !paths [path_u];
          if count_u < k then
            (* if u = t then explore()
               else *)
            let _ =
              Topology.iter_succ
                (fun edge ->
                   let (v,_) = Topology.edge_dst edge in
                   if !Kulfi_Globals.deloop && (List.mem path_u v ~equal:(=)) then
                     ()
                   else (* consider only simple paths *)
                     let path_v = v::path_u in
                     let weight = Link.weight (Topology.edge_to_label topo edge) in
                     let cost_v = cost_u +. weight in
                     let _ = PQueue.add_removable bheap (cost_v, path_v) in
                     ()) topo u in
            explore ()
          else if u = t then ()
          else explore () in
    explore ();

    (* Convert paths from list of nodes to list of edges *)
    List.fold_left !paths ~init:[]
      ~f:(fun acc path ->
        let edge_path =
          List.fold_left path ~init:([], None)
            ~f:(fun edges_u v ->
              let edges, u = edges_u in
              match u with
              | None ->
                (edges, Some v)
              | Some u ->
                let edge = Topology.find_edge topo v u in
                (edge::edges, Some v)) in
        let p,_ = edge_path in
        let p' =
          if !Kulfi_Globals.deloop then
            Kulfi_Frt.FRT.remove_cycles p
          else p in
        p'::acc)
(* end k-shortest path *)

let all_pair_k_shortest_path (topo:topology) (k:int) hosts =
  VertexSet.fold hosts ~init:SrcDstMap.empty
    ~f:(fun acc src ->
      VertexSet.fold hosts ~init:acc
        ~f:(fun acc dst ->
          let ksp = k_shortest_path topo src dst k in
          SrcDstMap.add acc ~key:(src, dst) ~data:ksp))
