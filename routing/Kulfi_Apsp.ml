open Kulfi_Types
open Core.Std
open Frenetic_Network                         
open Net
open Net.Topology

module PQueue = Core.Heap.Removable
       
(* TODO(jnf,rjs): We don't really understand the details of this
code. But we're trusting that Chris Yu wrote a correct implementation
of all pairs all shortest paths. The solve function treats
all_shortest_paths_multi as a black box. *)

let src_shortest_paths (topo:topology) hosts src =
  let prev_table = Hashtbl.Poly.create () in
  let dist_table = Hashtbl.Poly.create () in
  let tokens = Hashtbl.Poly.create () in
  let p_queue = PQueue.create
                  ~min_size:(Topology.num_vertexes topo)
                  ~cmp:(fun (dist1,_) (dist2,_) -> compare dist1 dist2) () in
  Topology.iter_vertexes
    (fun v ->
     let dist = if v = src then 0. else Float.infinity in
     let token = PQueue.add_removable p_queue (dist, v) in
     Hashtbl.Poly.add_exn dist_table v dist;
     Hashtbl.Poly.add_exn tokens v token) topo;
  
  (* Modified Dijkstra's that saves all shortest paths of equal length *)
  let rec extend_paths () =
    let closest_opt = PQueue.pop p_queue in
    match closest_opt with
    | None -> ()
    | Some (dist, vert) ->
       let () = Topology.iter_succ
                  (fun edge ->
                   let (neighbor, _) = Topology.edge_dst edge in
                   let weight = Link.weight (Topology.edge_to_label topo edge) in
                   let new_dist = dist +. weight in
                   let old_dist = Hashtbl.Poly.find_exn dist_table neighbor in
                   (* update paths if needed *)
                   if new_dist < old_dist then
                     (* found shorter path, update distance and replace old paths *)
                     let token = Hashtbl.Poly.find_exn tokens neighbor in
                     let new_token = PQueue.update p_queue token (new_dist, neighbor) in
                     (Hashtbl.Poly.set tokens neighbor new_token;
                      Hashtbl.Poly.set dist_table neighbor new_dist;
                      Hashtbl.Poly.set prev_table neighbor [vert])
                   else if new_dist = old_dist then
                     (* found equally short path, add it to existing paths *)
                     let old_prevs =
                       if Hashtbl.Poly.mem prev_table neighbor then
                         Hashtbl.Poly.find_exn prev_table neighbor
                       else [] in
                     Hashtbl.Poly.set prev_table neighbor (vert::old_prevs)
                   else ()) topo vert in
       extend_paths () in

  extend_paths ();

  let memo_table = Hashtbl.Poly.create () in

  (* Obtain all shortest paths from src to dst. Note that, while the edges
     themselves are properly oriented, the list is in reverse order. *)
  let rec get_paths dst =
    if src = dst then [[]]
    else if Hashtbl.Poly.mem memo_table dst then
      Hashtbl.Poly.find_exn memo_table dst
    else
      (* For each predecessor p of dst, find all of the paths from src to p.
         Then take each path and add the edge (p, dst). *)
      let preds = if Hashtbl.Poly.mem prev_table dst
                  then Hashtbl.Poly.find_exn prev_table dst
                  else [] in
      let all_paths = List.fold_left
                        preds
                        ~init:[]
                        ~f:(fun acc p ->
                            let prev_paths = get_paths p in
                            let new_paths = List.map
                                              prev_paths
                                              ~f:(fun l ->
                                                  let edge = Topology.find_edge topo p dst in
                                                  edge::l) in
                            new_paths @ acc) in
      (Hashtbl.Poly.add_exn memo_table dst all_paths; all_paths) in
  VertexSet.fold
    hosts
    ~init:[]
    ~f:(fun acc dst ->
        let revved_paths = get_paths dst in
        let forward_paths = List.map revved_paths ~f:List.rev in
        (dst, forward_paths)::acc)

let all_shortest_paths_multi  (topo:topology) (hosts)  =
  let path_table = Hashtbl.Poly.create () in
  VertexSet.iter
    hosts
    ~f:(fun src ->
        let dst_paths_list = src_shortest_paths topo hosts src in
        Hashtbl.Poly.add_exn path_table src dst_paths_list);
  path_table
