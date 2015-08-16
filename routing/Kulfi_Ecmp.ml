open Kulfi_Types
open Frenetic_Network
open Net
open Core.Std

module PQueue = Core.Heap.Removable
module VertexSet = Topology.VertexSet
module EdgeSet = Topology.EdgeSet

type traffic_matrix = (Topology.vertex * Topology.vertex * float) list
type congestion = (edge * float) list

type stats = {
  congestions : congestion list;
  num_paths : int list;
  churn : int list
}
               
let make_stats congs paths churn = {
  congestions = congs;
  num_paths = paths;
  churn = churn;
}
                                 
let solve (t:topology) (d:demands) (s:scheme) : scheme =
  assert false

let capacity_of_edge topo edge =
  let open Net.Topology in
  let lbl = edge_to_label topo edge in
  Int64.to_float (Link.capacity lbl)
         
let src_shortest_paths topo hosts src =
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
    
let all_shortest_paths_multi topo hosts =
  let path_table = Hashtbl.Poly.create () in
  VertexSet.iter
    hosts
    ~f:(fun src ->
        let dst_paths_list = src_shortest_paths topo hosts src in
        Hashtbl.Poly.add_exn path_table src dst_paths_list);
  path_table

    
let congestion_of_paths topo (paths : ((edge list) * float) list)
  : congestion =
  let load_table = Hashtbl.Poly.create () in
  List.iter paths (fun (path, wt) ->
      List.iter path (fun edge ->
	  (*
          let inverse = match Net.Topology.inverse_edge topo edge with
            | Some x -> x
            | None -> failwith "No inverse edge" in
	  *)
          let () = match Hashtbl.Poly.find load_table edge with
            | None -> Hashtbl.Poly.add_exn load_table edge wt
            | Some old_wt -> Hashtbl.Poly.set load_table edge (wt +. old_wt) in
	  ()
      (*
          match Hashtbl.Poly.find load_table inverse with
          | None -> Hashtbl.Poly.add_exn load_table inverse wt
          | Some old_wt -> Hashtbl.Poly.set load_table inverse (wt +. old_wt)
      *)
      ));
  Hashtbl.fold load_table ~init:[] ~f:(fun ~key:edge ~data:load acc ->
      let cap = capacity_of_edge topo edge in
      (edge, load /. cap)::acc)
               

    
let find_demand (tm : traffic_matrix) (s : Topology.vertex) (t : Topology.vertex) : float =
  match List.find tm ~f:(fun (u, v, rate) ->
      s = u && t = v) with
  | Some (u, v, rate) -> rate
  | None -> 0.
    
let create topo tag_hash matrices =
  let open Net.Topology in
  (* Get the set of hosts *)
  let host_set = VertexSet.filter (vertexes topo) ~f:(fun v ->
                                                      let lbl = vertex_to_label topo v in
                                                      Node.device lbl = Node.Host) in

  let path_table = all_shortest_paths_multi topo host_set in
  let weighted_hash = Hashtbl.Poly.create () in
  let paths_list, num = Hashtbl.Poly.fold path_table ~init:([], 0)
                                          ~f:(fun ~key:src ~data:dsts_list acc ->
                                              List.fold_left dsts_list ~init:acc ~f:(fun (acc, num) (dst, paths) ->
                                                                                     ((src, dst, paths)::acc, num + List.length paths))) in
  let load_paths tm =
    List.fold_left paths_list ~init:[] ~f:(fun acc (src, dst, paths) ->
                                           let demand = find_demand tm src dst in
                                           let num_paths = List.length paths in
                                           let per_path = demand /. (float num_paths) in
                                           List.fold_left paths ~init:acc ~f:(fun acc2 path ->
                                                                              (path, per_path)::acc2)) in

  let loaded_paths = List.map matrices ~f:load_paths in
  let congestions = List.map loaded_paths ~f:(congestion_of_paths topo) in
  let num_paths = List.map matrices ~f:(fun _ -> num) in

  (* For each source host, continue. *)
  Hashtbl.Poly.iter path_table ~f:(fun ~key:src ~data:dsts_list ->
                                   let src_lbl = vertex_to_label topo src in
                                   let src_addr = Node.ip src_lbl in
                                   let dsts_hash = Hashtbl.Poly.create () in
                                   Hashtbl.Poly.add_exn weighted_hash src_addr dsts_hash;
                                   (* For each destination host, get all of the shortest paths from
           src to dst, assign them an equal weight of 1, and put them in
           the table. *)
                                   List.iter dsts_list ~f:(fun (dst, paths) ->
                                                           if src = dst then () else
                                                             let dst_lbl = vertex_to_label topo dst in
                                                             let dst_addr = Node.ip dst_lbl in
                                                             let weighted_tags = List.map paths ~f:(fun path ->
                                                                                                    let tags = List.map (List.tl_exn path) ~f:(fun edge ->
                                                                                                                                               Hashtbl.Poly.find_exn tag_hash edge) in
                                                                                                    (1, tags)) in
                                                             Hashtbl.Poly.add_exn dsts_hash dst_addr weighted_tags));
  let churn = List.map num_paths ~f:(fun x -> 0) in
  let stats = make_stats congestions num_paths churn in
  ([weighted_hash], stats)
