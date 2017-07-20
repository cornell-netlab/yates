open Core
open Frenetic_Network
open Net
open Net.Topology

open Kulfi_Types
open Kulfi_Util

module PQueue = Core_kernel.Heap.Removable

let initialize _ = ()

let local_recovery = normalization_recovery

(* Find a constrained shortest path from src to dst which has at least `bw`
   unreserved bandwidth *)
let constrained_shortest_path (topo:topology) (avail_bw:float EdgeMap.t) src dst bw =
  Printf.printf "\n%s -> %s\n"
                (Node.name (Net.Topology.vertex_to_label topo src))
                (Node.name (Net.Topology.vertex_to_label topo dst));

  if src = dst then [[]]
  else
    let prev_table = Hashtbl.Poly.create () in
    let dist_table = Hashtbl.Poly.create () in
    let pq_tokens = Hashtbl.Poly.create () in
    let p_queue = PQueue.create
                    ~min_size:(Topology.num_vertexes topo)
                    ~cmp:(fun (dist1,_) (dist2,_) -> compare dist1 dist2) () in
    Topology.iter_vertexes
      (fun v ->
         let dist = if v = src then 0. else Float.infinity in
         let pq_token = PQueue.add_removable p_queue (dist, v) in
         Hashtbl.Poly.add_exn dist_table v dist;
         Hashtbl.Poly.add_exn pq_tokens v pq_token) topo;

    (* Modified Dijkstra's algorithm to compute all shortest paths that satisfy
       the bandwidth constraints *)
    let rec find_paths () =
      let closest_opt = PQueue.pop p_queue in
      match closest_opt with
      | None -> ()
      | Some (dist, vert) ->
        let () =
          Topology.iter_succ
            (fun edge ->
               let edge_avail_bw = EdgeMap.find_exn avail_bw edge in
               (* Consider an edge only if it has enough available
                  bandwidth *)
               Printf.printf "%f %f\n" (edge_avail_bw/.1e9) (bw/.1e9);
               if edge_avail_bw >= bw then
                 let weight = Link.weight (Topology.edge_to_label topo edge) in
                 let new_dist = dist +. weight in
                 let (neighbor, _) = Topology.edge_dst edge in
                 Printf.printf "%f %f %f %f %s %s\n" new_dist weight
                   (edge_avail_bw/.1e9) (bw/.1e9)
                   (Node.name (Net.Topology.vertex_to_label topo vert))
                   (Node.name (Net.Topology.vertex_to_label topo neighbor));
                 let old_dist = Hashtbl.Poly.find_exn dist_table neighbor in
                 (* update paths if needed *)
                 if new_dist < old_dist then
                   (* found shorter path,
                      update distance and replace old paths *)
                   let pq_token = Hashtbl.Poly.find_exn pq_tokens neighbor in
                   let new_pq_token = PQueue.update p_queue pq_token
                                        (new_dist, neighbor) in
                   (Hashtbl.Poly.set pq_tokens neighbor new_pq_token;
                    Hashtbl.Poly.set dist_table neighbor new_dist;
                    Hashtbl.Poly.set prev_table neighbor [vert])
                 else if new_dist = old_dist && new_dist < Float.infinity then
                   (* found equally short path, add it to existing paths *)
                   let old_prevs =
                     if Hashtbl.Poly.mem prev_table neighbor then
                       Hashtbl.Poly.find_exn prev_table neighbor
                     else [] in
                   Printf.printf "%f = %f\n" new_dist old_dist;
                   Hashtbl.Poly.set prev_table neighbor (vert::old_prevs)
                 else ()
               else ()) topo vert in
        find_paths () in

    find_paths ();

    let memo_table = Hashtbl.Poly.create () in

    (* Obtain all shortest paths from src to dst. Note that, while the edges
       themselves are properly oriented, the list is in reverse order. *)
    let rec get_paths dst =
      if src = dst then [[]]
      else if Hashtbl.Poly.mem memo_table dst then
        Hashtbl.Poly.find_exn memo_table dst
      else
        (* For each predecessor pred of dst, find all src -> pred paths
           Extend each of these paths with edge (pred, dst). *)
        let preds = if Hashtbl.Poly.mem prev_table dst
          then Hashtbl.Poly.find_exn prev_table dst
          else [] in
        let all_paths =
          List.fold_left preds
            ~init:[]
            ~f:(fun acc p ->
              Printf.printf "Pred(%s) = %s "
                (Node.name (Net.Topology.vertex_to_label topo dst))
                (Node.name (Net.Topology.vertex_to_label topo p));
              let prev_paths = get_paths p in
              let new_paths =
                List.map prev_paths
                  ~f:(fun l ->
                    let edge = Topology.find_edge topo p dst in
                    edge::l) in
              new_paths @ acc) in
        (Hashtbl.Poly.add_exn memo_table dst all_paths; all_paths) in

    get_paths dst
    |> List.map ~f:List.rev

let tie_break_random (paths: path List.t) =
    List.nth_exn paths (Random.int (List.length paths))

let reserve_bw avail_bw path bw =
  List.fold path ~init:avail_bw ~f:(fun acc e ->
    let prev_av_bw = EdgeMap.find_exn acc e in
    EdgeMap.add ~key:e ~data:(prev_av_bw -. bw) acc)

let solve (topo:topology) (pairs:demands) : scheme =
  let budget = (min !Kulfi_Globals.budget 100) in
  let per_lsp_prob = 1. /. (float_of_int budget) in
  (* Initialize available bandwidths as link capacities *)
  let avail_bw = Topology.fold_edges
                   (fun edge acc ->
                      let cap = capacity_of_edge topo edge in
                      EdgeMap.add ~key:edge ~data:cap acc) topo EdgeMap.empty in

  (* Assigning LSPs src-dst pairs one src-dst pair at a time is inefficient in
     terms of bin-packing. So assign LSPs for src-dst pairs in round-robin
     fashion. *)
  let (_, routes) =
    List.fold (range 0 budget)
      ~init:(avail_bw, SrcDstMap.empty)
      ~f:(fun (avail_bw, routes) i ->
        SrcDstMap.fold
          ~init:(avail_bw, routes)
          ~f:(fun ~key:(u,v) ~data:d (avail_bw, routes) ->
            let per_lsp_demand = d /. (float_of_int budget) in
            let uv_path =
              constrained_shortest_path topo avail_bw u v per_lsp_demand
              |> tie_break_random in
            (* TODO: Handle the case when demands are feasible but cSPF is
               unable to find paths due to inefficient bin-packing *)
            let avail_bw = reserve_bw avail_bw uv_path per_lsp_demand in
            let prev_pp_map = match SrcDstMap.find routes (u, v) with
              | Some x -> x
              | None -> PathMap.empty in
            let pp_map =
              add_or_increment_path prev_pp_map uv_path per_lsp_prob in
            (avail_bw,
             SrcDstMap.add ~key:(u,v) ~data:(pp_map) routes)) pairs) in
  routes
