open Core

open Kulfi_Apsp
open Kulfi_Types
open Kulfi_Util

module PQueue = Core_kernel.Heap

let prev_scheme = ref SrcDstMap.empty

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = normalization_recovery

(* Keep headroom for unexpected traffic *)
let max_util_cap = 0.8

(* Threshold to tolerate over max utilization *)
let max_thresh = 1.025

(***********************************************)
(* Tie-break algorithms *)
(* Configuring CSPF Tie Breaking - Juniper *)
(* [http://www.juniper.net/documentation/en_US/junos/topics/usage-guidelines/mpls-configuring-cspf-tie-breaking.html] *)
(***********************************************)
module type Tiebreaker = sig
  val tie_break : (float EdgeMap.t * topology) -> (path List.t) -> path option
end

module RandomTB : Tiebreaker = struct
  let tie_break (_) = function
    | [] -> None
    | paths ->
      Some (List.nth_exn paths (Random.int (List.length paths)))
end

module LeastFillTB : Tiebreaker = struct
  let tie_break (available_bw, topo) = function
    | [] -> None
    | paths ->
      let _,least_fill_path =
        List.fold_left paths ~init:(Float.infinity, [])
          ~f:(fun (acc_fill, acc_path) path ->
            let max_util =
              List.fold path ~init:0.0
                ~f:(fun acc e ->
                  let avail_cap = EdgeMap.find_exn available_bw e in
                  let cap = capacity_of_edge topo e in
                  let util = (cap -. avail_cap) /. cap in
                  max acc util) in
            if max_util < acc_fill then
              (max_util, path)
            else
              (acc_fill, acc_path)) in
      Some least_fill_path
end

module MostFillTB : Tiebreaker = struct
  let tie_break (available_bw, topo) = function
    | [] -> None
    | paths ->
      let _,most_fill_path =
        List.fold_left paths ~init:(Float.neg_infinity, [])
          ~f:(fun (acc_fill, acc_path) path ->
            let max_util =
              List.fold path ~init:0.0
                ~f:(fun acc e ->
                  let avail_cap = EdgeMap.find_exn available_bw e in
                  let cap = capacity_of_edge topo e in
                  let util = (cap -. avail_cap) /. cap in
                  max acc util) in
            if max_util < acc_fill then
              (acc_fill, acc_path)
            else
              (max_util, path)) in
      Some most_fill_path
end

(* select the paths with the fewest number of hops *)
let least_hop_paths = function
  | [] -> []
  | paths ->
    let _,mh_paths =
      List.fold_left paths ~init:(Int.max_value, [])
        ~f:(fun (acc_nhop, acc_paths) path ->
          let nhops = List.length path in
          if nhops < acc_nhop then
            (nhops, [path])
          else if nhops = acc_nhop then
            (nhops, path::acc_paths)
          else
            (acc_nhop, acc_paths)) in
    mh_paths

(* Find a constrained shortest path from src to dst which has at least `bw`
   unreserved bandwidth *)
let constrained_shortest_path (full_topo:topology) (avail_bw:float EdgeMap.t) src dst bw =
  if src = dst then [[]]
  else
    (* Remove other hosts to prevent routing through them *)
    let hosts = get_hosts_set full_topo in
    let topo = VertexSet.fold hosts ~init:full_topo
      ~f:(fun acc h ->
          if src = h || dst = h then acc
          else Topology.remove_vertex acc h) in
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
               (* Printf.printf "%f %f\n" (edge_avail_bw/.1e9) (bw/.1e9); *)
               if edge_avail_bw >= bw then
                 let weight = Link.weight (Topology.edge_to_label topo edge) in
                 let new_dist = dist +. weight in
                 let (neighbor, _) = Topology.edge_dst edge in
                 (* Printf.printf "%f %f %f %f %s %s\n" new_dist weight *)
                 (* (edge_avail_bw/.1e9) (bw/.1e9) *)
                 (* (Node.name (Topology.vertex_to_label topo vert)) *)
                 (* (Node.name (Topology.vertex_to_label topo neighbor)); *)
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
                   (* Printf.printf "%f = %f\n" new_dist old_dist; *)
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
              (* Printf.printf "Pred(%s) = %s " *)
              (* (Node.name (Topology.vertex_to_label topo dst)) *)
              (* (Node.name (Topology.vertex_to_label topo p)); *)
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

let reserve_bw avail_bw path bw =
  List.fold path ~init:avail_bw ~f:(fun acc e ->
    let prev_av_bw = EdgeMap.find_exn acc e in
    EdgeMap.set ~key:e ~data:(prev_av_bw -. bw) acc)

let solve (topo:topology) (pairs:demands) : scheme =
  let new_scheme =
    let cmax =
      if (SrcDstMap.is_empty !prev_scheme) then
        Float.infinity
      else
        congestion_of_paths topo pairs !prev_scheme
        |> EdgeMap.to_alist
        |> List.map ~f:snd
        |> get_max_congestion in
    (* Printf.printf "\n%f\n" cmax; *)
    if cmax < max_thresh *. max_util_cap then
      !prev_scheme
    else
      (* Recompute cSPF paths only if previous routing scheme would lead to
         exceeding the maximum allowed utilization *)
      let budget = (min !Kulfi_Globals.budget 100) in
      let per_lsp_prob = 1. /. (float_of_int budget) in
      (* Initialize available bandwidths as link capacities *)
      let avail_bw = Topology.fold_edges
          (fun edge acc ->
             let cap = capacity_of_edge topo edge in
             EdgeMap.set ~key:edge ~data:(max_util_cap *. cap) acc)
          topo EdgeMap.empty in

      (* Assign LSPs for src-dst pairs in round-robin order. *)
      (* How CSPF Selects a Path - Juniper *)
      (* [http://www.juniper.net/documentation/en_US/junos/topics/concept/mpls-cspf-path-selection-method.html] *)
      let (_, routes) =
        List.fold (range 0 budget)
          ~init:(avail_bw, SrcDstMap.empty)
          ~f:(fun (avail_bw, routes) i ->
            SrcDstMap.fold
              ~init:(avail_bw, routes)
              ~f:(fun ~key:(u,v) ~data:d (avail_bw, routes) ->
                let per_lsp_demand = d *. per_lsp_prob in
                let uv_path_opt =
                  constrained_shortest_path topo avail_bw u v per_lsp_demand
                  |> least_hop_paths
                  |> RandomTB.tie_break (avail_bw, topo) in
                match uv_path_opt with
                | Some uv_path ->
                  let avail_bw = reserve_bw avail_bw uv_path per_lsp_demand in
                  let prev_pp_map = match SrcDstMap.find routes (u, v) with
                    | Some x -> x
                    | None -> PathMap.empty in
                  let pp_map =
                    add_or_increment_path prev_pp_map uv_path per_lsp_prob in
                  (avail_bw,
                   SrcDstMap.set ~key:(u,v) ~data:(pp_map) routes)
                | None ->
                  (* Couldn't find an LSP. Normalize remaining paths later. *)
                  (avail_bw, routes)) pairs) in
      let full_routes =
        (* Fallback options if cSPF couldn't find paths *)
        SrcDstMap.fold pairs ~init:routes
          ~f:(fun ~key:(src,dst) ~data:_ acc ->
            match SrcDstMap.find routes (src,dst) with
            | Some _ -> acc
            | None ->
              (* If we couldn't find a single LSP for a pair *)
              begin
                (* Try using paths already computed in the previous iteration *)
                match SrcDstMap.find !prev_scheme (src,dst) with
                | Some pp_map ->
                  SrcDstMap.set ~key:(src,dst) ~data:pp_map acc
                | None ->
                  (* Last resort to find paths *)
                  let paths = k_shortest_paths topo src dst budget in
                  let prob = 1.0 /. Float.of_int (List.length paths) in
                  let pp_map =
                    List.fold_left paths ~init:PathMap.empty
                      ~f:(fun acc path ->
                        PathMap.set acc ~key:path ~data:prob) in
                  SrcDstMap.set ~key:(src,dst) ~data:pp_map acc
              end) in
      normalize_scheme full_routes in
  prev_scheme := new_scheme;
  new_scheme
