open Core
open Frenetic_Network
open Net
open Kulfi_Types
open Kulfi_Util

(************ Helpful functions for simulator *****************)

(* Return src and dst for a given path (edge list) *)
let get_src_dst_for_path (p:path) =
  if p = [] then None
  else
    let src,_ = List.hd_exn p
                |> Net.Topology.edge_src in
    let dst,_ = list_last p
                |> Net.Topology.edge_dst in
    Some (src, dst)

(* Return src and dst for a given path (edge array) *)
let get_src_dst_for_path_arr (p:edge Array.t) =
  if Array.length p = 0 then None
  else
    let src,_ = Net.Topology.edge_src p.(0) in
    let dst,_ = Net.Topology.edge_dst p.((Array.length p)-1) in
    Some (src, dst)


(* Capacity of a link in a given failure scenario *)
let curr_capacity_of_edge (topo:topology) (link:edge) (fail:failure) : float =
  if EdgeSet.mem fail link then 0.
  else capacity_of_edge topo link

(* For a given scheme, find the number of paths through each edge *)
let count_paths_through_edge (s:scheme) : (int EdgeMap.t) =
  SrcDstMap.fold s
  ~init:EdgeMap.empty
  ~f:(fun ~key:_ ~data:ppm acc ->
    PathMap.fold ppm
    ~init:acc
    ~f:(fun ~key:path ~data:_ acc ->
      List.fold_left path
      ~init:acc
      ~f:(fun acc edge ->
        let c = match EdgeMap.find acc edge with
                | None -> 0
                | Some x -> x in
        EdgeMap.add ~key:edge ~data:(c+1) acc)))



(****************** Common metric computation **********************)

(* Compute scheduled load on links ... can be > 1 *)
let congestion_of_paths (t:topology) (d:demands) (s:scheme) : (float EdgeMap.t) =
  let sent_on_each_edge =
    SrcDstMap.fold s
      ~init:EdgeMap.empty
      ~f:(fun ~key:(src,dst) ~data:paths acc ->
          PathMap.fold paths
            ~init:acc
            ~f:(fun ~key:path ~data:prob acc ->
                List.fold_left path
                  ~init:acc
                  ~f:(fun acc e ->
                      let demand =
                        match SrcDstMap.find d (src,dst) with
                        | None -> 0.0
                        | Some x -> x in
                      match EdgeMap.find acc e with
                      | None ->
                          EdgeMap.add ~key:e ~data:(demand *. prob) acc
                      | Some x ->
                          EdgeMap.add ~key:e ~data:((demand *. prob) +. x) acc))) in
  EdgeMap.fold
    ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:amount_sent acc ->
        EdgeMap.add
          ~key:e
          ~data:(amount_sent /. (capacity_of_edge t e))
          acc) sent_on_each_edge


let progress_bar x y l =
  "[" ^ (String.make (x*l/y) '#') ^ (String.make (l-1-x*l/y) ' ') ^ "]"
