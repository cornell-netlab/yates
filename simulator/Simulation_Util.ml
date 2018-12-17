open Core

open Simulation_Types
open Yates_types.Types
open Yates_routing.Util
open Yates_solvers.Solvers

(**************************************************************)
(* Helper functions for simulator *)
(**************************************************************)
let store_paths log_paths scheme topo out_dir algorithm n : unit =
  if log_paths || n = 0 then
    let _ = match (Sys.file_exists out_dir) with
      | `No -> Unix.mkdir out_dir
      | _ -> () in
    let out_dir = out_dir ^ "paths/" in
    let _ = match (Sys.file_exists out_dir) with
      | `No -> Unix.mkdir out_dir
      | _ -> () in
    let file_name = (solver_to_string algorithm) ^ "_" ^ (string_of_int n) in
    let oc = Out_channel.create (out_dir ^ file_name) in
    fprintf oc "%s\n" (dump_scheme topo scheme);
    Out_channel.close oc
  else ()

(**************************************************************)
(* Topology and routing scheme operations *)
(**************************************************************)

(* Return src and dst for a given path (edge list) *)
let get_src_dst_for_path (p:path) =
  if p = [] then None
  else
    let src,_ = List.hd_exn p
                |> Topology.edge_src in
    let dst,_ = list_last p
                |> Topology.edge_dst in
    Some (src, dst)

(* Return src and dst for a given path (edge array) *)
let get_src_dst_for_path_arr (p:edge Array.t) =
  if Array.length p = 0 then None
  else
    let src,_ = Topology.edge_src p.(0) in
    let dst,_ = Topology.edge_dst p.((Array.length p)-1) in
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
        EdgeMap.set ~key:edge ~data:(c+1) acc)))


let progress_bar x y l =
  "[" ^ (String.make (x*l/y) '#') ^ (String.make (l-1-x*l/y) ' ') ^ "]"

(* Failure handling *)

let validate_files_exist (files: (string * string option) list) : unit =
  List.iter files ~f:(fun (name, file_opt) ->
      match file_opt with
      | Some f -> begin
          match Sys.file_exists f with
          | `Yes -> ()
          | _ -> raise (Not_found_s (sexp_of_string (Printf.sprintf "Input file for %s not found at %s. Please check that the file exists and you have read permissions." name f)))
          end
      | None -> ())
