open Core

open Simulation_Types
open Yates_types.Types
open Yates_routing.Util

(**************************************************************)
(* Helper functions for simulator *)
(**************************************************************)
let solver_to_string (s:solver_type) : string =
  match s with
  | Ac -> "ac"
  | AkEcmp -> "akecmp"
  | AkKsp -> "akksp"
  | AkMcf -> "akmcf"
  | AkRaeke -> "akraeke"
  | AkVlb -> "akvlb"
  | Cspf -> "cspf"
  | Ecmp -> "ecmp"
  | Edksp -> "edksp"
  | Ffc -> "ffc"
  | Ffced -> "ffced"
  | Ksp -> "ksp"
  | Mcf -> "mcf"
  | MwMcf -> "mwmcf"
  | Raeke -> "raeke"
  | SemiMcfAc -> "semimcfac"
  | SemiMcfEcmp -> "semimcfecmp"
  | SemiMcfEdksp -> "semimcfedksp"
  | SemiMcfKsp -> "semimcfksp"
  | SemiMcfKspFT -> "semimcfkspft"
  | SemiMcfMcf -> "semimcfmcf"
  | SemiMcfMcfEnv -> "semimcfmcfenv"
  | SemiMcfMcfFTEnv -> "semimcfmcfftenv"
  | SemiMcfRaeke -> "semimcfraeke"
  | SemiMcfRaekeFT -> "semimcfraekeft"
  | SemiMcfVlb -> "semimcfvlb"
  | Spf -> "spf"
  | Vlb -> "vlb"
  | OptimalMcf -> "optimalmcf"

let solver_to_description (s:solver_type) : string =
  match s with
  | Ac -> "Applegate-Cohen's optimal oblivious TE"
  | AkEcmp -> "ECMP for path selection + multiplicative weights for rate adaptation"
  | AkKsp -> "k-shortest paths for path selection + multiplicative weights for rate adaptation"
  | AkMcf -> "multi-commodity flow for path selection + multiplicative weights for rate adaptation"
  | AkRaeke -> "Raecke's oblivious routing for path selection + multiplicative weights for rate adaptation"
  | AkVlb -> "Valiant load balancing for path selection + multiplicative weights for rate adaptation"
  | Cspf -> "Constrained Shortest Paths First"
  | Ecmp -> "Equal-Cost Multi Path"
  | Edksp -> "Edge-disjoint k-shortest paths"
  | Ffc -> "Forward Fault Correction (FFC) with k-shortest paths"
  | Ffced -> "Forward Fault Correction (FFC) with edge-disjoing k-shortest paths"
  | Ksp -> "k-shortest paths"
  | Mcf -> "multi-commodity flow that minimizes max. link utilization"
  | MwMcf -> "multi-commodity flow using multiplicative weights"
  | Raeke -> "Raecke's oblivious routing"
  | SemiMcfAc -> "Applegate-Cohen's oblivious routing for path selection + restricted MCF for rate adaptation"
  | SemiMcfEcmp -> "ECMP for path selection + restricted MCF for rate adaptation"
  | SemiMcfEdksp -> "Edge-disjoint k-shortest paths for path selection + restricted MCF for rate adaptation"
  | SemiMcfKsp -> "k-shortest paths for path selection + restricted MCF for rate adaptation"
  | SemiMcfKspFT -> "k-shortest paths with fault tolerance for path selection + restricted MCF for rate adaptation"
  | SemiMcfMcf -> "MCF for path selection + restricted MCF for rate adaptation"
  | SemiMcfMcfEnv -> "MCF with demand envelope for path selection + restricted MCF for rate adaptation"
  | SemiMcfMcfFTEnv -> "MCF with demand envelope and fault tolerance for path selection + restricted MCF for rate adaptation"
  | SemiMcfRaeke -> "Raecke's oblivious routing for path selection + restricted MCF for rate adaptation"
  | SemiMcfRaekeFT -> "Raecke's oblivious routing with fault tolerance for path selection + restricted MCF for rate adaptation"
  | SemiMcfVlb -> "Valiant load balancing for path selection + restricted MCF for rate adaptation"
  | Spf -> "Shortest paths first"
  | Vlb -> "Valiant Load Balancing"
  | OptimalMcf -> "Optimal multi-commodity flow based TE which doesn't have any operational constraints"


let select_algorithm solver = match solver with
  | Ac -> Yates_routing.AC.solve
  | AkEcmp
  | AkKsp
  | AkMcf
  | AkRaeke
  | AkVlb -> Yates_routing.Ak.solve
  | Cspf -> Yates_routing.Cspf.solve
  | Ecmp -> Yates_routing.Ecmp.solve
  | Edksp -> Yates_routing.Edksp.solve
  | Ffc
  | Ffced -> Yates_routing.Ffc.solve
  | Ksp -> Yates_routing.Ksp.solve
  | Mcf -> Yates_routing.Mcf.solve
  | MwMcf -> Yates_routing.MwMcf.solve
  | OptimalMcf -> Yates_routing.Mcf.solve
  | Raeke -> Yates_routing.Raeke.solve
  | SemiMcfAc
  | SemiMcfEcmp
  | SemiMcfEdksp
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfVlb -> Yates_routing.SemiMcf.solve
  | Spf -> Yates_routing.Spf.solve
  | Vlb -> Yates_routing.Vlb.solve

let select_local_recovery solver = match solver with
  | Ac -> Yates_routing.AC.local_recovery
  | AkEcmp
  | AkKsp
  | AkMcf
  | AkRaeke
  | AkVlb -> Yates_routing.Ak.local_recovery
  | Cspf -> Yates_routing.Cspf.local_recovery
  | Ecmp -> Yates_routing.Ecmp.local_recovery
  | Edksp -> Yates_routing.Edksp.local_recovery
  | Ffc
  | Ffced -> Yates_routing.Ffc.local_recovery
  | Ksp -> Yates_routing.Ksp.local_recovery
  | Mcf -> Yates_routing.Mcf.local_recovery
  | MwMcf -> Yates_routing.MwMcf.local_recovery
  | OptimalMcf -> failwith "No local recovery for optimal mcf"
  | Raeke -> Yates_routing.Raeke.local_recovery
  | SemiMcfAc
  | SemiMcfEcmp
  | SemiMcfEdksp
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfVlb -> Yates_routing.SemiMcf.local_recovery
  | Spf -> Yates_routing.Spf.local_recovery
  | Vlb -> Yates_routing.Vlb.local_recovery

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
