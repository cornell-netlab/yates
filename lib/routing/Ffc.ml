open Core

open LP_Lang
open Util
open Yates_types.Types

let () = match !Globals.rand_seed with
  | Some x -> Random.init x
  | None -> Random.self_init ~allow_in_tests:true ()

let objective = Var "Z"

(*
 * Basic constrains are slightly different compared to SemiMCF
 * MCF tries to minimize congestion. So, it miminimzes Z where traffic on an edge <= Z * capacity
 * FFC tries to maximize throughput Z = sum of granted bandwidths
 * Refer equation numbers from SIGCOMM 2014 FFC paper
 * *)
let state_base_path_set = ref SrcDstMap.empty

let var_name_uid id =
  Printf.sprintf "f_%d" id

(* Equation (1) :
 * Throughput Z = total granted bandwidth *)
let objective_fun (topo : Topology.t)
                  (d_pairs : demands)
                  (init_acc : constrain list) : constrain list =
  let granted_bw_vars = SrcDstMap.fold ~init:[]
    ~f:(fun ~key:(src,dst) ~data:(_) acc ->
      let granted_bw_var = granted_bw_var_name topo (src,dst) in
      (Var(granted_bw_var)) :: acc) d_pairs in
  let throughput = Sum (granted_bw_vars) in
  let diff_tput_obj = minus throughput objective in
  let name = "tput" in
  (Eq (name, diff_tput_obj, 0.))::init_acc

(* Equation (2) :
 * Total traffic on an edge <= capacity of edge *)
let capacity_constraints (pmap : path_uid_map)
                        (emap : edge_uidlist_map)
                        (topo : topology)
                        (d : demands)
                        (init_acc : constrain list) : constrain list =
  (* For every edge, there is a capacity constraint *)
  Topology.fold_edges (fun edge acc ->
    (* The sum of all commodity flows must be less than capacity. *)
    (* Gather all of the terms for each commodity *)
    match (EdgeMap.find emap edge) with
     | None -> acc
     | Some uid_list ->
         let all_flows = List.map ~f:(fun x -> Var(var_name_uid x)) uid_list in
         (* Add them all up *)
         let total_flow = Sum (all_flows) in
         let scaled_cap = ((capacity_of_edge topo edge) /. cap_divisor) in
         (* Total flow is at most the scaled capacity *)
         let name = Printf.sprintf "cap_%s"
                    (string_of_edge topo edge) in
         (Leq (name, total_flow, scaled_cap))::acc) topo init_acc

(* Equation (3) :
 * sum of allocated BWs on all tunnels for a flow should be >= BW granted for the flow *)
let grantedbw_constraints (pmap : path_uid_map)
                          (emap : edge_uidlist_map)
                          (topo : topology)
                          (d : demands)
                          (base_path_set : (path List.t) SrcDstMap.t)
                          (init_acc : constrain list) : constrain list =
    SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:(demand) acc ->
      if (src = dst) then acc else
      (* We need to add up the rates for all paths in pmap(src,dst) *)
      let granted_bw_var = granted_bw_var_name topo (src,dst) in
      let name = Printf.sprintf "grant-%s-%s" (name_of_vertex topo src)
              (name_of_vertex topo dst) in
      match (SrcDstMap.find base_path_set (src,dst)) with
        | None -> (* Grant 0 BW if no tunnel *)
            (Eq (name, (Var (granted_bw_var)), 0.))::acc
        | Some path_list ->
            if List.is_empty path_list then (Eq (name, (Var (granted_bw_var)), 0.))::acc else
            let all_flows =  List.fold_left ~init:[] ~f:(fun acc p ->
                let pvar = match (PathMap.find pmap p) with
                  | None -> assert false
                  | Some id -> Var(var_name_uid id) in
                pvar::acc ) path_list in
            (* some code to generate a constraint *)
            let total_allocated = Sum (all_flows) in
            let diff_allocated_granted = minus total_allocated (Var (granted_bw_var)) in
            (Geq (name, diff_allocated_granted, 0.))::acc) d


(* Equation (4) :
 * BW granted for a flow <= demand for the flow *)
let demand_constraints (topo : Topology.t)
                      (d_pairs : demands)
                      (init_acc : constrain list) : constrain list =
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:(demand) acc ->
      let granted_bw_var = granted_bw_var_name topo (src,dst) in
      let name = Printf.sprintf "dem-%s-%s" (name_of_vertex topo src)
          (name_of_vertex topo dst) in
      (* BW granted <= demand *)
      (Leq (name, (Var (granted_bw_var)), (demand /. demand_divisor)))::acc) d_pairs


(* Equation (9)
 * Consider every possible *link* failure and add a bandwidth constraint *)
let data_plane_fault_constraints (pmap : path_uid_map)
                (emap : edge_uidlist_map)
                (topo : topology)
                (d : demands)
                (base_path_set : (path List.t) SrcDstMap.t)
                (init_acc : constrain list) : constrain list =
  (* generate all possible failures of up to k_e links *)
  let failures_list = List.fold_left (range 0 !Globals.ffc_max_link_failures)
    ~init:[] ~f:(fun acc i ->
      get_all_possible_failures topo (i+1)
      |> List.append acc) in

  (* fold over all failures, add corresponding constraint *)
  List.fold_left failures_list ~init:init_acc
    ~f:(fun acc failure_scen ->
      let residual_path_set = SrcDstMap.fold base_path_set ~init:SrcDstMap.empty
            ~f:(fun ~key:(src,dst) ~data:paths path_acc ->
              (* for each src,dst pair, find the set of paths that are still up *)
              let is_tunnel_up (failed_links) (p:path) : bool =
                List.fold_left p ~init:true ~f:(fun up edge ->
                  let edge_is_safe = not (EdgeSet.mem failed_links edge) in
                  up && edge_is_safe) in
              let residual_paths = List.filter ~f:(is_tunnel_up failure_scen) paths in
              SrcDstMap.set ~key:(src,dst) ~data:residual_paths path_acc) in
      grantedbw_constraints pmap emap topo d residual_path_set acc)



(* Construct linear program *)
let lp_of_ffc_maps (pmap:path_uid_map)
              (emap:edge_uidlist_map)
              (topo:topology)
              (d:demands)
              (base_path_set : (path List.t) SrcDstMap.t) : (arith_exp * constrain list) =
  let all_constrs =
      capacity_constraints pmap emap topo d []
      |> grantedbw_constraints pmap emap topo d base_path_set
      |> data_plane_fault_constraints pmap emap topo d base_path_set
      |> demand_constraints topo d
      |> objective_fun topo d in
  (objective, all_constrs)


(* Generate a LP file with unique name *)
let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "/tmp/ffc_%f.lp" rand) in
  match Sys.file_exists try_fn with
      `Yes -> new_rand ()
       | _ -> rand

(* Solve linear program *)
let solve_ffc_lp (pmap:int PathMap.t) (emap:int list EdgeMap.t) (topo:topology)
    (d:demands) (base_path_set : (path List.t) SrcDstMap.t) : (float * (int * float) list) =
  let lp = lp_of_ffc_maps pmap emap topo d base_path_set in
  let rand = new_rand () in
  let lp_filename = (Printf.sprintf "/tmp/ffc_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "/tmp/ffc_%f.sol" rand) in

  (* Serialize LP and call Gurobi *)
  serialize_max_lp lp lp_filename;
  call_gurobi lp_filename lp_solname;

  (* read back all the edge flows from the .sol file *)
  let ratio, flows = In_channel.with_file lp_solname
      ~f:(fun file ->
          let result_str = "^f_\\([a-zA-Z0-9]+\\) \\([0-9.e+-]+\\)$" in
          let regex = Str.regexp result_str in
          In_channel.fold_lines
            file
            ~init:(0.,[])
            ~f:(fun (opt_z,flows) line ->
                match line with
                | "" -> (opt_z,flows)
                | _ ->
                  if line.[0] = '#' then (opt_z, flows)
                  else if line.[0] = 'Z' then
                    let ratio_str = Str.string_after line 2 in
                    let ratio = Float.of_string ratio_str in
                    (ratio *. demand_divisor /. cap_divisor, flows)
                  else
                  if (Str.string_match regex line 0)
                  then
                    let id = Int.of_string (Str.matched_group 1 line) in
                    let flow_amt = Float.of_string (Str.matched_group 2 line) in
                    (* Printf.printf "%d %f\n" id flow_amt;  *)
                    let tup = (id, flow_amt) in
                    (opt_z, tup::flows)
                  else
                    (opt_z, flows))) in
  ignore (Sys.remove lp_filename);
  ignore (Sys.remove lp_solname);
  (ratio,flows)

let initialize (s:scheme) : unit =
  (* compute the base path set by ignoring associated path probs in scheme *)
  ignore (if (SrcDstMap.is_empty s) then failwith "FFC must be initialized with a non-empty scheme" else ());
  let b = SrcDstMap.fold s
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(u,v) ~data:pp_map acc ->
       SrcDstMap.set ~key:(u,v) ~data:(PathMap.keys pp_map) acc) in
  state_base_path_set := b;
  ()

let ffc_mcf (topo:topology) (d:demands) (base_path_set : (path List.t) SrcDstMap.t) : scheme =
  let uuid = ref (-1) in
  let fresh_uid () =
    uuid := !uuid + 1;
    !uuid
  in

  let (umap,pmap,emap) =
    SrcDstMap.fold
      base_path_set (* fold over the base path set *)
      ~init:(UidMap.empty, PathMap.empty, EdgeMap.empty)
      (* for every pair of hosts u,v *)
      ~f:(fun ~key:(u,v) ~data:path_list acc ->
          if (u = v) then acc
          else
            begin
              (* get the possible paths, and for every path *)
              List.fold_left path_list ~init:acc ~f:(fun (umap,pmap,emap) path ->
                  let id = fresh_uid () in
                  (*Printf.printf "\npath %d\t%d : " id (List.length path);*)
                  let umap' = UidMap.set ~key:id ~data:(u,v,path) umap in
                  let pmap' = PathMap.set ~key:path ~data:id pmap in
                  (* get the edges in the path *)
                  (* This assertion fails because we have some paths with no edges *)
                  assert (not (List.is_empty path));
                  let emap' = List.fold_left path ~init:emap ~f:(fun emap e ->
                      let ids = match (EdgeMap.find emap e ) with
                        | None -> [id]
                        | Some ids -> id::ids in
                      (*Printf.printf "%s " (string_of_edge topo e) ;*)
                      EdgeMap.set ~key:e ~data:ids emap) in
                  (umap',pmap',emap'))
            end) in

  assert (not (EdgeMap.is_empty emap));
  let (ratio, flows) =  solve_ffc_lp pmap emap topo d base_path_set in
  let (unnormalized_scheme, flow_sum) = SemiMcf.scheme_and_flows flows umap in
  let new_scheme = SemiMcf.normalize unnormalized_scheme flow_sum in
  if (SrcDstMap.is_empty new_scheme) then assert false;
  new_scheme

let solve (topo:topology) (d:demands) : scheme =
  ignore (if (SrcDstMap.is_empty !state_base_path_set) then failwith "FFC: base path set empty!" else ());
  let base_path_set = !state_base_path_set in
  ffc_mcf topo d base_path_set

let local_recovery = normalization_recovery
