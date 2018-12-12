open Core

open Apsp
open LP_Lang
open Util
open Yates_types.Types

let prev_scheme = ref SrcDstMap.empty

let use_min_cut = true

let () = match !Globals.rand_seed with
  | Some x -> Random.init x
  | None -> Random.self_init ~allow_in_tests:true ()

let objective = Var "Z"

let capacity_constraints (topo : Topology.t) (src : Topology.vertex) (dst : Topology.vertex)
                         (init_acc : constrain list) : constrain list =
  (* For every inter-switch edge, there is a unit capacity constraint *)
  Topology.fold_edges
    (fun edge acc ->
    if edge_connects_switches edge topo then
      let flow_on_edge = Var(var_name topo edge (src,dst)) in
      (* Total flow is at most 1 *)
      let name = Printf.sprintf "cap_%s"
          (string_of_edge topo edge) in
      (Leq (name, flow_on_edge, 1.))::acc
    else acc) topo init_acc

let num_path_constraints (topo : Topology.t) (src : Topology.vertex) (dst : Topology.vertex) (k:int)
    (init_acc : constrain list) : constrain list =
  (* Constraint: sum of out going flows to other switches from src's ingress switch = k  *)
  let ingress_switch = outgoing_edges topo src
    |> List.hd_exn
    |> Topology.edge_dst
    |> fst in
  let edges = outgoing_edges topo ingress_switch in
  let diffs = List.fold_left edges ~init:[] ~f:(fun acc edge ->
    if edge_connects_switches edge topo then
          let forward_amt = var_name topo edge (src,dst) in
          let reverse_amt = var_name_rev topo edge (src,dst) in
          let net_outgoing = minus (Var (forward_amt)) (Var (reverse_amt)) in
          net_outgoing::acc
    else acc) in
  let sum_net_outgoing = Sum (diffs) in
  let name = Printf.sprintf "num-%s-%s" (name_of_vertex topo src) (name_of_vertex topo dst) in
  (Eq (name, sum_net_outgoing, Float.of_int k))::init_acc

let conservation_constraints_st (topo : Topology.t) (src : Topology.vertex) (dst : Topology.vertex)
    (init_acc : constrain list) : constrain list =
  (* Every node in the topology except the source and sink has conservation constraints *)
  Topology.fold_vertexes (fun v acc ->
   if v = src || v = dst then acc else
      let edges = outgoing_edges topo v in
      let outgoing = List.fold_left edges ~init:[] ~f:(fun acc_vars e ->
        (Var (var_name topo e (src,dst)))::acc_vars) in
      let incoming = List.fold_left edges ~init:[] ~f:(fun acc_vars e ->
        (Var (var_name_rev topo e (src,dst)))::acc_vars) in
      let total_out = Sum (outgoing) in
      let total_in = Sum (incoming) in
      let net = minus total_out total_in in
      let name = Printf.sprintf "con-%s-%s_%s" (name_of_vertex topo src)
        (name_of_vertex topo dst) (name_of_vertex topo v) in
      let constr = Eq (name, net, 0.) in
      constr::acc) topo init_acc

let minimize_path_lengths (topo : Topology.t) (src : Topology.vertex) (dst : Topology.vertex)
    (init_acc : constrain list) : constrain list =
  (* Set objective = sum of all path lengths *)
  let paths_list = Topology.fold_edges (fun e acc ->
      let weight = Link.weight (Topology.edge_to_label topo e) in
      (Times (weight, (Var (var_name topo e (src,dst)))))::acc) topo [] in
  let total_path_length = Sum (paths_list) in
  let path_length_obj = minus total_path_length objective in
  let name = Printf.sprintf "obj-%s-%s" (name_of_vertex topo src)
      (name_of_vertex topo dst) in
  let constr = Eq (name, path_length_obj, 0.) in
  constr::init_acc

let edksp_lp_of_st (topo : Topology.t) (src : Topology.vertex) (dst : Topology.vertex) (k : int) =
  let all_constrs = capacity_constraints topo src dst []
    |> num_path_constraints topo src dst k
    |> conservation_constraints_st topo src dst
    |> minimize_path_lengths topo src dst in
  (objective, all_constrs)

let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "/tmp/edksp_%f.lp" rand) in
  match Sys.file_exists try_fn with
      `Yes -> new_rand ()
       | _ -> rand

(***************** Minimum s-t cut ******************)
(* Given a topology, a source src and a sink dst, find the minimum src-dst cut
 * assuming unit link capacity *)
let max_st_flow (topo : Topology.t) (src : Topology.vertex) (dst : Topology.vertex)
    (init_acc : constrain list) : constrain list =
  (* objective = sum of out going flows to other switches from src's ingress switch *)
  let ingress_switch = outgoing_edges topo src
    |> List.hd_exn
    |> Topology.edge_dst
    |> fst in
  let edges = outgoing_edges topo ingress_switch in
  let diffs = List.fold_left edges ~init:[] ~f:(fun acc edge ->
    if edge_connects_switches edge topo then
          let forward_amt = var_name topo edge (src,dst) in
          let reverse_amt = var_name_rev topo edge (src,dst) in
          let net_outgoing = minus (Var (forward_amt)) (Var (reverse_amt)) in
          net_outgoing::acc
    else acc) in
  let sum_net_outgoing = Sum (diffs) in
  let constr = minus sum_net_outgoing objective in
  let name = Printf.sprintf "obj-%s-%s" (name_of_vertex topo src)
      (name_of_vertex topo dst) in
  (Eq (name, constr, 0.))::init_acc

let min_st_cut (topo : Topology.t) (src : Topology.vertex)
    (dst : Topology.vertex) =
  let all_constrs = capacity_constraints topo src dst []
    |> conservation_constraints_st topo src dst
    |> max_st_flow topo src dst in
  let rand = new_rand () in
  let lp_filename = (Printf.sprintf "/tmp/cut_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "/tmp/cut_%f.sol" rand) in

  (* Serialize LP and call Gurobi *)
  serialize_max_lp (objective, all_constrs) lp_filename;
  let gurobi_in = gurobi_process lp_filename lp_solname in
  let obj_str = "Optimal objective  \\([0-9.e+-]+\\)" in
  let obj_regex = Str.regexp obj_str in
  let rec read_output gurobi opt_z =
    try
      let line = In_channel.input_line_exn gurobi in
      let opt_z =
        if Str.string_match obj_regex line 0 then
          Float.of_string (Str.matched_group 1 line)
        else
          opt_z in
      read_output gurobi opt_z
    with
      End_of_file -> opt_z in
  let opt_z = read_output gurobi_in 0. in
  ignore (Unix.close_process_in gurobi_in);
  let _ = Sys.remove lp_filename in
  let _ = Sys.remove lp_solname in
  let min_cut = int_of_float opt_z in
  Printf.printf "Min cut: %s - %s : %d\n"
    (name_of_vertex topo src) (name_of_vertex topo dst) min_cut;
  min_cut

(****************************************************)

(* Given a topology,
 *  returns k edge-disjoint shortest paths per src-dst pair
 * if k edge-disjoint shortest paths are not possible,
 *  returns k-shortest paths for that pair *)
let solve_lp (topo:topology) : scheme =
  (* TODO: handle edge router constrains *)
  if !Globals.er_mode then failwith "Not implemented" else
  let sd_pairs = get_srcdst_pairs topo in
  List.fold_left sd_pairs ~init:SrcDstMap.empty ~f:(fun acc (src, dst) ->
  (* Iterate over each src-dst pair to find k edge-disjoint shortest paths *)
  let name_table = Hashtbl.Poly.create () in
  Topology.iter_vertexes (fun vert ->
    let label = Topology.vertex_to_label topo vert in
    let name = Node.name label in
        Hashtbl.Poly.add_exn name_table name vert) topo;

  let max_budget =
    if use_min_cut then
      min (min_st_cut topo src dst) (!Globals.budget)
    else
      (!Globals.budget) in
  let lp = edksp_lp_of_st topo src dst max_budget in
  let rand = new_rand () in
  let lp_filename = (Printf.sprintf "/tmp/edksp_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "/tmp/edksp_%f.sol" rand) in
  (* Serialize LP and call Gurobi *)
  serialize_lp lp lp_filename;
  call_gurobi lp_filename lp_solname;

  (* read back all the edge flows from the .sol file *)
  let read_results input =
     let results = In_channel.create input in
     let result_str = "^f_\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\)_" ^
                      "\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\) \\([0-9.e+-]+\\)$"
     in
     let regex = Str.regexp result_str in
     let rec read inp opt_z flows =
       let line = try In_channel.input_line_exn inp
         with End_of_file -> "" in
       if line = "" then (opt_z,flows)
       else
         let new_z, new_flows =
           if line.[0] = '#' then (opt_z, flows)
           else if line.[0] = 'Z' then
             let ratio_str = Str.string_after line 2 in
             let ratio = Float.of_string ratio_str in
             (ratio *. demand_divisor /. cap_divisor, flows)
           else
             (if Str.string_match regex line 0 then
                let vertex s = Topology.vertex_to_label topo
                    (Hashtbl.Poly.find_exn name_table s) in
                let dem_src = vertex (Str.matched_group 1 line) in
                let dem_dst = vertex (Str.matched_group 2 line) in
                let edge_src = vertex (Str.matched_group 3 line) in
                let edge_dst = vertex (Str.matched_group 4 line) in
                let flow_amt = Float.of_string (Str.matched_group 5 line) in
                if flow_amt = 0. then (opt_z, flows)
                else
                  let tup = (dem_src, dem_dst, flow_amt, edge_src, edge_dst) in
                  (opt_z, (tup::flows))
              else (opt_z, flows)) in
         read inp new_z new_flows in
        (* end read *)
      let result = read results 0. [] in
      In_channel.close results; result in
      (* end read_results *)
    let ratio, flows = read_results lp_solname in
    let _ = Sys.remove lp_filename in
    let _ = Sys.remove lp_solname in
    let flows_table = Hashtbl.Poly.create () in

    (* partition the edge flows based on which commodity they are *)
    List.iter flows ~f:(fun (d_src, d_dst, flow, e_src, e_dst) ->
        if Hashtbl.Poly.mem flows_table (d_src, d_dst) then
          let prev_edges = Hashtbl.Poly.find_exn flows_table (d_src, d_dst) in
          Hashtbl.Poly.set flows_table (d_src, d_dst)
            ((e_src, e_dst, flow)::prev_edges)
        else
          Hashtbl.Poly.add_exn flows_table (d_src, d_dst)
            [(e_src, e_dst, flow)]);
    (* tmp_scheme has paths for only src->dst *)
    let tmp_scheme = Mcf.recover_paths topo flows_table in
    let st_ppmap = SrcDstMap.find tmp_scheme (src,dst) in
    match st_ppmap with
    | None ->
      if use_min_cut then
        failwith "EdKSP failed to find min-cut # paths."
      else
        (* If k-edge-disjoint paths is not feasible,
           fall back to k-shortest paths *)
        let ksp = k_shortest_paths topo src dst (min !Globals.budget 100) in
        let num_paths = Float.of_int  (List.length ksp) in
        let path_map = List.fold_left ksp ~init:PathMap.empty
                         ~f:(fun acc2 path ->
                           let prob = 1.0 /. num_paths in
                           PathMap.set acc2 ~key:path ~data:prob) in
        SrcDstMap.set ~key:(src,dst) ~data:path_map acc
    | Some x ->
        SrcDstMap.set ~key:(src,dst) ~data:x acc)

let solve (topo:topology) (_:demands) : scheme =
  let new_scheme =
    if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme else
      solve_lp topo in
  prev_scheme := new_scheme;
  new_scheme

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = normalization_recovery
