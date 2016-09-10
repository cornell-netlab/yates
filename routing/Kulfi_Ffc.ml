open Kulfi_Types

open Frenetic_Network
open Net
open Core.Std
open Kulfi_LP_Lang
open Kulfi_Spf
open Kulfi_Globals

let () = match !Kulfi_Globals.rand_seed with
  | Some x -> Random.init x
  | None -> Random.self_init ~allow_in_tests:true ()

let objective = Var "Z"

(*
 * Basic constrains are slightly different compared to MCF
 * MCF tries to minimize congestion. So, it miminimzes Z where traffic on an edge <= Z * capacity
 * FFC tries to maximize throughput Z = sum of granted bandwidths
 * Refer equation numbers from SIGCOMM 2014 FFC paper
 * *)



(* Equation (1) :
 * Throughput Z = total granted bandwidth *)
let objective_fun (topo : Topology.t) (d_pairs : demands)
                  (init_acc : constrain list) : constrain list =
    let granted_bw_vars = SrcDstMap.fold ~init:[] ~f:(fun ~key:(src,dst) ~data:(_) acc ->
        let granted_bw_var = granted_bw_var_name topo (src,dst) in
        (Var(granted_bw_var)) :: acc) d_pairs in
    let throughput = Sum (granted_bw_vars) in
    let diff_tput_obj = minus throughput objective in
    let name = "tput" in
    (Eq (name, diff_tput_obj, 0.))::init_acc


(* Equation (2) :
 * Total traffic on an edge <= capacity of edge *)
let capacity_constraints (topo : Topology.t) (d_pairs : demands)
                         (init_acc : constrain list) : constrain list =
  (* For every edge, there is a capacity constraint *)
  Topology.fold_edges
    (fun edge acc ->
     (* The sum of all commodity flows must be less than capacity. *)
     (* Gather all of the terms for each commodity *)
     let all_flows = SrcDstMap.fold
                       ~init:[]
                       ~f:(fun ~key:(u,v) ~data:r acc2 ->
                           let forward_amt = var_name topo edge (u,v) in
                           Var(forward_amt):: acc2) d_pairs
      in
      (* Add them all up *)
      let total_flow = Sum (all_flows) in
      let scaled_cap = ((capacity_of_edge topo edge) /. cap_divisor) in
      (* Total flow is at most the scaled capacity *)
      let name = Printf.sprintf "cap_%s"
          (string_of_edge topo edge) in
      (Leq (name, total_flow, scaled_cap))::acc) topo init_acc

let neighboring_edges topo src =
  let src_neighbors = Topology.neighbors topo src in
  (* Get all outgoing edges *)
  let edges = VertexSet.fold src_neighbors ~init:[] ~f:(fun acc vtx ->
      let es = Topology.find_all_edges topo src vtx in
      List.rev_append (EdgeSet.elements es) acc) in
  edges


(* Equation (3) :
 * sum of allocated BWs on all tunnels for a flow should be >= BW granted for the flow *)
let grantedbw_constraints (topo : Topology.t) (d_pairs : demands)
    (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has a granted bandwidth constraint *)
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:(_) acc ->
      (* We need to add up the rates for all edges adjoining the source *)
      let edges = neighboring_edges topo src in
      let diffs = List.fold_left edges ~init:[] ~f:(fun acc2 edge ->
          let forward_amt = var_name topo edge (src,dst) in
          let reverse_amt = var_name_rev topo edge (src,dst) in
          let net_outgoing = minus (Var (forward_amt)) (Var (reverse_amt)) in
          net_outgoing::acc2) in
      let sum_net_outgoing = Sum (diffs) in
      (* Net amount of outgoing flow must meet the granted BW
       * i.e, net outgoing - bw granted >= 0 *)
      let granted_bw_var = granted_bw_var_name topo (src,dst) in
      let diff_allocated_granted = minus sum_net_outgoing (Var (granted_bw_var)) in
      let name = Printf.sprintf "grant-%s-%s" (name_of_vertex topo src)
          (name_of_vertex topo dst) in
      (Geq (name, diff_allocated_granted, 0.))::acc) d_pairs



(* Equation (4) :
 * BW granted for a flow <= demand for the flow *)
let demand_constraints (topo : Topology.t) (d_pairs : demands)
    (init_acc : constrain list) : constrain list =
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:(demand) acc ->
      let granted_bw_var = granted_bw_var_name topo (src,dst) in
      let name = Printf.sprintf "dem-%s-%s" (name_of_vertex topo src)
          (name_of_vertex topo dst) in
      (* BW granted <= demand *)
      (Leq (name, (Var (granted_bw_var)), (demand /. demand_divisor)))::acc) d_pairs


(* Flow conservation at each switch *)
let conservation_constraints (topo : Topology.t) (d_pairs : demands)
    (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has its own conservation constraints *)
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:demand acc ->
      (* Every node in the topology except the source and sink has
       * conservation constraints *)
      Topology.fold_vertexes (fun v acc2 ->
          if v = src || v = dst then acc2 else
            let edges = neighboring_edges topo v in
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
            constr::acc2) topo acc) d_pairs

(* Construct linear program *)
let lp_of_graph (topo : Topology.t) (demand_pairs : demands) =
  let all_constrs =
      capacity_constraints topo demand_pairs []
      |> grantedbw_constraints topo demand_pairs
      |> demand_constraints topo demand_pairs
      |> conservation_constraints topo demand_pairs
      |> objective_fun topo demand_pairs in
  (objective, all_constrs)

(* Generate a lp file with unique name *)
let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "lp/ffc_%f.lp" rand) in
  match Sys.file_exists try_fn with
      `Yes -> new_rand ()
       | _ -> rand

(* Construct a LP to maximize throughput and call gurobi to solve it *)
let solve (topo:topology) (pairs:demands) : scheme =

  let name_table = Hashtbl.Poly.create () in
  Topology.iter_vertexes (fun vert ->
    let label = Topology.vertex_to_label topo vert in
    let name = Node.name label in
    Hashtbl.Poly.add_exn name_table name vert) topo;

  let lp = lp_of_graph topo pairs in
  let rand = new_rand () in
  let lp_filename = (Printf.sprintf "lp/ffc_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "lp/ffc_%f.sol" rand) in
  serialize_max_lp lp lp_filename;

  let method_str = (Int.to_string !gurobi_method) in
  let gurobi_in = Unix.open_process_in
        ("gurobi_cl Method=" ^ method_str ^ " OptimalityTol=1e-9 ResultFile=" ^ lp_solname ^ " " ^ lp_filename) in
  let time_str = "Solved in [0-9]+ iterations and \\([0-9.e+-]+\\) seconds" in
  let time_regex = Str.regexp time_str in
  let rec read_output gurobi solve_time =
    try
      let line = input_line gurobi in
      if Str.string_match time_regex line 0 then
        let num_seconds = Float.of_string (Str.matched_group 1 line) in
          read_output gurobi num_seconds
        else
          read_output gurobi solve_time
      with
        End_of_file -> solve_time in
    let _ = read_output gurobi_in 0. in
    ignore (Unix.close_process_in gurobi_in);

    (* read back all the edge flows from the .sol file *)
    let read_results input =
      let results = open_in input in
      let result_str = "^f_\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\)_" ^
                       "\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\) \\([0-9.e+-]+\\)$"
      in
      let regex = Str.regexp result_str in
      let rec read inp opt_z flows =
        let line = try input_line inp
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

      let result = read results 0. [] in
      In_channel.close results; result in
    let ratio, flows = read_results lp_solname in
    (*let _ = Sys.remove lp_filename in
    let _ = Sys.remove lp_solname in*)
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

    Kulfi_Mcf.recover_paths topo flows_table

let initialize _ = ()

let local_recovery = Kulfi_Types.normalization_recovery
