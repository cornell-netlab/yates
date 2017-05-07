open Frenetic_Network
open Net
open Core.Std
open Kulfi_Globals
open Kulfi_Routing_Util
open Kulfi_Types
open SM_LP_Lang

let objective = Var "Z"

let capacity_constraints (topo : Topology.t) (crashed_dc:Topology.vertex)
    (d_pairs : demands) (init_acc : constrain list) : constrain list =
  (* For every edge, there is a capacity constraint *)
  Topology.fold_edges
    (fun edge acc ->
       (* The sum of all commodity flows in each direction must exceed be less
          than Z * capacity. *)

       (* Gather all of the terms for each commodity *)
       let all_flows = SrcDstMap.fold ~init:[]
           ~f:(fun ~key:(u,v) ~data:_ acc2 ->
               if u = crashed_dc || v = crashed_dc then acc2
               else
                 let forward_amt = var_name topo edge (u,v) in
                 Var(forward_amt)::  acc2) d_pairs in
       (* Add them all up *)
       let total_flow = Sum (all_flows) in
       let scaled_cap = Times ((capacity_of_edge topo edge) /. cap_divisor,
                               objective) in
       (* Total flow is at most the scaled capacity *)
       let constr = minus total_flow scaled_cap in
       let name = Printf.sprintf "cap_%s"
           (string_of_edge topo edge) in
       (Leq (name, constr, 0.))::acc) topo init_acc

let demand_constraints (topo : Topology.t) (crashed_dc:Topology.vertex)
    (d_pairs : demands) (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has a demand constraint *)
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:(demand) acc ->
      (* ignore demands to/from crashed node *)
      if src = crashed_dc || dst = crashed_dc || src = dst then acc
      else
        (* Add up the flows on all edges adjoining the source *)
        let edges = outgoing_edges topo src in
        let diffs = List.fold_left edges ~init:[] ~f:(fun acc2 edge ->
            let forward_amt = var_name topo edge (src,dst) in
            let reverse_amt = var_name_rev topo edge (src,dst) in
            let net_outgoing = minus (Var (forward_amt)) (Var (reverse_amt)) in
            net_outgoing::acc2) in
        let sum_net_outgoing = Sum (diffs) in

        (* Update demand to included traffic shifted:
           (src:a, dst:b, crashednode:x)
           d'_ab = d_ab + sin_xb * d_ax + seg_ax * d_xb *)
        let d_ax = SrcDstMap.find_exn d_pairs (src, crashed_dc) /. demand_divisor in
        let sin_xb = var_name_sin topo crashed_dc dst in
        let d_xb = SrcDstMap.find_exn d_pairs (crashed_dc, dst) /. demand_divisor in
        let seg_ax = var_name_seg topo src crashed_dc in
        let demand_on_shift = Sum ([ (* d_ab goes to right side of constraint *)
            Times (d_ax, Var (sin_xb));
            Times (d_xb, Var (seg_ax))]) in

        (* Outgoing traffic - new_demand >= 0 *)
        let diff = minus sum_net_outgoing demand_on_shift in

        (* Net amount of outgoing flow must meet the demand *)
        let name = Printf.sprintf "dem-%s-%s" (name_of_vertex topo src)
            (name_of_vertex topo dst) in
        (Geq (name, diff, (demand /. demand_divisor)))::acc) d_pairs

let conservation_constraints (topo : Topology.t) (crashed_dc:Topology.vertex)
      (d_pairs : demands) (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has its own conservation constraints *)
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:_ acc ->
      (* ignore demands to/from crashed node *)
      if src = crashed_dc || dst = crashed_dc then acc
      else
        (* Every node in the topology except the source and sink has
         * conservation constraints *)
        Topology.fold_vertexes (fun v acc2 ->
            if v = src || v = dst then acc2 else
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
              constr::acc2) topo acc) d_pairs

let shift_amount_constraints (topo : Topology.t) (crashed_dc:Topology.vertex)
    (init_acc : constrain list) : constrain list =
  let hosts = get_hosts_set topo in
  let sum_sin, sum_seg =
    VertexSet.fold ~init:([],[]) ~f:(fun (sin_acc,seg_acc) host ->
        (* total volume of traffic shifted to/from the crashed node is some
           fraction of total traffic to/from the crashed node in steady state *)
        if host = crashed_dc then (sin_acc, seg_acc)
        else
          let sin_xb = Var (var_name_sin topo crashed_dc host) in
          let seg_ax = Var (var_name_seg topo host crashed_dc) in
          (sin_xb::sin_acc, seg_ax::seg_acc)) hosts in
  let sin_constr = Geq("sum_sin", Sum (sum_sin), 1.) in
  let seg_constr = Geq("sum_seg", Sum (sum_seg), 1.) in
  sin_constr::seg_constr::init_acc


(* Construct joint LP considering naive traffic shift and routing *)
let lp_of_graph (topo : Topology.t) (crashed_dc:Topology.vertex) (demand_pairs : demands) =
  let all_constrs = capacity_constraints topo crashed_dc demand_pairs []
                    |> demand_constraints topo crashed_dc demand_pairs
                    |> conservation_constraints topo crashed_dc demand_pairs
                    |> shift_amount_constraints topo crashed_dc in
  (objective, all_constrs)

let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "lp/ns_%f.lp" rand) in
  match Sys.file_exists try_fn with
    `Yes -> new_rand ()
  | _ -> rand

(* Given a modified topology, crashed_node and TM,
   returns the updated TM after shifting demands *)
let model (topo:topology) (crashed_dc:vertex) (pairs:demands) : demands =
  let name_table = Hashtbl.Poly.create () in
  Topology.iter_vertexes (fun vert ->
      let label = Topology.vertex_to_label topo vert in
      let name = Node.name label in
      Hashtbl.Poly.add_exn name_table name vert) topo;

  let lp = lp_of_graph topo crashed_dc pairs in
  let rand = new_rand () in
  let lp_filename = (Printf.sprintf "lp/ns_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "lp/ns_%f.sol" rand) in
  serialize_lp lp lp_filename;

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
                     "\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\) \\([0-9.e+-]+\\)$" in
    let regex = Str.regexp result_str in
    let shift_str = "^s\\([a-z]+\\)_\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\)" ^
                     " \\([0-9.e+-]+\\)$" in
    let shift_regex = Str.regexp shift_str in

    let rec read inp opt_z flows shift_vals =
      let line = try input_line inp
        with End_of_file -> "" in
      if line = "" then (opt_z,flows,shift_vals)
      else
        let new_z, new_flows, shift_vals =
          if line.[0] = '#' then (opt_z, flows, shift_vals)
          else if line.[0] = 'Z' then
            let ratio_str = Str.string_after line 2 in
            let ratio = Float.of_string ratio_str in
            (ratio *. demand_divisor /. cap_divisor, flows, shift_vals)
          else
            (if Str.string_match regex line 0 then
               let vertex s = Topology.vertex_to_label topo
                                (Hashtbl.Poly.find_exn name_table s) in
               let dem_src = vertex (Str.matched_group 1 line) in
               let dem_dst = vertex (Str.matched_group 2 line) in
               let edge_src = vertex (Str.matched_group 3 line) in
               let edge_dst = vertex (Str.matched_group 4 line) in
               let flow_amt = Float.of_string (Str.matched_group 5 line) in
               if flow_amt = 0. then (opt_z, flows, shift_vals)
               else
                 let tup = (dem_src, dem_dst, flow_amt, edge_src, edge_dst) in
                 (opt_z, (tup::flows), shift_vals)
             else (
               if Str.string_match shift_regex line 0 then
                 let shift_in, shift_eg = shift_vals in
                 let shift_amt = Float.of_string (Str.matched_group 4 line) in
                 let new_shift_vals =
                   (match (Str.matched_group 1 line) with
                    | "in" ->
                      let shift_in_node = Hashtbl.Poly.find_exn name_table (Str.matched_group 3 line) in
                      (SrcDstMap.add shift_in
                         ~key:(crashed_dc, shift_in_node) ~data:shift_amt,
                       shift_eg)
                    | "eg" ->
                      let shift_eg_node = Hashtbl.Poly.find_exn name_table (Str.matched_group 2 line) in
                      (shift_in,
                       SrcDstMap.add shift_eg
                         ~key:(shift_eg_node, crashed_dc) ~data:shift_amt)
                    | _ -> shift_vals) in
                 (opt_z, flows, new_shift_vals)

              else (opt_z, flows, shift_vals))) in
        read inp new_z new_flows shift_vals in
    let result = read results 0. [] (SrcDstMap.empty, SrcDstMap.empty) in
    In_channel.close results; result in
    (* end read_results *)

  let _, _, (shift_in, shift_eg) = read_results lp_solname in
  let _ = Sys.remove lp_filename in
  let _ = Sys.remove lp_solname in

  let shifted_tm = SrcDstMap.fold ~init:SrcDstMap.empty
      ~f:(fun ~key:(u,v) ~data:(d) acc ->
          if u = crashed_dc || v = crashed_dc then acc
          else
            let d' =
              (if u = v then 0.
               else
                 let sin_xv = SrcDstMap.find_exn shift_in (crashed_dc, v) in
                 let seg_ux = SrcDstMap.find_exn shift_eg (u, crashed_dc) in
                 let d_ux = SrcDstMap.find_exn pairs (u, crashed_dc) in
                 let d_xv = SrcDstMap.find_exn pairs (crashed_dc, v) in
                 d +. d_ux *. sin_xv +. d_xv *. seg_ux) in
            SrcDstMap.add ~key:(u,v) ~data:d' acc) pairs in
  shifted_tm
