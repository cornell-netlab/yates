open Frenetic_Network
open Net
open Core.Std
open Kulfi_Globals
open Kulfi_Routing_Util
open Kulfi_Types
open SM_LP_Lang

let objective = Var "Z"

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
           (src:u, dst:v, crashednode:x)
           d'_uv = d_uv + sin_uxv * d_ux + seg_xuv * d_xv *)
        let d_ux = SrcDstMap.find_exn d_pairs (src, crashed_dc) /. demand_divisor in
        let sin_uxv = var_name_sdsin topo src crashed_dc dst in
        let d_xv = SrcDstMap.find_exn d_pairs (crashed_dc, dst) /. demand_divisor in
        let seg_xuv = var_name_sdseg topo crashed_dc src dst in
        let demand_on_shift = Sum ([ (* d_ab goes to right side of constraint *)
            Times (d_ux, Var (sin_uxv));
            Times (d_xv, Var (seg_xuv))]) in

        (* Outgoing traffic - new_demand >= 0 *)
        let diff = minus sum_net_outgoing demand_on_shift in

        (* Net amount of outgoing flow must meet the demand *)
        let name = Printf.sprintf "dem-%s-%s" (name_of_vertex topo src)
            (name_of_vertex topo dst) in
        (Geq (name, diff, (demand /. demand_divisor)))::acc) d_pairs

let shift_amount_constraints (topo : Topology.t) (crashed_dc:Topology.vertex)
    (init_acc : constrain list) : constrain list =
  let hosts = get_hosts_set topo in
  let sin_acc = VertexSet.fold ~init:init_acc ~f:(fun acc u ->
      (* for each u, *)
      if u = crashed_dc then acc
      else
        (* \sum sin_uxv = 1 *)
        let sin_uxvs = VertexSet.fold ~init:[] ~f:(fun acc v ->
            if v = crashed_dc then acc
            else Var (var_name_sdsin topo u crashed_dc v)::acc) hosts in
        let sum_sin_uxvs = Sum(sin_uxvs) in
        let name = Printf.sprintf "sum_sin_%s" (name_of_vertex topo u) in
        (Eq (name, sum_sin_uxvs, 1.)::acc)) hosts in
  let seg_acc = VertexSet.fold ~init:sin_acc ~f:(fun acc v ->
      (* for each v, *)
      if v = crashed_dc then acc
      else
        (* \sum seg_xuv = 1 *)
        let seg_xuvs = VertexSet.fold ~init:[] ~f:(fun acc u ->
            if u = crashed_dc then acc
            else Var (var_name_sdseg topo crashed_dc u v)::acc) hosts in
        let sum_seg_xuvs = Sum(seg_xuvs) in
        let name = Printf.sprintf "sum_seg_%s" (name_of_vertex topo v) in
        (Eq (name, sum_seg_xuvs, 1.)::acc)) hosts in
  VertexSet.fold ~init:seg_acc ~f:(fun acc u ->
      (* for each u, *)
      if u = crashed_dc then acc
      else
        let sin_uxu = Var (var_name_sdsin topo u crashed_dc u) in
        let seg_xuu = Var (var_name_sdseg topo crashed_dc u u) in
        let name_sin = Printf.sprintf "sin_%s" (name_of_vertex topo u) in
        let name_seg = Printf.sprintf "seg_%s" (name_of_vertex topo u) in
        (Eq (name_sin, sin_uxu, 0.)::Eq (name_seg, seg_xuu, 0.)::acc)) hosts

(* Construct joint LP considering naive traffic shift and routing *)
let lp_of_graph (topo : Topology.t) (crashed_dc:Topology.vertex) (demand_pairs : demands) =
  let all_constrs = SM_Naive.capacity_constraints topo crashed_dc demand_pairs []
                    |> demand_constraints topo crashed_dc demand_pairs
                    |> SM_Naive.conservation_constraints topo crashed_dc demand_pairs
                    |> shift_amount_constraints topo crashed_dc in
  (objective, all_constrs)

let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "lp/sds_%f.lp" rand) in
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
  let lp_filename = (Printf.sprintf "lp/sds_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "lp/sds_%f.sol" rand) in
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
                     "--\\([a-zA-Z0-9]+\\) \\([0-9.e+-]+\\)$" in
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
                 let shift_amt = Float.of_string (Str.matched_group 5 line) in
                 let new_shift_vals =
                   (match (Str.matched_group 1 line) with
                    | "in" ->
                      let shift_in_u = Hashtbl.Poly.find_exn name_table (Str.matched_group 2 line) in
                      let shift_in_v = Hashtbl.Poly.find_exn name_table (Str.matched_group 4 line) in
                      Hashtbl.Poly.add_exn shift_in (shift_in_u, crashed_dc, shift_in_v) shift_amt;
                      (shift_in, shift_eg)
                    | "eg" ->
                      let shift_eg_u = Hashtbl.Poly.find_exn name_table (Str.matched_group 3 line) in
                      let shift_eg_v = Hashtbl.Poly.find_exn name_table (Str.matched_group 4 line) in
                      Hashtbl.Poly.add_exn shift_eg (crashed_dc, shift_eg_u, shift_eg_v) shift_amt;
                      (shift_in, shift_eg)
                    | _ -> shift_vals) in
                 (opt_z, flows, new_shift_vals)

              else (opt_z, flows, shift_vals))) in
        read inp new_z new_flows shift_vals in
    let result = read results 0. [] (Hashtbl.Poly.create(),
                                     Hashtbl.Poly.create()) in
    In_channel.close results; result in
    (* end read_results *)

  let _, _, (shift_in, shift_eg) = read_results lp_solname in
  let _ = Sys.remove lp_filename in
  let _ = Sys.remove lp_solname in

  let shifted_tm = SrcDstMap.fold ~init:SrcDstMap.empty
      ~f:(fun ~key:(u,v) ~data:(d) acc ->
          if u = crashed_dc || v = crashed_dc then acc
          else
           (* d'_uv = d_uv + sin_uxv * d_ux + seg_xuv * d_xv *)
            let d' =
              (if u = v then 0.
               else
                 let sin_uxv = Hashtbl.Poly.find_exn shift_in (u, crashed_dc, v) in
                 let seg_xuv = Hashtbl.Poly.find_exn shift_eg (crashed_dc, u, v) in
                 let d_ux = SrcDstMap.find_exn pairs (u, crashed_dc) in
                 let d_xv = SrcDstMap.find_exn pairs (crashed_dc, v) in
                 d +. d_ux *. sin_uxv +. d_xv *. seg_xuv) in
            SrcDstMap.add ~key:(u,v) ~data:d' acc) pairs in
  shifted_tm
