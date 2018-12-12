open Core

open LP_Lang
open Util
open Yates_types.Types

let () = match !Globals.rand_seed with
  | Some x -> Random.init x
  | None -> Random.self_init ~allow_in_tests:true ()

let objective = Var "Z"

let capacity_constraints (topo : Topology.t) (d_pairs : demands)
                         (init_acc : constrain list) : constrain list =
  (* For every edge, there is a capacity constraint *)
  Topology.fold_edges
    (fun edge acc ->
     (* The sum of all commodity flows in both direction must exceed
         the capacity by less than Z * capacity. *)
     (* Gather all of the terms for each commodity *)
     let all_flows = SrcDstMap.fold
                       ~init:[]
                       ~f:(fun ~key:(u,v) ~data:r acc2 ->
                           let forward_amt = var_name topo edge (u,v) in
                           Var(forward_amt)::acc2) d_pairs
      in
      (* Add them all up *)
      let total_flow = Sum (all_flows) in
      let scaled_cap = Times ((capacity_of_edge topo edge) /. cap_divisor, objective) in
      (* Total flow is at most the scaled capacity *)
      let constr = minus total_flow scaled_cap in
      let name = Printf.sprintf "cap_%s"
          (string_of_edge topo edge) in
      (Leq (name, constr, 0.))::acc) topo init_acc

let demand_constraints (topo : Topology.t) (d_pairs : demands)
    (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has a demand constraint *)
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:(demand) acc ->
      (* We need to add up the rates for all edges adjoining the source *)
      let edges = outgoing_edges topo src in
      let diffs = List.fold_left edges ~init:[] ~f:(fun acc2 edge ->
          let forward_amt = var_name topo edge (src,dst) in
          let reverse_amt = var_name_rev topo edge (src,dst) in
          let net_outgoing = minus (Var (forward_amt)) (Var (reverse_amt)) in
          net_outgoing::acc2) in
      let sum_net_outgoing = Sum (diffs) in
      (* Net amount of outgoing flow must meet the demand *)
      let name = Printf.sprintf "dem-%s-%s" (name_of_vertex topo src)
          (name_of_vertex topo dst) in
      (Geq (name, sum_net_outgoing, demand /. demand_divisor))::acc) d_pairs

let conservation_constraints (topo : Topology.t) (d_pairs : demands)
    (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has its own conservation constraints *)
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:demand acc ->
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

let avoid_access_links_constraints (topo : Topology.t) (d_pairs : demands)
    (init_acc : constrain list) : constrain list =
  (* Avoid traffic to go through other edge routers or hosts *)
  let hosts = get_hosts_set topo in
  let access_links = VertexSet.fold hosts ~init:[]
      ~f:(fun acc h ->
          incoming_edges topo h @ outgoing_edges topo h @ acc) in
  SrcDstMap.fold ~init:init_acc
    ~f:(fun ~key:(src,dst) ~data:_ acc ->
        List.fold_left ~init:acc
          ~f:(fun acc e ->
              if is_incident e src || is_incident e dst then acc
              else
                let name = Printf.sprintf "access-%s-%s_%s"
                    (name_of_vertex topo src) (name_of_vertex topo dst)
                    (string_of_edge topo e) in
                let constr =
                  Eq (name, (Var (var_name topo e (src,dst))), 0.) in
                constr::acc) access_links) d_pairs

let lp_of_graph (topo : Topology.t) (demand_pairs : demands) =
  let all_constrs = capacity_constraints topo demand_pairs []
                  |> demand_constraints topo demand_pairs
                  |> conservation_constraints topo demand_pairs
                  |> avoid_access_links_constraints topo demand_pairs in
  (objective, all_constrs)

let recover_paths (orig_topo : Topology.t) (flow_table : flow_table)
  : scheme  =

  (* For a single commodity, given the individual edges used, get all the paths
     used to route that commodity *)
  let strip_paths (d_src, d_dst) edges =
    let empty_topo = Topology.empty () in
    (* Add all the nodes *)
    let node_table = Hashtbl.Poly.create () in
    (* Adds a node if it has not already been added *)
    let add topo node =
      if Hashtbl.Poly.mem node_table node then topo else
        let (new_topo, node_id) = Topology.add_vertex topo node in
        let () = Hashtbl.Poly.add_exn node_table node node_id in
        new_topo in
    let topo_with_src = add empty_topo d_src in
    let topo_with_src_dst = add topo_with_src d_dst in
    (* Add both endpoints of every edge *)
    let topo_with_nodes = List.fold_left edges ~init:topo_with_src_dst
        ~f:(fun acc_topo (src,dst,_) ->
        let acc_topo2 = add acc_topo src in
        add acc_topo2 dst) in
    (* Add all the edges *)
    let id_of_node n = Hashtbl.Poly.find_exn node_table n in
    let topo_with_edges = List.fold_left edges ~init:topo_with_nodes
        ~f:(fun acc_topo (src,dst,cap) ->
            let src_id = id_of_node src in
            let dst_id = id_of_node dst in
            let edge = Link.create 1L 1L in
            Link.set_weight edge cap;
            let (new_topo, edge_id) = Topology.add_edge acc_topo src_id Int32.one
                edge dst_id Int32.one in
            new_topo) in

    let src_id = id_of_node d_src in
    let dst_id = id_of_node d_dst in

    (* Until there is no src-dst path, find a path, determine its bottleneck
       capacity, pull it out and decrement the capacities by the bottleneck.*)
    let rec find_paths path_topo acc_paths =
      let path_opt = NetPath.shortest_path path_topo src_id dst_id in
      match path_opt with
      | None -> acc_paths
      | Some path ->
        (* Find bottleneck *)
        let bottleneck = List.fold_left path ~init:Float.infinity
            ~f:(fun acc edge ->
                let label = Topology.edge_to_label path_topo edge in
                min acc (Link.weight label)) in
        (* Decrease weights by bottleneck, mark those with zero weight
           for deletion *)
        let delete_these = List.fold_left path ~init:[] ~f:(fun acc edge ->
            let label = Topology.edge_to_label path_topo edge in
            let old_wt = Link.weight label in
            let new_wt = old_wt -. bottleneck in
            Link.set_weight label new_wt;
            if new_wt <= 0. then edge::acc else acc) in
        (* Delete the edges that were zeroed out *)
        let new_topo = List.fold_left delete_these ~init:path_topo
            ~f:(fun acc_topo edge ->
                Topology.remove_edge acc_topo edge) in

        let mapped_path = List.map path ~f:(fun edge ->
            let src_label = Topology.vertex_to_label
                path_topo (fst (Topology.edge_src edge)) in
            let dst_label = Topology.vertex_to_label
                path_topo (fst (Topology.edge_dst edge)) in
            let src = Topology.vertex_of_label orig_topo src_label in
            let dst = Topology.vertex_of_label orig_topo dst_label in
            let orig_edge = Topology.find_edge orig_topo src dst in
            orig_edge) in

        find_paths new_topo ((mapped_path,
                              bottleneck *. demand_divisor)::acc_paths) in
    let paths = find_paths topo_with_edges [] in
    paths in (* This line ends find_paths by returning paths. *)
    (* For every commodity, get their paths. *)
    let (unnormalized_scheme, flow_sum) = Hashtbl.fold
      flow_table
      ~init:(SrcDstMap.empty, SrcDstMap.empty)
      ~f:(fun ~key:d_pair ~data:edges (us,fs) ->
          let (s,t) = d_pair in
          let s_v = Topology.vertex_of_label orig_topo s in
          let t_v = Topology.vertex_of_label orig_topo t in
          let paths = if s <> t then strip_paths (s, t) edges else [] in
          let (p,sum_rate) =
            List.fold_left
              paths
              ~init:(PathMap.empty,0.)
              ~f:(fun (acc,sum_acc) (path,scalar) -> (PathMap.set acc path scalar, sum_acc +. scalar) ) in
          let new_us = SrcDstMap.set us ~key:(s_v,t_v) ~data:p in
          let new_fs = SrcDstMap.set fs ~key:(s_v,t_v) ~data:sum_rate in
          (new_us, new_fs)) in
      (* Now normalize the values in the scheme so that they sum to 1 for each source-dest pair *)
  SrcDstMap.fold ~init:(SrcDstMap.empty)
    ~f:(fun ~key:(u,v) ~data:f_decomp acc  ->
      match SrcDstMap.find flow_sum (u,v) with
      | None -> assert false
      | Some sum_rate ->
        ignore (if (sum_rate < 0.) then failwith "sum_rate leq 0. on flow" else ());
        let default_value = 1.0 /. (Float.of_int (PathMap.length f_decomp) ) in
        let normalized_f_decomp =
          PathMap.fold ~init:(PathMap.empty)
            ~f:(fun ~key:path ~data:rate acc ->
                let normalized_rate =
                  if sum_rate = 0. then
                    default_value
                  else
                    rate /. sum_rate in

                PathMap.set ~key:path ~data:normalized_rate acc)
            f_decomp in
        SrcDstMap.set ~key:(u,v) ~data:normalized_f_decomp acc) unnormalized_scheme

let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "/tmp/mcf_%f.lp" rand) in
  match Sys.file_exists try_fn with
      `Yes -> new_rand ()
       | _ -> rand

(* Run everything. Given a topology and a set of pairs with demands,
   returns the optimal congestion ratio, the paths used, and the number
   of paths used. *)
(* let solver_paths topo pairs verbose = *)
let solve (topo:topology) (pairs:demands) : scheme =

  let name_table = Hashtbl.Poly.create () in
  Topology.iter_vertexes (fun vert ->
      let label = Topology.vertex_to_label topo vert in
      let name = Node.name label in
      Hashtbl.Poly.add_exn name_table name vert) topo;

  let lp = lp_of_graph topo pairs in
  let rand = new_rand () in
  let lp_filename = (Printf.sprintf "/tmp/mcf_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "/tmp/mcf_%f.sol" rand) in
  (* serialize LP and send to Gurobi *)
  serialize_lp lp lp_filename;
  call_gurobi lp_filename lp_solname;

  (* read back all the edge flows from the .sol file *)
  let read_results input =
    (* start read_results fn *)
    let results = In_channel.create input in
    let result_str = "^f_\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\)_" ^
                     "\\([a-zA-Z0-9]+\\)--\\([a-zA-Z0-9]+\\) \\([0-9.e+-]+\\)$" in
    let regex = Str.regexp result_str in
    let rec read inp opt_z flows =
      (* start read fn *)
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
        (* end read fn *)
      let result = read results 0. [] in
      In_channel.close results; result in
      (* end read_results fn *)

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

    recover_paths topo flows_table

let initialize _ = ()

let local_recovery = normalization_recovery
