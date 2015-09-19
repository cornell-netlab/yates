open Kulfi_Types

open Frenetic_Network
open Net
open Core.Std

type var = string

(* This is stripped down to cover only what we'll need for MCF *)
type arith_exp =
    Var of var
  | Num of float
  | Times of float * arith_exp
  | Sum of arith_exp list

let minus ex1 ex2 =
  let list1 = match ex1 with
    | Var _ | Num _ | Times _ -> [ex1]
    | Sum lst -> lst in
  let rec negate ex = match ex with
    | Var var -> [Times (-1., Var var)]
    | Num f -> [Num (-.f)]
    | Times (f, x) -> [Times (-.f, x)]
    | Sum lst -> List.concat (List.map lst ~f:(fun x -> negate x)) in
  let all_terms = list1 @ (negate ex2) in
  Sum all_terms

type constrain =
    Eq of string * arith_exp * float
  | Leq of string * arith_exp * float
  | Geq of string * arith_exp * float

(* (s,t,r) = node s wants to send to node t at rate r *)
type demand_pair = Topology.vertex * Topology.vertex * float

(* Minimize (arith_exp) subject to (constraints) *)
type lp = arith_exp * (constrain list)

let name_of_vertex topo v =
  let label = Topology.vertex_to_label topo v in
  Node.name label

let string_of_edge topo e =
  let (v1,_) = Topology.edge_src e in
  let (v2,_) = Topology.edge_dst e in
  Printf.sprintf "%s--%s"
    (name_of_vertex topo v1)
    (name_of_vertex topo v2)

let string_of_pair topo (s,t) =
  Printf.sprintf "%s--%s"
    (name_of_vertex topo s)
    (name_of_vertex topo t)

(* Given an edge (i,j) and a source-sink pair (s,t),
 * returns the name of the variable representing the
 * flow on (i,j) originating from s and going to t. *)
let var_name topo edge d_pair =
  let src,_ = Topology.edge_src edge in
  let dst,_ = Topology.edge_dst edge in
  Printf.sprintf "f_%s_%s--%s"
    (string_of_pair topo d_pair)
    (name_of_vertex topo src)
    (name_of_vertex topo dst)

(* Same as above, but for the flow in the reverse direction (j,i). *)
let var_name_rev topo edge d_pair =
  let src,_ = Topology.edge_src edge in
  let dst,_ = Topology.edge_dst edge in
  Printf.sprintf "f_%s_%s--%s"
    (string_of_pair topo d_pair)
    (name_of_vertex topo dst)
    (name_of_vertex topo src)

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
	                       (* let reverse_amt = var_name_rev topo edge pair in *)
                           Var(forward_amt):: (* Var(reverse_amt):: *) acc2) d_pairs                       
      in
      (* Add them all up *)
      let total_flow = Sum (all_flows) in
      let scaled_cap = Times (capacity_of_edge topo edge, objective) in
      (* Total flow is at most the scaled capacity *)
      let constr = minus total_flow scaled_cap in
      let name = Printf.sprintf "cap_%s"
          (string_of_edge topo edge) in
      (Leq (name, constr, 0.))::acc) topo init_acc

let neighboring_edges topo src =
  let src_neighbors = Topology.neighbors topo src in
  (* Get all outgoing edges *)
  let edges = VertexSet.fold src_neighbors ~init:[] ~f:(fun acc vtx ->
      let es = Topology.find_all_edges topo src vtx in
      List.rev_append (EdgeSet.elements es) acc) in
  edges

let demand_constraints (topo : Topology.t) (d_pairs : demands)
    (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has a demand constraint *)
  SrcDstMap.fold ~init:init_acc ~f:(fun ~key:(src,dst) ~data:(demand) acc ->
      (* We need to add up the rates for all edges adjoining the source *)
      let edges = neighboring_edges topo src in
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

let rec string_of_aexp ae =
  match ae with
  | Var v -> v
  | Num f -> Float.to_string f
  | Times (coeff, a2) ->
    Printf.sprintf "%f %s" (coeff) (string_of_aexp a2)
  | Sum (aexs) ->
    List.fold_left aexs ~init:"" ~f:(fun acc ae ->
        if acc = "" then string_of_aexp ae else match ae with
          | Times (coeff, a2) ->
            if coeff = -1. then acc ^ " - " ^ (string_of_aexp a2)
            else if coeff < 0. then acc ^ " - " ^
                                   (string_of_aexp (Times (-.coeff, a2)))
            else acc ^ " + " ^ (string_of_aexp ae)
          | _ -> acc ^ " + " ^ (string_of_aexp ae))

let string_of_constraint c =
  match c with
  | Eq (name, ae, f) ->
    Printf.sprintf "%s: %s = %s" name (string_of_aexp ae) (Float.to_string f)
  | Leq (name, ae, f) ->
    Printf.sprintf "%s: %s <= %s" name (string_of_aexp ae) (Float.to_string f)
  | Geq (name, ae, f) ->
    Printf.sprintf "%s: %s >= %s" name (string_of_aexp ae) (Float.to_string f)

let serialize_lp ((objective, constrs) : lp) (filename : string) =
  let open Out_channel in
  let lp_file = create filename in
  output_string lp_file "Minimize\n";
  output_string lp_file (Printf.sprintf "  %s\n" (string_of_aexp objective));
  output_string lp_file "Subject To\n";
  List.iter constrs ~f: (fun c -> output_string lp_file
                (Printf.sprintf "  %s\n" (string_of_constraint c)));
  close lp_file

let lp_of_graph (topo : Topology.t) (demand_pairs : demands) =
  let cap_constrs = capacity_constraints topo demand_pairs [] in
  let cap_and_demand = demand_constraints topo demand_pairs cap_constrs in
  let all_constrs = conservation_constraints topo demand_pairs cap_and_demand in
  (objective, all_constrs)

type flow_table = ((Node.t * Node.t),
                   (Node.t * Node.t * float) list) Hashtbl.t

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
      let path_opt = UnitPath.shortest_path path_topo src_id dst_id in
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
    paths in
    (* For every commodity, get their paths. *)
    Hashtbl.fold 
      flow_table 
      ~init:SrcDstMap.empty 
      ~f:(fun ~key:d_pair ~data:edges acc ->
          let (s,t) = d_pair in
          let s_v = Topology.vertex_of_label orig_topo s in
          let t_v = Topology.vertex_of_label orig_topo t in
          let paths = strip_paths (s, t) edges in
          let p = 
	    List.fold_left 
	      paths 
	      ~init:PathMap.empty
	      (* TODO(rjs,rdk): the weight is wrong. *)
              ~f:(fun acc (path,scalar) -> PathMap.add acc path scalar) in
          SrcDstMap.add acc ~key:(s_v,t_v) ~data:p )
      

(* Run everything. Given a topology and a set of pairs with demands,
   returns the optimal congestion ratio, the paths used, and the number
   of paths used. *)
(* let solver_paths topo pairs verbose = *)
let solve (topo:topology) (pairs:demands) (s:scheme) : scheme =
  
  let name_table = Hashtbl.Poly.create () in
  Topology.iter_vertexes (fun vert ->
			  let label = Topology.vertex_to_label topo vert in
			  let name = Node.name label in
        Hashtbl.Poly.add_exn name_table name vert) topo;
  
  let lp = lp_of_graph topo pairs in
  serialize_lp lp "mcf.lp";
  
  let gurobi_in = Unix.open_process_in
		    "gurobi_cl OptimalityTol=1e-9 ResultFile=mcf.sol mcf.lp" in
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
    let ratio, flows = read_results "mcf.sol" in
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



       

                
