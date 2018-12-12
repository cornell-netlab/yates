open Core

open LP_Lang
open Util
open Yates_types.Types

let prev_scheme = ref SrcDstMap.empty

let objective = Var "Z"

(* Since Gurobi automatically sets lower bound to zero, we can avoid some
   constraints to be specified in the LP *)
let gurobi_auto_lb_zero = true

(* Functions on undirected links which are used for LP formulation *)
(* Define a link as a set of endpoints *)
type aclink = VertexSet.t [@@ deriving sexp]

module AclinkOrd = struct
  type t = aclink [@@ deriving sexp]
  let compare = Pervasives.compare
end
module AclinkSet = Set.Make(AclinkOrd)

let link_of (e : edge) : aclink =
  let v1 = (fst (Topology.edge_src e)) in
  let v2 = (fst (Topology.edge_dst e)) in
  VertexSet.add (VertexSet.singleton v1) v2

let edges_of (topo : topology) (l : aclink) : EdgeSet.t =
  match VertexSet.elements l with
  | u::v::[] ->
    EdgeSet.union
      (Topology.find_all_edges topo u v)
      (Topology.find_all_edges topo v u)
  | _ -> failwith "Invalid link"

let string_of_link (topo : topology) (l : aclink) : string =
  List.fold_left (VertexSet.to_list l) ~init:"link"
    ~f:(fun acc v -> acc ^ "--" ^ (name_of_vertex topo v))

let capacity_of_link (topo : topology) (l : aclink) : float =
  EdgeSet.fold (edges_of topo l) ~init:0.
    ~f:(fun acc e -> acc +. (capacity_of_edge topo e))

let all_topo_links (topo : topology) : AclinkSet.t =
  Topology.fold_edges
    (fun e acc -> AclinkSet.add acc (link_of e)) topo AclinkSet.empty

let scaled_capacity_of_link (topo : topology) (l : aclink) : float =
  (capacity_of_link topo l) /. cap_divisor

let var_pi (topo : topology) (l : aclink) (m : aclink) : string =
  "pi_" ^ (string_of_link topo l) ^ "_" ^ (string_of_link topo m)

let var_path_length (topo : topology) (l : aclink) i j : string =
  "pl_" ^ (string_of_link topo l)
  ^ "_" ^ (name_of_vertex topo i)
  ^ "_" ^ (name_of_vertex topo j)


(********************************************************************)

(* Routing constraints - *)
(* Eq 2 of "Optimal Oblivious Routing in Polynomial Time" *)
let routing_constraints (topo : Topology.t) (demand_pairs : demands)
  : constrain list =
  let non_neg_flow_constraints =
    if gurobi_auto_lb_zero then []
    else
      (* For every src, dst *)
      SrcDstMap.fold ~init:[]
        ~f:(fun ~key:(src,dst) ~data:_ sd_acc ->
            if src = dst then sd_acc else
              (* For every edge *)
              Topology.fold_edges
                (fun edge e_acc ->
                   (* Flow from src to dst on edge >= 0 *)
                   let flow_on_edge = var_name topo edge (src, dst) in
                   let name = Printf.sprintf "gez-%s" flow_on_edge in
                   (Geq (name, Var (flow_on_edge), 0.)::e_acc)
                ) topo sd_acc) demand_pairs in

  let source_constraints =
    (* For every src, dst:
     *   sum of src-dst flows on edges from src = 1 *)
    SrcDstMap.fold ~init:[]
      ~f:(fun ~key:(src,dst) ~data:_ sd_acc ->
          if src = dst then sd_acc else
          let out_src_edges = outgoing_edges topo src in
          let in_src_edges = incoming_edges topo src in
          let outgoing_flows = List.fold_left out_src_edges ~init:[]
              ~f:(fun out_acc edge ->
                  let out_amount = Var (var_name topo edge (src, dst)) in
                  out_amount::out_acc) in
          let incoming_flows = List.fold_left in_src_edges ~init:[]
              ~f:(fun in_acc edge ->
                  let in_amount = Var (var_name topo edge (src, dst)) in
                  in_amount::in_acc) in
          let net_outgoing = minus (Sum (outgoing_flows)) (Sum (incoming_flows)) in
          let name = Printf.sprintf "src-%s-%s" (name_of_vertex topo src)
              (name_of_vertex topo dst) in
          let src_constr = (Eq (name, net_outgoing, 1.)) in
          src_constr::sd_acc) demand_pairs in

  let conservation_constraints =
    (* For every src, dst and node v != src or dst:
     *   net src-dst flow entering v = net src-dst flow leaving v *)
    SrcDstMap.fold ~init:[]
      ~f:(fun ~key:(src,dst) ~data:_ sd_acc ->
          if src = dst then sd_acc
          else
            Topology.fold_vertexes (fun v v_acc ->
                if v = src || v = dst then v_acc
                else
                  let out_v_edges = outgoing_edges topo v in
                  let in_v_edges = incoming_edges topo v in
                  let outgoing_flows = List.fold_left out_v_edges ~init:[]
                      ~f:(fun out_acc edge ->
                          let out_amount =
                            Var (var_name topo edge (src, dst)) in
                          out_amount::out_acc) in
                  let incoming_flows = List.fold_left in_v_edges ~init:[]
                      ~f:(fun in_acc edge ->
                          let in_amount =
                            Var (var_name topo edge (src, dst)) in
                          in_amount::in_acc) in
                  let net_outgoing =
                    minus (Sum (outgoing_flows)) (Sum (incoming_flows)) in
                  let name = Printf.sprintf "con-%s-%s"
                      (name_of_vertex topo src) (name_of_vertex topo dst) in
                  let con_constr = (Eq (name, net_outgoing, 0.)) in
                  con_constr::v_acc) topo sd_acc) demand_pairs in
  (* concat all constraints *)
  non_neg_flow_constraints @ source_constraints @ conservation_constraints

(* Applegate-Cohen's LP constraints *)
let ac_lp_constraints (topo : Topology.t) (demand_pairs) =
  let aclinks = all_topo_links topo in
  let cong_cnstrs =
    AclinkSet.fold aclinks ~init:[] ~f:(fun l_acc l ->
        let cap_m_pi_lm =
          AclinkSet.fold aclinks ~init:[] ~f:(fun m_acc m ->
              let cap_m = scaled_capacity_of_link topo m in
              let pi_lm = var_pi topo l m in
              Times (cap_m, Var pi_lm)::m_acc) in
        let constr = minus (Sum cap_m_pi_lm) objective in
        let name = Printf.sprintf "cong-%s" (string_of_link topo l) in
        (Leq (name, constr, 0.))::l_acc) in

  let fcp_cnstrs =
    AclinkSet.fold aclinks ~init:[] ~f:(fun l_acc l ->
        SrcDstMap.fold demand_pairs ~init:l_acc
          ~f:(fun ~key:(i,j) ~data:_ ij_acc ->
              if i = j then ij_acc
              else
                let f_ij_l = Sum (EdgeSet.fold (edges_of topo l) ~init:[]
                    ~f:(fun acc e -> Var (var_name topo e (i,j))::acc)) in
                let cap_l = scaled_capacity_of_link topo l in
                let pl_ij = Var (var_path_length topo l i j) in
                let constr = minus f_ij_l (Times (cap_l, pl_ij)) in
                let name = Printf.sprintf "fcp-%s-%s-%s"
                    (string_of_link topo l)
                    (name_of_vertex topo i)
                    (name_of_vertex topo j) in
                (Leq (name, constr, 0.))::ij_acc)) in

  let pipath_cnstrs =
    AclinkSet.fold aclinks ~init:[]
      ~f:(fun l_acc l ->
          Topology.fold_vertexes (fun i i_acc ->
            Topology.fold_edges (fun e e_acc ->
                let j = (fst (Topology.edge_src e)) in
                let k = (fst (Topology.edge_dst e)) in
                let pi_le = Var (var_pi topo l (link_of e)) in
                let diff_pl = minus (Var (var_path_length topo l i j))
                    (Var (var_path_length topo l i k)) in
                let constr = Sum ([pi_le; diff_pl]) in
                let name =  Printf.sprintf "pipath-%s-%s-%s"
                    (string_of_link topo l)
                    (name_of_vertex topo i)
                    (string_of_edge topo e) in
                (Geq (name, constr, 0.))::e_acc)
              topo i_acc)
          topo l_acc) in

  let pi_gez_cnstrs =
    if gurobi_auto_lb_zero then []
    else
      AclinkSet.fold aclinks ~init:[] ~f:(fun l_acc l ->
          AclinkSet.fold aclinks ~init:l_acc ~f:(fun m_acc m ->
              let pi_lm = Var (var_pi topo l m) in
              let name =  Printf.sprintf "pi-%s-%s"
                  (string_of_link topo l) (string_of_link topo m) in
              (Geq (name, pi_lm, 0.))::m_acc)) in

  let path_length_cnstrs =
    AclinkSet.fold aclinks ~init:[]
      ~f:(fun l_acc l ->
          Topology.fold_vertexes
            (fun i i_acc ->
               Topology.fold_vertexes
                 (fun j j_acc ->
                    let pl_ij = Var (var_path_length topo l i j) in
                    let name = Printf.sprintf "pl-%s-%s-%s"
                        (string_of_link topo l)
                        (name_of_vertex topo i)
                        (name_of_vertex topo j) in
                    if i = j then (Eq (name, pl_ij, 0.))::j_acc
                    else if gurobi_auto_lb_zero then j_acc
                    else (Geq (name, pl_ij, 0.))::j_acc)
                 topo i_acc)
            topo l_acc) in
  (* Concat all the constraints *)
  cong_cnstrs @
  fcp_cnstrs @
  pipath_cnstrs @
  pi_gez_cnstrs @
  path_length_cnstrs

(* Applegate-Cohen's LP formulation *)
let ac_lp_of_graph (topo : Topology.t) =
  (* Create a SrcDstMap of src-dst pairs *)
  let hosts = get_hosts topo in
  let demand_pairs =
    List.fold_left hosts ~init:SrcDstMap.empty
      ~f:(fun u_acc u ->
          List.fold_left hosts ~init:u_acc
            ~f:(fun v_acc v ->
                let dem =
                  if u = v then 0.
                  else 1. in
                SrcDstMap.set v_acc ~key:(u,v) ~data:dem)) in
  let routing_constrs = routing_constraints topo demand_pairs in
  let ac_lp_constrs = ac_lp_constraints topo demand_pairs in
  (objective, routing_constrs@ac_lp_constrs)

let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "/tmp/ac_%f.lp" rand) in
  match Sys.file_exists try_fn with
    `Yes -> new_rand ()
  | _ -> rand

(* Given a topology, returns an oblivious routing scheme with optimal oblivious
   congestion ratio. *)
let solve (topo:topology) (_:demands) : scheme =
  (* TODO: handle edge router constrains *)
  if !Globals.er_mode then failwith "Not implemented" else
  let new_scheme =
    if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
    else
      let name_table = Hashtbl.Poly.create () in
      Topology.iter_vertexes (fun vert ->
          let label = Topology.vertex_to_label topo vert in
          let name = Node.name label in
          Hashtbl.Poly.add_exn name_table name vert) topo;

      let lp = ac_lp_of_graph topo in
      let rand = new_rand () in
      let lp_filename = (Printf.sprintf "/tmp/ac_%f.lp" rand) in
      let lp_solname = (Printf.sprintf "/tmp/ac_%f.sol" rand) in

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

        let result = read results 0. [] in
        In_channel.close results; result in
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

      Mcf.recover_paths topo flows_table in
  prev_scheme := new_scheme;
  new_scheme

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = normalization_recovery
