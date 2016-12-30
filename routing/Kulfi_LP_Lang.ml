open Core.Std
open Frenetic_Network
open Net

(* This is stripped down to cover only what we'll need for MCF *)

(* (s,t,r) = node s wants to send to node t at rate r *)
type demand_pair = Topology.vertex * Topology.vertex * float

type arith_exp =
  | Var of string
  | Num of float
  | Times of float * arith_exp
  | Sum of arith_exp list

type constrain =
  | Eq of string * arith_exp * float
  | Leq of string * arith_exp * float
  | Geq of string * arith_exp * float

type lp = arith_exp * (constrain list)

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

(* FFC: Create a LP variable for src->dst granted badnwidth *)
let granted_bw_var_name topo (src,dst) =
  Printf.sprintf "gbf_%s--%s"
    (name_of_vertex topo src)
    (name_of_vertex topo dst)


let string_of_lp ((objective, constrs) : lp) : string =
  let cs =
    List.fold_left
      constrs
      ~init:""
      ~f: (fun acc c -> acc ^ (Printf.sprintf "  %s\n" (string_of_constraint c))) in
  "Minimize\n" ^ (Printf.sprintf "  %s\n" (string_of_aexp objective))
  ^ "Subject To\n" ^ cs


let serialize_lp ((objective, constrs) : lp) (filename : string) =
  let open Out_channel in
  let lp_file = create filename in
  output_string lp_file "Minimize\n";
  output_string lp_file (Printf.sprintf "  %s\n" (string_of_aexp objective));
  output_string lp_file "Subject To\n";
  List.iter constrs ~f: (fun c -> output_string lp_file
                (Printf.sprintf "  %s\n" (string_of_constraint c)));
  close lp_file

let serialize_max_lp ((objective, constrs) : lp) (filename : string) =
  let open Out_channel in
  let lp_file = create filename in
  output_string lp_file "Maximize\n";
  output_string lp_file (Printf.sprintf "  %s\n" (string_of_aexp objective));
  output_string lp_file "Subject To\n";
  List.iter constrs ~f: (fun c -> output_string lp_file
                (Printf.sprintf "  %s\n" (string_of_constraint c)));
  close lp_file
