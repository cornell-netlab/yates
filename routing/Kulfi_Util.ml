open Array
open Core.Std
open Frenetic_Network
open Kulfi_Types
open Net
open Net.Topology

(********************* Helper functions **************)

(************** to string *******)
let string_of_vertex (t:topology) v : string =
  Printf.sprintf "%s"
                    (Node.name (Net.Topology.vertex_to_label t v))

let string_of_edge (t:topology) (e:edge) : string =
  Printf.sprintf "(%s,%s)"
                    (string_of_vertex t (fst (Net.Topology.edge_src e)))
                    (string_of_vertex  t (fst (Net.Topology.edge_dst e)))

let edge_to_string_map (t:topology) : string EdgeMap.t =
  let edge_list = EdgeSet.elements (Topology.edges t) in
  List.fold_left edge_list
  ~init:EdgeMap.empty
  ~f:(fun acc e ->
    let edge_str = (string_of_edge t e) in
    EdgeMap.add acc ~key:e ~data:edge_str)


let string_to_edge_map (t:topology) : edge Kulfi_Types.StringMap.t =
  let edge_list = EdgeSet.elements (Topology.edges t) in
  List.fold_left edge_list
  ~init:StringMap.empty
  ~f:(fun acc e -> StringMap.add acc ~key:(string_of_edge t e) ~data:e)

let dump_path_prob_set (t:topology) (pps:probability PathMap.t) : string =
  let buf = Buffer.create 101 in
  PathMap.iteri
    pps
    ~f:(fun ~key:path ~data:prob -> Printf.bprintf buf "[%s] @ %f\n" (dump_edges t path) prob);
  Buffer.contents buf

let dump_scheme (t:topology) (s:scheme) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iteri s ~f:(fun ~key:(v1,v2) ~data:pps ->
                       Printf.bprintf buf "%s -> %s :\n  %s\n"
                                      (Node.name (Net.Topology.vertex_to_label t v1))
                                      (Node.name (Net.Topology.vertex_to_label t v2))
                                      (dump_path_prob_set t pps));
  Buffer.contents buf

let dump_demands (t:topology) (d:demands) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iteri d
  ~f:(fun ~key:(src,dst) ~data:dem ->
    Printf.bprintf buf "(%s,%s) : %f\n"
                     (Node.name (Net.Topology.vertex_to_label t src))
                     (Node.name (Net.Topology.vertex_to_label t dst))
  dem;);
  Buffer.contents buf

let write_to_file filename text_to_write =
  try
    let cout = Out_channel.create filename in
    let co = Format.formatter_of_out_channel cout in
    Format.fprintf co "%s\n" text_to_write;
    Out_channel.close cout
  with _ as e ->
    Format.printf "Cannot open file \"%s\": %s\n" filename (Exn.to_string e)



(********* Lists ***********)
let list_last l = match l with
  | hd::tl -> List.fold_left ~f:(fun _ x -> x) ~init:hd tl
  | []    -> failwith "no element"

let rec list_next l elem = match l with
  (* Assumes no duplicate elems in list *)
  | hd::tl -> if hd = elem then List.hd tl
              else list_next tl elem
  | [] -> failwith "not found"

let rec next_hop (t:topology) (p:path) (e:edge) = match p with
  (* Assumes no duplicate elems in list *)
  | hd::tl -> if (string_of_edge t hd) = (string_of_edge t e) then List.hd tl
              else next_hop t tl e
  | [] -> failwith "not found"

let split_alist (l:('a * 'b) list) : ('a list * 'b list) =
  let k,v = List.fold l ~init:([],[])
    ~f:(fun acc (k,v) ->
      let ak,av = acc in
      (k::ak, v::av)) in
  (List.rev k, List.rev v)

let average_list l =
  (List.fold_left ~f:(+.) ~init:0. l) /. (float_of_int (List.length l))

let rec max_list = function
  | [] -> failwith "empty list"
  | [h] -> h
  | h::t -> max h (max_list t)


let next_hop_arr (p:edge Array.t) (dist:int) : edge option =
  if Array.length p <= dist then None
  else Some (p.(dist))

(* sublist containing first n elements *)
let rec list_first_n l n =
  if n = 0 then []
  else match l with
  | hd::tl -> hd::(list_first_n tl (n-1))
  | [] -> failwith "not enough elements"


(******************* Routing ****************)
(* prune a scheme by limiting number of s-d paths within a given budget *)
let prune_scheme (t:topology) (s:scheme) (budget:int) : scheme =
  let new_scheme = SrcDstMap.fold s
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
      if src = dst then acc
      else
        let pruned_paths = if PathMap.length paths <= budget then paths
              else
                let pplist = PathMap.to_alist paths in
                let sorted_pp = List.sort ~cmp:(fun x y -> Float.compare (snd y) (snd x)) pplist in
                let top_pp = list_first_n sorted_pp budget in
                let total_prob = List.fold_left top_pp ~init:0.0 ~f:(fun acc (_,prob) -> acc +. prob) in
                List.fold_left top_pp
                  ~init:PathMap.empty
                  ~f:(fun acc (path,prob) -> PathMap.add acc ~key:path ~data:(prob /. total_prob))
                in
        SrcDstMap.add acc ~key:(src,dst) ~data:pruned_paths) in
  assert (probabilities_sum_to_one new_scheme);
  assert (all_pairs_connectivity t (get_hosts t) new_scheme);
  new_scheme

