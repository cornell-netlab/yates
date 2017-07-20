open Core
open Frenetic_Network
open Net

open Kulfi_Types

let reverse_edge_exn topo e = match Topology.inverse_edge topo e with
          | Some x -> x
          | None -> assert false

let intercalate f s = function
  | [] ->
    ""
  | h::t ->
      List.fold_left t ~f:(fun acc x -> acc ^ s ^ f x) ~init:(f h)

let dump_edges (t:topology) (es:path) : string =
  intercalate
    (fun e ->
     Printf.sprintf "(%s,%s)"
          (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_src e))))
          (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_dst e))))) ", "  es

let dump_topology (t:topology) : string =
  let buf = Buffer.create 101 in
  Printf.bprintf buf "V: [ ";
  Topology.iter_vertexes
    (fun v ->
      Printf.bprintf buf "%s " (Node.name (Net.Topology.vertex_to_label t v)))
    t;
  Printf.bprintf buf "]\nE: [ ";
  Topology.iter_edges
    (fun e -> Printf.bprintf buf "(%s,%s) "
      (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_src e))))
      (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_dst e)))))
    t;
  Printf.bprintf buf "]\n";
  Buffer.contents buf

let compare_scheme (s1:scheme) (s2:scheme) : int = assert false

let add_or_increment_path (fd : flow_decomp) (p : path) (r : probability) : flow_decomp =
  let new_value = match PathMap.find fd p with
    | None -> r
    | Some prior_value -> prior_value +. r in
  PathMap.add ~key:p ~data:new_value fd


(* The following stuff was moved from Kulfi_Mcf.ml to here
   so that it could be used in Kulfi_Ak.ml. It doesn't really
   belong in Kulfi_Types.ml, we should move it somewhere else
   in a future re-factoring of the code. *)

(* convert to Mbps for input to Gurobi *)
let cap_divisor = 1000000.
let demand_divisor = 1000000.


let edge_connects_switches (e:edge) (topo:topology) : bool =
  let src,_ = Topology.edge_src e in
  let src_label = Topology.vertex_to_label topo src in
  let dst,_ = Topology.edge_dst e in
  let dst_label = Topology.vertex_to_label topo dst in
  Node.device src_label = Node.Switch && Node.device dst_label = Node.Switch


let capacity_of_edge topo edge =
  let label = Topology.edge_to_label topo edge in
  let cap = (Int64.to_float (Link.capacity label)) in
  if edge_connects_switches edge topo then cap
  else 100. *. cap

let source_routing_configuration_of_scheme (topo:topology) (scm:scheme) (tag_hash: (edge,int) Hashtbl.t) : configuration =
  SrcDstMap.fold scm
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
        if src = dst then acc
        else
          let tags = PathMap.fold paths ~init:TagsMap.empty
            ~f:(fun ~key:path ~data:prob acc ->
              match path with
                | [] -> assert false
                | _::path' ->
                  let tags = List.filter ~f:(fun x -> x <> 99)
                    (List.map path' ~f:(fun edge ->
                      match Hashtbl.find tag_hash edge with
                        | None ->
                          Printf.printf "Couldn't find %s\n" (dump_edges topo [edge]);
                          99
                        | Some t -> t)) in
                  TagsMap.add acc ~key:tags ~data:prob) in
          SrcDstMap.add acc ~key:(src,dst) ~data:tags)

let path_routing_configuration_of_scheme (topo:topology) (scm:scheme) (path_tag_map: int PathMap.t) : configuration =
  SrcDstMap.fold scm
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:path_prob_map acc ->
        if src = dst then acc
        else
          let tag_prob_map = PathMap.fold path_prob_map ~init:TagsMap.empty
            ~f:(fun ~key:path ~data:prob acc ->
              let tag = PathMap.find_exn path_tag_map path in
              TagsMap.add acc ~key:([tag]) ~data:prob) in
          SrcDstMap.add acc ~key:(src,dst) ~data:tag_prob_map)


let bprint_tags (buf:Buffer.t) (tag_dist:probability TagsMap.t) : unit =
  TagsMap.iteri
    tag_dist
    ~f:(fun ~key:tags ~data:prob ->
        Printf.bprintf buf "%d " (Float.to_int (1000.0 *. prob));
        Printf.bprintf buf "%d " (List.length tags);
        List.iter tags (Printf.bprintf buf "%d "))

let bprint_configuration (topo:topology) (bufs:(Topology.vertex,Buffer.t) Hashtbl.t) (conf:configuration) : unit =
  let dstCount =
  SrcDstMap.fold
    conf
    ~init:VertexMap.empty
    ~f:(fun ~key:(src, dst) ~data:tag_dist acc ->
	let count =
	  match VertexMap.find acc src with
	  | None -> 0
          | Some x -> x
        in
     VertexMap.add acc ~key:src ~data:(count+1);
     ) in
  SrcDstMap.iteri
    conf
    ~f:(fun ~key:(src,dst) ~data:tag_dist ->
        let buf =
          match Hashtbl.find bufs src with
          | Some buf -> buf
          | None ->
             let buf = Buffer.create 101 in
             Hashtbl.add_exn bufs src buf;
             let count =
               match VertexMap.find dstCount src with
               | None -> 0
               | Some x -> x
             in
             Printf.bprintf buf "%d " count;
             buf in
        Printf.bprintf buf "%lu " (Node.ip (Topology.vertex_to_label topo dst));
        Printf.bprintf buf "%d " (TagsMap.length tag_dist);
        bprint_tags buf tag_dist)

let print_configuration (topo:topology) (conf:configuration) (time:int) : unit =
  let bufs = Hashtbl.Poly.create () in
  bprint_configuration topo bufs conf;
  Hashtbl.Poly.iteri
    bufs
    ~f:(fun ~key:src ~data:buf ->
	let route_filename = Printf.sprintf "routes/%s_%d" (Frenetic_Packet.string_of_ip (Node.ip (Topology.vertex_to_label topo src))) time in
	let route_file = Out_channel.create route_filename in
	Out_channel.output_string route_file (Buffer.contents buf);
	Out_channel.close route_file;
	)

let normalize_scheme (s : scheme) (fs: float SrcDstMap.t) : scheme =
  (* s = a routing scheme, fs = the sum of flow values in each flow_decomp *)
  SrcDstMap.fold ~init:(SrcDstMap.empty)
    ~f:(fun ~key:(u,v) ~data:f_decomp acc  ->
      let sum_rate = match SrcDstMap.find fs (u,v) with
	| None ->
	   ( PathMap.fold f_decomp
	       ~init:0.
	       ~f:(fun ~key:_ ~data:r acc -> acc +. r) )
	| Some sr -> sr in
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
	     PathMap.add ~key:path ~data:normalized_rate acc)
	     f_decomp in
       SrcDstMap.add ~key:(u,v) ~data:normalized_f_decomp acc) s

let normalize_scheme_opt (s:scheme) : scheme =
  let zero_sum = SrcDstMap.fold s
    ~init:SrcDstMap.empty
    ~f:(fun ~key:sd ~data:_ acc ->
      SrcDstMap.add ~key:sd ~data:0. acc) in
    normalize_scheme s zero_sum

(* Modify routing scheme by normalizing path probabilites while avoiding failed links *)
let normalization_recovery (curr_scheme:scheme) (_:topology) (failed_links:failure) (_:demands) : scheme =
  Printf.printf "\t\t\t\t\t\t\t\t\t   L-REC\r";
  let new_scheme = SrcDstMap.fold curr_scheme
  ~init:SrcDstMap.empty
  ~f:(fun ~key:(src,dst) ~data:paths acc ->
    let is_path_alive (p:path) : bool =
      List.fold_left p
      ~init:true
      ~f:(fun valid edge ->
        let edge_is_safe = not (EdgeSet.mem failed_links edge) in
        valid && edge_is_safe) in
    let n_paths = PathMap.filteri ~f:(fun ~key:p ~data:_ -> is_path_alive p) paths in
    let total_prob = PathMap.fold n_paths
      ~init:0.0
      ~f:(fun ~key:_ ~data:prob acc -> acc +. prob) in

    let renormalized_paths = PathMap.fold n_paths
      ~init:PathMap.empty
      ~f:(fun ~key:path ~data:prob acc ->
        let new_prob = if (total_prob = 0.)
                       then 1. /. (Float.of_int (PathMap.length n_paths))
                       else prob /. total_prob in
        PathMap.add ~key:path ~data:new_prob acc) in
    SrcDstMap.add ~key:(src,dst) ~data:renormalized_paths acc) in
  new_scheme

(* Set of host nodes *)
let get_hosts_set (topo:topology) : VertexSet.t =
  VertexSet.filter (Topology.vertexes topo)
  ~f:(fun v ->
    let label = Topology.vertex_to_label topo v in
    Node.device label = Node.Host)

(* List of host nodes *)
let get_hosts (topo:topology) =
  let host_set = get_hosts_set topo in
  Topology.VertexSet.elements host_set

(* List of src-dst pairs. src != dst *)
let get_srcdst_pairs (topo:topology) =
  let hosts = get_hosts topo in
  List.fold_left hosts ~init:[] ~f:(fun acc u ->
    List.fold_left hosts ~init:acc ~f:(fun acc v ->
      if u = v then acc else
      (u,v)::acc))

(* Check if all src-dst pairs are connected with given scheme *)
let all_pairs_connectivity topo hosts scheme : bool =
  List.fold_left hosts
    ~init:true
    ~f:(fun acc u ->
      List.fold_left hosts
        ~init:acc
        ~f:(fun acc v ->
          if u = v then acc
          else
            match SrcDstMap.find scheme (u,v) with
            | None -> Printf.printf "No route for pair (%s, %s)\n%!"
             (Node.name (Net.Topology.vertex_to_label topo u))
             (Node.name (Net.Topology.vertex_to_label topo v)); false
            | Some paths -> not (PathMap.is_empty paths)  && acc))


let paths_are_nonempty (s:scheme) : bool =
  SrcDstMap.fold s
    ~init:true
    (* for every pair of hosts u,v *)
    ~f:(fun ~key:(u,v) ~data:paths acc ->
      if u = v then true && acc
      else
        PathMap.fold paths
          ~init:acc
          (* get the possible paths, and for every path *)
          ~f:(fun ~key:path ~data:_ acc ->
            acc && (not (List.is_empty path))))

let probabilities_sum_to_one (s:scheme) : bool =
  SrcDstMap.fold s
    ~init:true
    ~f:(fun ~key:(u,v) ~data:f_decomp acc ->
      if u = v then acc
      else
        let sum_rate =
          PathMap.fold f_decomp
            ~init:0.
            ~f:(fun ~key:path ~data:r acc -> acc +. r) in
        acc && (sum_rate > 1.-.1e-4) && (sum_rate < 1.+.1e-4) )


(* Latency for a path *)
let get_path_weight (topo:topology) (p:path) : float =
  List.fold_left p ~init:0.
    ~f:(fun acc e -> acc +. Link.weight (Topology.edge_to_label topo e))

let get_path_weight_arr (topo:topology) (p:edge Array.t) =
  Array.foldi p ~init:0.
    ~f:(fun _ acc e -> acc +. Link.weight (Topology.edge_to_label topo e))


let rec range i j = if i >= j then [] else i :: (range (i+1) j)
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


let string_to_edge_map (t:topology) : edge StringMap.t =
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
      if src = dst then acc else
      let pruned_paths =
        if PathMap.length paths <= budget then paths
        else
          let pplist = PathMap.to_alist paths in
          let sorted_pp = List.sort ~cmp:(fun x y -> Float.compare (snd y) (snd x)) pplist in
          let top_pp = list_first_n sorted_pp budget in
          let total_prob = List.fold_left top_pp ~init:0.0 ~f:(fun acc (_,prob) -> acc +. prob) in
          List.fold_left top_pp ~init:PathMap.empty ~f:(fun acc (path,prob) ->
              PathMap.add acc ~key:path ~data:(prob /. total_prob)) in
      SrcDstMap.add acc ~key:(src,dst) ~data:pruned_paths) in
  (*Printf.printf "\nWARN: Disabled scheme check assertions for FFC while pruning\n";*)
  (*assert (probabilities_sum_to_one new_scheme);*)
  (*assert (all_pairs_connectivity t (get_hosts t) new_scheme);*)
  new_scheme


(**************** Failures ************)
let bidir_failure topo e =
  let e' = reverse_edge_exn topo e in
  EdgeSet.add (EdgeSet.singleton e) e'

let update_topo_with_failure (t:topology) (f:failure) : topology =
  EdgeSet.fold f ~init:t
    ~f:(fun acc link -> Topology.remove_edge acc link)

(* Solve SPF *)
let solve_spf (topo:topology) (d:demands) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
    ~f:(fun x y ->
          (match (device x, device y) with | (Node.Host,Node.Host) -> true | _ -> false)
    ) in
  List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) ->
    SrcDstMap.add acc ~key:(v1,v2) ~data:( PathMap.singleton p 1.0) )


(* check all-pairs connectivity after failure *)
let check_connectivity_after_failure (topo:topology) (fail:failure) : bool =
  let topo' = update_topo_with_failure topo fail in
  let hosts = get_hosts topo' in
  solve_spf topo' SrcDstMap.empty
    |> all_pairs_connectivity topo' hosts


(* Compute all possible failure scenarios with num_failures link failing, while not partitioning the network *)
let get_all_possible_failures (topo:topology) (num_failures:int) : (failure List.t) =
  (* List of single link failures *)
  if num_failures = 0 then failwith "Number of failures should be > 0" else
  let all_single_failures =
    EdgeSet.fold (Topology.edges topo) ~init:[]
      ~f:(fun acc e ->
        if not (edge_connects_switches e topo) then acc
        else
          let fl = bidir_failure topo e in
          if List.mem ~equal:EdgeSet.equal acc fl then acc
          else fl::acc)
    |> List.filter ~f:(fun fl -> check_connectivity_after_failure topo fl) in
  List.fold_left (range 1 num_failures) ~init:all_single_failures
    ~f:(fun partial_acc i ->
      List.fold_left partial_acc ~init:[]
        ~f:(fun acc partial_fl ->
          List.fold_left all_single_failures ~init:acc
            ~f:(fun acc single_fl ->
              if EdgeSet.is_subset single_fl partial_fl then acc
              else
                let new_failure = EdgeSet.union partial_fl single_fl in
                if List.mem ~equal:EdgeSet.equal acc new_failure then acc else
                if check_connectivity_after_failure topo new_failure then new_failure::acc
                else acc)))

(* Find all outgoing edges from a node `src` *)
let outgoing_edges topo src =
  let src_neighbors = Topology.neighbors topo src in
  (* Get all outgoing edges *)
  let edges = VertexSet.fold src_neighbors ~init:[] ~f:(fun acc vtx ->
      let es = Topology.find_all_edges topo src vtx in
      List.rev_append (EdgeSet.elements es) acc) in
  edges

(* Find all incoming edges to a node `dst` *)
let incoming_edges topo dst =
  let dst_neighbors = Topology.neighbors topo dst in
  (* Get all incoming edges *)
  let edges = VertexSet.fold dst_neighbors ~init:[] ~f:(fun acc vtx ->
      let es = Topology.find_all_edges topo vtx dst in
      List.rev_append (EdgeSet.elements es) acc) in
  edges

(* Find a host's neighboring switch *)
let ingress_switch topo host =
  let label = Topology.vertex_to_label topo host in
  assert (Node.device label = Node.Host);
  VertexSet.choose_exn (Topology.neighbors topo host)
