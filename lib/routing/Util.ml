open Core

open Yates_types.Types

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
          (Node.name (Topology.vertex_to_label t (fst (Topology.edge_src e))))
          (Node.name (Topology.vertex_to_label t (fst (Topology.edge_dst e))))) ", "  es

let dump_topology (t:topology) : string =
  let buf = Buffer.create 101 in
  Printf.bprintf buf "V: [ ";
  Topology.iter_vertexes
    (fun v ->
      Printf.bprintf buf "%s " (Node.name (Topology.vertex_to_label t v)))
    t;
  Printf.bprintf buf "]\nE: [ ";
  Topology.iter_edges
    (fun e -> Printf.bprintf buf "(%s,%s) "
      (Node.name (Topology.vertex_to_label t (fst (Topology.edge_src e))))
      (Node.name (Topology.vertex_to_label t (fst (Topology.edge_dst e)))))
    t;
  Printf.bprintf buf "]\n";
  Buffer.contents buf


(* The following stuff was moved from Yates_Mcf.ml to here
   so that it could be used in Yates_Ak.ml. It doesn't really
   belong in Yates_Types.ml, we should move it somewhere else
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
  else if !Globals.er_mode then cap
  else 100. *. cap

let normalize_scheme_fs (s : scheme) (fs: float SrcDstMap.t) : scheme =
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
            PathMap.set ~key:path ~data:normalized_rate acc)
          f_decomp in
      SrcDstMap.set ~key:(u,v) ~data:normalized_f_decomp acc) s

let normalize_scheme_opt (s:scheme) : scheme =
  let zero_sum =
    SrcDstMap.fold s ~init:SrcDstMap.empty
      ~f:(fun ~key:sd ~data:_ acc ->
        SrcDstMap.set ~key:sd ~data:0. acc) in
  normalize_scheme_fs s zero_sum

(* Normalize path weights so that the weights sum to 1 *)
let normalize_paths_prob paths =
  let total_prob = PathMap.fold paths ~init:0.0
      ~f:(fun ~key:_ ~data:prob acc -> acc +. prob) in
  PathMap.fold paths ~init:PathMap.empty
    ~f:(fun ~key:path ~data:prob acc ->
      let new_prob =
        if (total_prob = 0.) then
          1. /. (Float.of_int (PathMap.length paths))
        else
          prob /. total_prob in
      PathMap.set ~key:path ~data:new_prob acc)

(* Normalize path weights for the routing scheme *)
let normalize_scheme sch =
  SrcDstMap.fold sch ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
      SrcDstMap.set ~key:(src,dst) ~data:(normalize_paths_prob paths) acc)

(* Modify routing scheme by normalizing path probabilites while avoiding failed links *)
let normalization_recovery (curr_scheme:scheme) (_:topology)
    (failed_links:failure) (_:demands) : scheme =
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
    let renormalized_paths = normalize_paths_prob n_paths in
    SrcDstMap.set ~key:(src,dst) ~data:renormalized_paths acc) in
  new_scheme

(* Set of switch nodes *)
let get_switch_set (topo:topology) : VertexSet.t =
  VertexSet.filter (Topology.vertexes topo)
  ~f:(fun v ->
    let label = Topology.vertex_to_label topo v in
    Node.device label = Node.Switch)


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

(* check if a vertex u is one of edge e's endpoints *)
let is_incident (e:Topology.edge) (u:Topology.vertex) : bool =
  u = (fst (Topology.edge_src e)) || u = (fst (Topology.edge_dst e))

(* Latency for a path *)
let get_path_weight (topo:topology) (p:path) : float =
  List.fold_left p ~init:0.
    ~f:(fun acc e -> acc +. Link.weight (Topology.edge_to_label topo e))

let get_path_weight_arr (topo:topology) (p:edge Array.t) =
  Array.foldi p ~init:0.
    ~f:(fun _ acc e -> acc +. Link.weight (Topology.edge_to_label topo e))

let rec range i j = if i >= j then [] else i :: (range (i+1) j)

(***********************************************)
(* to string *)
(***********************************************)
let string_of_vertex (t:topology) v : string =
  Printf.sprintf "%s"
                    (Node.name (Topology.vertex_to_label t v))

let string_of_edge (t:topology) (e:edge) : string =
  Printf.sprintf "(%s,%s)"
                    (string_of_vertex t (fst (Topology.edge_src e)))
                    (string_of_vertex  t (fst (Topology.edge_dst e)))

let edge_to_string_map (t:topology) : string EdgeMap.t =
  let edge_list = EdgeSet.elements (Topology.edges t) in
  List.fold_left edge_list
  ~init:EdgeMap.empty
  ~f:(fun acc e ->
    let edge_str = (string_of_edge t e) in
    EdgeMap.set acc ~key:e ~data:edge_str)


let string_to_edge_map (t:topology) : edge StringMap.t =
  let edge_list = EdgeSet.elements (Topology.edges t) in
  List.fold_left edge_list
  ~init:StringMap.empty
  ~f:(fun acc e -> StringMap.set acc ~key:(string_of_edge t e) ~data:e)

let dump_path_prob_set (t:topology) (pps:probability PathMap.t) : string =
  let buf = Buffer.create 101 in
  let sorted_paths = PathMap.keys pps
                     |> List.sort ~compare:(fun p1 p2 ->
                       Pervasives.compare
                         (get_path_weight t p1) (get_path_weight t p2)) in
  List.iter sorted_paths
    ~f:(fun path -> let prob = PathMap.find_exn pps path in
         Printf.bprintf buf "[%s] @ %f\n" (dump_edges t path) prob);
  Buffer.contents buf

let dump_scheme (t:topology) (s:scheme) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iteri s ~f:(fun ~key:(v1,v2) ~data:pps ->
                       Printf.bprintf buf "%s -> %s :\n%s\n"
                                      (Node.name (Topology.vertex_to_label t v1))
                                      (Node.name (Topology.vertex_to_label t v2))
                                      (dump_path_prob_set t pps));
  Buffer.contents buf

let dump_demands (t:topology) (d:demands) : string =
  let buf = Buffer.create 101 in
  SrcDstMap.iteri d
  ~f:(fun ~key:(src,dst) ~data:dem ->
    Printf.bprintf buf "(%s,%s) : %f\n"
                     (Node.name (Topology.vertex_to_label t src))
                     (Node.name (Topology.vertex_to_label t dst))
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

let is_int v =
  let p = (Float.modf v) in
  let f = Float.Parts.fractional p in
  let c = Float.classify f in
  c = Float.Class.Zero

(* assumes l is sorted *)
let kth_percentile (l:float list) (k:float) : float =
  let n = List.length l in
  let x = (Float.of_int n) *. k in
  (*Printf.printf "%f / %d\n%!" x n;*)
  if n = 0 then 0. else
  if is_int x then
    let i = Int.of_float (Float.round_up x) in
    let lhs = match (List.nth l i) with
      | Some f -> f
      | None -> assert false in
    let rhs = match List.nth l (min (i+1) (n-1)) with
      | Some f -> f
      | None -> assert false in
    ((lhs +. rhs)/.2.)
  else
    let i = Int.of_float x in
    match (List.nth l i) with
    | Some f -> f
    | None -> assert false

let get_mean_congestion (l:float list) =
  (List.fold_left ~init:0. ~f:( +. )  l) /. (Float.of_int (List.length l))

let get_max_congestion (congestions:float list) : float =
  List.fold_left ~init:Float.nan ~f:(fun a acc -> Float.max_inan a acc) congestions

(**************************************************)
(* Topology helpers *)
(**************************************************)

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



(**************************************************)
(* Operations on routing scheme *)
(**************************************************)

let compare_scheme (s1:scheme) (s2:scheme) : int = assert false

let add_or_increment_path (fd : flow_decomp) (p : path) (r : probability) : flow_decomp =
  let new_value = match PathMap.find fd p with
    | None -> r
    | Some prior_value -> prior_value +. r in
  PathMap.set ~key:p ~data:new_value fd

(* Checks *)
(* Check if all src-dst pairs are connected with given scheme *)
let all_pairs_connectivity topo hosts scheme : bool =
  List.fold_left hosts
    ~init:true ~f:(fun acc u ->
      List.fold_left hosts
        ~init:acc ~f:(fun acc v ->
          if u = v then acc
          else
            match SrcDstMap.find scheme (u,v) with
            | None -> Printf.printf "No route for pair (%s, %s)\n%!"
                        (Node.name (Topology.vertex_to_label topo u))
                        (Node.name (Topology.vertex_to_label topo v));
              false
            | Some paths ->
              not (PathMap.is_empty paths)  && acc))


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


(* Check if the queried scheme contains all paths from the base scheme *)
let find_coverage (base:scheme) (query:scheme) : bool * float =
  let get_path_set sch : PathSet.t =
    SrcDstMap.fold sch ~init:PathSet.empty
      ~f:(fun ~key:uv ~data:ppm acc ->
        PathMap.fold ppm ~init:acc
          ~f:(fun ~key:path ~data:prob acc -> PathSet.add acc path)) in
  let base_paths = get_path_set base in
  let query_paths = get_path_set query in
  let diff = PathSet.diff base_paths query_paths in
  let diff_size = PathSet.length diff in
  if diff_size = 0 then (true, 100.)
  else
    let base_size = Float.of_int (PathSet.length base_paths) in
    let coverage = Float.((base_size - (of_int diff_size)) / base_size) in
    (false, coverage)

(* Modifications *)
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
          let sorted_pp = List.sort ~compare:(fun x y -> Float.compare (snd y) (snd x)) pplist in
          let top_pp = list_first_n sorted_pp budget in
          let total_prob = List.fold_left top_pp ~init:0.0 ~f:(fun acc (_,prob) -> acc +. prob) in
          List.fold_left top_pp ~init:PathMap.empty ~f:(fun acc (path,prob) ->
              PathMap.set acc ~key:path ~data:(prob /. total_prob)) in
      SrcDstMap.set acc ~key:(src,dst) ~data:pruned_paths) in
  (*assert (probabilities_sum_to_one new_scheme);*)
  (*assert (all_pairs_connectivity t (get_hosts t) new_scheme);*)
  new_scheme

let fit_scheme_to_bins (s:scheme) (nbins:int) : scheme =
  (* Round weights to multiples of 1/nbins while minimizing sum of differences
     between new weights and old weights *)
  SrcDstMap.fold s ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
      let path_int_rem_prob, sum_int_probs =
        PathMap.fold paths ~init:(PathMap.empty, 0)
          ~f:(fun ~key:path ~data:prob (path_acc, int_prob_acc) ->
            let scaled_prob = prob *. (Float.of_int nbins) in
            (* (prob * nbins) -> get ceiling and remainder *)
            let int_prob = Float.to_int scaled_prob in
            let rem_prob = scaled_prob -. (Float.of_int int_prob) in
            (PathMap.set path_acc ~key:path ~data:(int_prob, rem_prob),
             int_prob_acc + int_prob)) in
      let num_paths_to_roundup = nbins - sum_int_probs in
      let sorted_by_rem_prob =
        PathMap.to_alist path_int_rem_prob
        |> List.sort ~compare:(fun x y -> Float.compare (snd (snd y)) (snd (snd x))) in
      let pp_map,_ =
        List.fold sorted_by_rem_prob ~init:(PathMap.empty, num_paths_to_roundup)
          ~f:(fun (path_acc, to_roundup) (path, (int_prob, _)) ->
            let int_prob =
              if to_roundup > 0 then int_prob + 1
              else int_prob in
            let prob = Float.(of_int int_prob / of_int nbins) in
            (PathMap.set path_acc ~key:path ~data:prob,
             to_roundup - 1)) in
      SrcDstMap.set acc ~key:(src,dst) ~data:pp_map)


(**************************************************)
(* Helper functions to simulate failures *)
(**************************************************)

let bidir_failure topo e =
  let e' = reverse_edge_exn topo e in
  EdgeSet.add (EdgeSet.singleton e) e'

let update_topo_with_failure (t:topology) (f:failure) : topology =
  EdgeSet.fold f ~init:t
    ~f:(fun acc link -> Topology.remove_edge acc link)

(* Solve SPF *)
let solve_spf (topo:topology) (_:demands) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
    ~f:(fun x y ->
          (match (device x, device y) with | (Node.Host,Node.Host) -> true | _ -> false)
    ) in
  List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) ->
    SrcDstMap.set acc ~key:(v1,v2) ~data:( PathMap.singleton p 1.0) )


(* check all-pairs connectivity after failure *)
let check_connectivity_after_failure (topo:topology) (fail:failure) : bool =
  let topo' = update_topo_with_failure topo fail in
  let hosts = get_hosts topo' in
  solve_spf topo' SrcDstMap.empty
    |> all_pairs_connectivity topo' hosts


(* Compute all possible failure scenarios with num_failures link failing,
   while not partitioning the network *)
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



(*******************************************************************)
(* Common metric computation *)
(*******************************************************************)

(* Compute amount of traffic scheduled to be sent on each edge *)
let traffic_on_edge (t:topology) (d:demands) (s:scheme) : (float EdgeMap.t) =
  SrcDstMap.fold s ~init:EdgeMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
      PathMap.fold paths ~init:acc
        ~f:(fun ~key:path ~data:prob acc ->
          List.fold_left path ~init:acc
            ~f:(fun acc e ->
              let demand =
                match SrcDstMap.find d (src,dst) with
                | None -> 0.0
                | Some x -> x in
              match EdgeMap.find acc e with
              | None ->
                EdgeMap.set ~key:e ~data:(demand *. prob) acc
              | Some x ->
                EdgeMap.set ~key:e ~data:((demand *. prob) +. x) acc)))

(* Compute expected link utilization load on links.
   Expected utilization can be > 1 *)
let congestion_of_paths (t:topology) (d:demands) (s:scheme) : (float EdgeMap.t) =
  let sent_on_each_edge = traffic_on_edge t d s in
  EdgeMap.fold ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:amount_sent acc ->
      EdgeMap.set
        ~key:e
        ~data:(amount_sent /. (capacity_of_edge t e))
        acc) sent_on_each_edge

(*******************************************************************)
(* External LP solvers - Gurobi *)
(*******************************************************************)
(* start gurobi with an LP *)
let gurobi_process (lp_filename:string) (lp_solname:string) =
  let method_str = (Int.to_string !Globals.gurobi_method) in
  Unix.open_process_in ("gurobi_cl Method=" ^ method_str ^
                        " OptimalityTol=1e-9 ResultFile=" ^ lp_solname ^
                        " " ^ lp_filename)

(* call gurobi and wait for completion *)
let call_gurobi (lp_filename:string) (lp_solname:string) =
  let gurobi_in = gurobi_process lp_filename lp_solname in
  let time_str = "Solved in [0-9]+ iterations and \\([0-9.e+-]+\\) seconds" in
  let time_regex = Str.regexp time_str in
  let rec read_output gurobi solve_time =
    try
      let line = In_channel.input_line_exn gurobi in
      if Str.string_match time_regex line 0 then
        let num_seconds = Float.of_string (Str.matched_group 1 line) in
        read_output gurobi num_seconds
      else
        read_output gurobi solve_time
    with
      End_of_file -> solve_time in
  let _ = read_output gurobi_in 0. in
  let status = Unix.close_process_in gurobi_in in
  match status with
  | Error exit_or_signal ->
    begin
      failwith("Failed to run Gurobi. Please check that Gurobi is installed (gurobi_cl is in $PATH) and that you have a valid license.");
    end
  | _ -> ();
