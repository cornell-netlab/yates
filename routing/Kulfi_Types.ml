open Frenetic_Network
open Net
open Core.Std

module EdgeSet = Topology.EdgeSet
module VertexSet = Topology.VertexSet

type topology = Net.Topology.t

type edge = Net.Topology.edge [@@ deriving sexp]

type path = edge list [@@ deriving sexp]

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

type demand = float [@@ deriving sexp]

type probability = float [@@ deriving sexp]

type congestion = float [@@ deriving sexp]

type latency = float [@@ deriving sexp]

type throughput = float [@@ deriving sexp]

type failure = EdgeSet.t [@@ deriving sexp]

module PathOrd = struct
  type t = path [@@ deriving sexp]
  let compare = Pervasives.compare
end

module PathMap = Map.Make(PathOrd)

module IntMap = Map.Make(Int)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type edgestr_util_map = congestion StringMap.t

type index_map = Topology.vertex IntMap.t

module Latency = Float
module LatencyMap = Map.Make(Latency)

module PathSet = Set.Make(PathOrd)

type tag = int

module Tag = Int

module TagsMap =
  Map.Make
    (struct
      type t = Tag.t list [@@ deriving sexp]
      let compare = Pervasives.compare (* List.compare ~cmp:(Pervasives.compare) *)
    end)

module VertexOrd = struct
  type t = Topology.vertex [@@ deriving sexp]
  let compare = Pervasives.compare
end

module VertexMap = Map.Make(VertexOrd)

(* TODO(rjs): Give a better name. VertexPair map? *)
module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex [@@ deriving sexp]
  let compare = Pervasives.compare
end

module SrcDstMap = Map.Make(SrcDstOrd)


type node_map = Node.t StringMap.t

module EdgeOrd = struct
  type t = edge [@@ deriving sexp]
  let compare = Pervasives.compare
end

module EdgeMap = Map.Make(EdgeOrd)

type uid = int [@@ deriving sexp]

module UidOrd = struct
  type t = uid [@@ deriving sexp]
  let compare = Pervasives.compare
end

module UidMap = Map.Make(UidOrd)

type path_uid_map = uid PathMap.t

type uid_path_map = (Topology.vertex * Topology.vertex  * path) UidMap.t

type edge_uidlist_map = uid list EdgeMap.t

(* A flow assigns a numerical value to each edge, denoting the number
   of flow units that traverse the edge. *)
type flow = float EdgeMap.t

(* A mc_flow, short for multi-commodity flow, is given by a collection
   of source-destination pairs and a flow for each of them. *)
type mc_flow = flow SrcDstMap.t

(* A flow_decomp is a flow decomposed into paths. *)
type flow_decomp = probability PathMap.t

(* Keeps track of paths to their congestion *)
type overhead = congestion PathMap.t

type demands = demand SrcDstMap.t

(* A routing scheme specifies a flow_decomp for each source-destination pair. *)
type scheme = flow_decomp SrcDstMap.t

type configuration = (probability TagsMap.t) SrcDstMap.t

(* A Routing Scheme is an object that describes a prob distribution over paths.
   It supports an interface to lets one draw a random sample, and a way to compare
   to other routing schemes, for example, if we want to minimize differences  *)

let sample_dist (path_dist:flow_decomp) : path =
  (* TODO: make a correct sampling procedure here.
           A correct procedure would do the following.
           1. Build up a list of partial sums of the probabilities in
              path_dist.
           2. Simultaneously with 1, build up a map from each partial sum
              to the path whose probability yielded that partial sum.
           3. Sample a uniformly random float between 0 and 1.
           4. Find which is the smallest partial sum that exceeds the
              sampled value.
           5. Use the map to find the corresponding path.
  *)
  assert false
  (* Old but incorrect sampling routing is here:
     let paths = PathMap.keys path_dist in
     let bound = List.length paths in
     let i = Random.int bound in
     match List.nth paths i with
     | None -> assert false
     | Some p -> p
  *)

let compare_scheme (s1:scheme) (s2:scheme) : int = assert false

(* The following function is used in Kulfi_Vlb.ml and Kulfi_SemiMcf.ml,
   and may possibly be useful elsewhere. Not sure that Kulfi_Types is
   where it belongs, but it's a convenient place. *)

let add_or_increment_path (fd : flow_decomp) (p : path) (r : probability) : flow_decomp =
  let new_value = match PathMap.find fd p with
  | None -> r
  | Some prior_value -> prior_value +. r
  in
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

(* Modify routing scheme by normalizing path probabilites while avoiding failed links *)
let normalization_recovery (curr_scheme:scheme) (_:topology) (failed_links:failure) (_:demands) : scheme =
  Printf.printf "\t\t\t\t\t\t\t\t\t\tLocal\r";
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
  Printf.printf "\t\t\t\t\t\t\t\t\t\tLOCAL\r";
  new_scheme


let get_hosts_set (topo:topology) : VertexSet.t =
  VertexSet.filter (Topology.vertexes topo)
  ~f:(fun v ->
    let label = Topology.vertex_to_label topo v in
    Node.device label = Node.Host)

let get_hosts (topo:topology) =
  let host_set = get_hosts_set topo in
  Topology.VertexSet.elements host_set

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

