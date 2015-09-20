open Frenetic_Network
open Net
open Core.Std

module EdgeSet = Topology.EdgeSet
module VertexSet = Topology.VertexSet
       
type topology = Net.Topology.t

type edge = Net.Topology.edge with sexp
                                     
type path = edge list with sexp

type solver_type = | Mcf | Vlb | Ecmp | Spf | Ak | Smcf | Raeke

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
                  
type demand = float with sexp

type probability = float with sexp

type congestion = float with sexp

module PathOrd = struct
  type t = path with sexp
  let compare = Pervasives.compare                    
end

module PathMap = Map.Make(PathOrd)

module IntMap = Map.Make(Int)

type index_map = Topology.vertex IntMap.t
			
module PathSet = Set.Make(PathOrd)

type tag = int

module Tag = Int

module TagsMap =
  Map.Make
    (struct
      type t = Tag.t list with sexp
      let compare = Pervasives.compare (* List.compare ~cmp:(Pervasives.compare) *)
    end)

module VertexOrd = struct
  type t = Topology.vertex with sexp
  let compare = Pervasives.compare                                      
end
		  
module VertexMap = Map.Make(VertexOrd)
		  
(* TODO(rjs): Give a better name. VertexPair map? *)                        
module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex with sexp
  let compare = Pervasives.compare                                      
end
                     
module SrcDstMap = Map.Make(SrcDstOrd)

module StringMap = Map.Make(String)

type node_map = Node.t StringMap.t			   
			   
module EdgeOrd = struct
  type t = edge with sexp
  let compare = Pervasives.compare                                      
end

module EdgeMap = Map.Make(EdgeOrd)                     

type uid = int with sexp			

module UidOrd = struct
  type t = uid with sexp
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
  let paths = PathMap.keys path_dist in
  let bound = List.length paths in
  let i = Random.int bound in
  match List.nth paths i with
  | None -> assert false
  | Some p -> p

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
                       
let capacity_of_edge topo edge =
  let label = Topology.edge_to_label topo edge in
  (Int64.to_float (Link.capacity label))

let configuration_of_scheme (topo:topology) (scm:scheme) (tag_hash: (edge,int) Hashtbl.t) : configuration =
  SrcDstMap.fold
    scm
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
        if src = dst then acc
        else
          let tags =
            PathMap.fold
              paths
              ~init:TagsMap.empty
              ~f:(fun ~key:path ~data:prob acc ->
                  match path with
                  | [] ->
                     assert false
                  | _::path' -> 
                     let tags =
                       List.map
                         path'
                         ~f:(fun edge ->
                             match Hashtbl.find tag_hash edge with
                             | None ->
                                Printf.printf "Couldn't find %s\n" (dump_edges topo [edge]);
                                99
                             | Some t ->
                                t) in 
                     TagsMap.add acc ~key:tags ~data:prob) in
        SrcDstMap.add acc ~key:(src,dst) ~data:tags)

let bprint_tags (buf:Buffer.t) (tag_dist:probability TagsMap.t) : unit =
  TagsMap.iter
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
  SrcDstMap.iter
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
  Hashtbl.Poly.iter
    bufs
    ~f:(fun ~key:src ~data:buf ->
	let route_filename = Printf.sprintf "routes/%s_%d" (Frenetic_Packet.string_of_ip (Node.ip (Topology.vertex_to_label topo src))) time in
	let route_file = Out_channel.create route_filename in
	Out_channel.output_string route_file (Buffer.contents buf);
	Out_channel.close route_file;
	)


