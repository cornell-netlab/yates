open Frenetic_Network
open Net
open Core.Std

module EdgeSet = Topology.EdgeSet
module VertexSet = Topology.VertexSet
       
type topology = Net.Topology.t

type edge = Net.Topology.edge with sexp

type path = edge list with sexp
                  
type demand = float with sexp

type probability = float with sexp

type congestion = float with sexp

module PathOrd = struct
  type t = path with sexp
  let compare = Pervasives.compare                    
end

module PathMap = Map.Make(PathOrd)

module PathSet = Set.Make(PathOrd)

type tag = int

module Tag = Int

module TagMap = Map.Make(Tag)

(* TODO(rjs): Give a better name. VertexPair map? *)                        
module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex with sexp
  let compare = Pervasives.compare                                      
end
                     
module SrcDstMap = Map.Make(SrcDstOrd)
                     
module EdgeOrd = struct
  type t = edge with sexp
  let compare = Pervasives.compare                                      
end

module EdgeMap = Map.Make(EdgeOrd)                     

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

type configuration = (probability TagMap.t) SrcDstMap.t
             
(* A Routing Scheme is an object that describes a prob distribution over paths. 
   It supports an interface to lets one draw a random sample, and a way to compare
   to other routing schemes, for example, if we want to minimize differences  *)

let sample_dist (path_dist:flow_decomp) : path = assert false

let compare_scheme (s1:scheme) (s2:scheme) : int = assert false

(* The following stuff was moved from Kulfi_Mcf.ml to here 
   so that it could be used in Kulfi_Ak.ml. It doesn't really
   belong in Kulfi_Types.ml, we should move it somewhere else
   in a future re-factoring of the code. *)

let cap_divisor = 100000.
let demand_divisor = 1000.
                       
let capacity_of_edge topo edge =
  let label = Topology.edge_to_label topo edge in
  (Int64.to_float (Link.capacity label)) /. cap_divisor

