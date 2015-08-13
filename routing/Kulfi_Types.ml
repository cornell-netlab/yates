open Frenetic_Network
open Net
open Core.Std
       
type topology = Net.Topology.t

type demand = float

type demand_pair = Topology.vertex * Topology.vertex * demand

type demands = demand_pair list

type edge = Net.Topology.edge with sexp

type path = edge list with sexp

type probability = float with sexp

module PathProbabilityOrd = struct
  type t = path * probability with sexp
  let compare = Pervasives.compare                                      
end

module PathProbabilitySet = Set.Make(PathProbabilityOrd)
                             
module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex with sexp
  let compare = Pervasives.compare                                      
end

module SrcDstMap = Map.Make(SrcDstOrd)

type scheme = PathProbabilitySet.t SrcDstMap.t

                                                                   
(* A Routing Scheme is an object that describes a prob distribution over paths. 
   It supports an interface to lets one draw a random sample, and a way to compare
   to other routing schemes, for example, if we want to minimize differences  *)

let sample (s:scheme) (src:Topology.vertex) (dst:Topology.vertex) : path = assert false

let compare_scheme (s1:scheme) (s2:scheme) : int = assert false
