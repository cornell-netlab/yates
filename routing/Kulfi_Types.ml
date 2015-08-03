open Frenetic_Network
open Kulfi_Util
open Net
       
type topology = Net.Topology.t

type demand = float

type demand_pair = Topology.vertex * Topology.vertex * demand

type demands = demand_pair list

type edge = Topology.vertex * Topology.vertex 

type path = edge list

type probability = float

module PathProbabilityOrd = struct
  type t = path * probability
  let compare = Pervasives.compare
end

module PathProbabilitySet = Setplus.Make(PathProbabilityOrd)
                             
module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex
  let compare = Pervasives.compare
end

module SrcDstMap = Mapplus.Make(PathProbabilityOrd)

type scheme = PathProbabilitySet.t SrcDstMap.t

                                                                   
(* A Routing Scheme is an object that describes a prob distribution over paths. 
   It supports an interface to lets one draw a random sample, and a way to compare
   to other routing schemes, for example, if we want to minimize differences  *)

let sample (s:scheme) (src:Topology.vertex) (dst:Topology.vertex) : path = assert false

let compare_scheme (s1:scheme) (s2:scheme) : int = assert false
