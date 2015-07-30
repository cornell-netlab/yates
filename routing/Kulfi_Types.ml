open Frenetic_Network
open Kulfi_Util
open Net
       
type topology = Net.Topology.t

type demand_pair = Topology.vertex * Topology.vertex * float

type demands = demand_pair list

type edge = Topology.vertex * Topology.vertex 

type path = edge list

module PathProbabilityOrd = struct
  type t = path * float
  let compare = Pervasives.compare
end

module PathProbabilitySet = Setplus.Make(PathProbabilityOrd)
                             
module SrcDstOrd = struct
  type t = Topology.vertex * Topology.vertex
  let compare = Pervasives.compare
end

module SrcDstSet = Mapplus.Make(PathProbabilityOrd)

type scheme = PathProbabilitySet.t SrcDstSet.t                 
