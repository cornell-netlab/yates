open Frenetic.Net
open Core

module Topology = Frenetic_kernel.Network.Net.Topology
module Node = Frenetic_kernel.Network.Node
module Link = Frenetic_kernel.Network.Link
module NetPath = Frenetic_kernel.Network.NetPath
module Net = Frenetic_kernel.Network.Net

module EdgeSet = Topology.EdgeSet
module VertexSet = Topology.VertexSet

type topology = Topology.t

type edge = Topology.edge [@@ deriving sexp]

type vertex = Topology.vertex [@@ deriving sexp]

type path = edge list [@@ deriving sexp]

type demand = float [@@ deriving sexp]

type probability = float [@@ deriving sexp]

type congestion = float [@@ deriving sexp]

type latency = float [@@ deriving sexp]

type throughput = float [@@ deriving sexp]

type failure = EdgeSet.t [@@ deriving sexp]

type flow_table = ((Node.t * Node.t),
                   (Node.t * Node.t * float) list) Hashtbl.t


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
