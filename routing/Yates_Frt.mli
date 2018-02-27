open Yates_Types

module type FRT_TYPE = sig

  type frt_tree
  type routing_tree

  type routing_edge = Topology.vertex * Topology.vertex
  type routing_path = routing_edge list

  val make_frt_tree : Topology.t -> frt_tree

  val generate_rt : Topology.t -> frt_tree ->
    Topology.vertex list -> routing_tree

  val usage_of_tree : routing_tree -> (Topology.edge * float) list

  val get_path_halves : routing_tree -> Topology.vertex -> Topology.vertex ->
    routing_path * routing_path

  val get_path : routing_tree -> Topology.vertex ->
    Topology.vertex -> routing_path

  val remove_cycles : NetPath.t -> NetPath.t

  val routing_edges : routing_tree -> routing_edge list

  val edge_to_physical : routing_tree -> routing_edge -> NetPath.t

  val path_to_physical : routing_tree -> routing_edge list -> NetPath.t

  val get_levels : frt_tree -> Topology.VertexSet.t list list

  val write_frt : Topology.t -> frt_tree -> string -> unit
  val write_rt : Topology.t -> routing_tree -> string -> unit

end

module FRT : FRT_TYPE
