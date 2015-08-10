open Frenetic_Network
open Net
open Kulfi_Types

val solve : topology -> demands -> scheme -> scheme
       
module type MW_INPUT = sig

  type structure

  (* Gets the next structure (e.g. path) that we are going to augment the
   * flow by, along with the weight of that structure. *)
  val select_structure : topology -> Topology.VertexSet.t -> (structure * float)

  (* Gets the usages of every edge in the topology. *)
  val usage_of_structure : topology -> structure -> (edge * float) list

  (* Gets the weight of one edge in the topology. *)
  val get_weight : topology -> edge -> float

  (* Sets the weight of one edge in the topology. *)
  val set_weight : topology -> edge -> float -> topology

end

module type MW_ALG = sig

  type structure

  val hedge : float -> float -> topology -> Topology.VertexSet.t
    -> (edge, float) Hashtbl.t
    -> (structure * float * topology * (edge, float) Hashtbl.t)

  val hedge_iterations : float -> topology -> Topology.VertexSet.t ->
    (int * (structure * float) list * topology)

end

module Make (Experts : MW_INPUT) : MW_ALG
  with type structure = Experts.structure

module RRTs : MW_ALG with type structure = Kulfi_FRT.FRT.routing_tree
