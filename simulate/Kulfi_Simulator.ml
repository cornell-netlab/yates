open Frenetic_Network
open Net
       
open Kulfi_Routing
open Kulfi_Types
open Kulfi_MCF


module EdgeSet = Topology.EdgeSet
module VertexSet = Topology.VertexSet
                     
       
let () = Random.self_init ()

let _ = Kulfi_Demands.create_random

module Make(SOLVER:Kulfi_Routing.Algorithm) = struct    
  let start () = ()
end

module Solver = Make(Kulfi_Routing.Mcf)
                
let () =
  print_endline "Kulfi Simulator";
  if (Array.length Sys.argv <> 2) then
    (Printf.printf "usage: %s [dot-file]\n" Sys.argv.(0))
  else
    let graph_file = Sys.argv.(1) in
    let topo = Parse.from_dotfile graph_file in
    let host_set = VertexSet.filter (Topology.vertexes topo)
        ~f:(fun v ->
            let label = Topology.vertex_to_label topo v in
            Node.device label = Node.Host) in
    let hosts = Topology.VertexSet.elements host_set in
    let demand_matrix = Kulfi_Demands.create_sparse hosts 0.1 100 in
    let pairs = Kulfi_Demands.get_demands demand_matrix in
    Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
    Printf.printf "# pairs = %d\n" (List.length pairs);
    Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);
    (* let _ = Solver.solve topo pairs SrcDstSet.empty in *)

