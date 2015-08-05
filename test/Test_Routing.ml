open Kulfi_Routing
open Frenetic_Network
open Net
open Kulfi_Spf
open Kulfi_Types

module VertexSet = Topology.VertexSet

       
let test_spf =
  let topo = Parse.from_dotfile "./data/3cycle.dot" in
  let host_set = VertexSet.filter (Topology.vertexes topo)
                                  ~f:(fun v ->
                                      let label = Topology.vertex_to_label topo v in
                                      Node.device label = Node.Host) in
  let hs = Topology.VertexSet.elements host_set in
  let hosts = Array.of_list hs in
  let num_hosts = List.length hs in
  let demands = Array.make_matrix num_hosts num_hosts 1.0 in
  let pairs =
    let lst = ref [] in
    Array.iteri (fun i h_i ->
                 Array.iteri (fun j h_j ->
                              let demand = demands.(i).(j) in
                              if i = j || demand = 0.0 then () else
                                lst := (hosts.(i), hosts.(j), demand)::(!lst))
                             hosts)
                hosts;
    !lst
  in 
  Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
  Printf.printf "# pairs = %d\n" (List.length pairs);
  Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);
  let scheme = 
    solve topo pairs SrcDstMap.empty in
  

  false
    
TEST "spf" = test_spf = true


