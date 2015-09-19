open Kulfi_Routing
open Frenetic_Network
open Net
open Kulfi_Ecmp
open Kulfi_Mcf
open Kulfi_Mw
open Kulfi_Raeke
open Kulfi_Spf
open Kulfi_Vlb
open Kulfi_Types
open Kulfi_Util
open Core.Std

module VertexSet = Topology.VertexSet

let create_topology_and_demands () =
  let topo = Parse.from_dotfile "./data/topologies/3cycle.dot" in
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
  let demands = List.fold_left ~init:SrcDstMap.empty
                               ~f:(fun acc (u,v,r) -> SrcDstMap.add acc ~key:(u,v) ~data:r) pairs in
  (* Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set); *)
  (* Printf.printf "# demands = %d\n" (SrcDstMap.length demands); *)
  (* Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo); *)
  (hosts,topo,demands)


let test_mcf () = 
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = 
    Kulfi_Mcf.solve topo pairs SrcDstMap.empty in
  let h1 = Array.get hosts 0  in 
  let h2 = Array.get hosts 1  in

    match SrcDstMap.find scheme (h1,h2) with
    | None -> false
    | Some paths ->
       let sum_of_probs =               
	 PathMap.fold paths ~init:0.0 ~f:(fun ~key:p ~data:s acc -> s +. acc) in
       (sum_of_probs > 0.9) && (sum_of_probs < 1.1)
                 		   
let test_spf () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = 
    Kulfi_Spf.solve topo pairs SrcDstMap.empty in
  let h1 = Array.get hosts 0  in 
  let h2 = Array.get hosts 1  in

  (* TODO(jnf,rjs): could just call sample_scheme here? *)
  let x = match SrcDstMap.find scheme (h1,h2)  with | None -> assert false | Some x -> x in
  let path = sample_dist x in
  (List.length path) = 3
    
let test_vlb () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = 
    Kulfi_Vlb.solve topo pairs SrcDstMap.empty in
  let h1 = Array.get hosts 0  in 
  let h2 = Array.get hosts 1  in
  let paths = match SrcDstMap.find scheme (h1,h2) with | None -> assert false | Some x -> x in
  (* Printf.printf "VLB set length =%d\n"  (PathMap.length paths); *)
  (* Printf.printf "%s\n" (dump_scheme topo scheme); *)
  (PathMap.length paths) = 2

let test_apsp () = 
    let (hosts,topo,pairs) = create_topology_and_demands () in
    let paths = Frenetic_Network.NetPath.all_pairs_shortest_paths ~topo:topo ~f:(fun _ _ -> true) in 
    Array.fold
      hosts
      ~init:true
      ~f:(fun acc u -> 	
	  Array.fold
	    hosts
	    ~init:acc
	    ~f:(fun acc v ->
		if u = v then acc
		else acc && List.exists paths (fun (_,v1,v2,_) -> v1 = u && v2 = v)))

let test_vlb2 () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = Kulfi_Vlb.solve topo pairs SrcDstMap.empty in
  Array.fold
    hosts
    ~init:true
    ~f:(fun acc u -> 	
	Array.fold
	  hosts
	  ~init:acc
	  ~f:(fun acc v ->			   
	      if u = v then true && acc
	      else
		match SrcDstMap.find scheme (u,v) with
		| None -> 
		   false
		| Some paths -> not (PathMap.is_empty paths)  && acc)) 
    
let test_mw () = false

let test_raeke () = false

let test_ecmp () = false

let test_ak () = false		  
			    
TEST "mcf" = test_mcf () = true

TEST "spf" = test_spf () = true

TEST "apsp" = test_apsp () = true

TEST "vlb" = test_vlb () = true

TEST "vlb2" = test_vlb2 () = true
 			   
TEST "mw" = test_mw () = true

TEST "raeke" = test_raeke () = true
			    
TEST "ak" = test_ak () = true

TEST "ecmp" = test_ecmp () = true

