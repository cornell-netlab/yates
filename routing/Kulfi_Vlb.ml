open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net

       
let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo ~f:(fun _ _ -> true) in
  
  let spf_table =
    List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) -> 
    SrcDstMap.add acc ~key:(v1,v2) ~data:p) in 

  let find_path src dst = SrcDstMap.find_exn spf_table (src,dst) in  

  let route_thru_detour src det dst =
    find_path src det @ find_path det dst in 

  let has_loop path = 
    let rec loop acc = function
      | [] -> 
	 false
      | e::rest -> 	 
	 let src,_ = Topology.edge_src e in 
	 (Topology.VertexSet.mem acc src)
	 || loop (Topology.VertexSet.add acc src) rest in 
    loop Topology.VertexSet.empty path in
      
  let vlb_pps src dst = 
    let paths,n = 
      Topology.fold_vertexes 
	(fun v (acc,n) ->
	 match device v with
	 | Node.Host -> 
	    (* Don't include hosts as detour nodes *)
	    (acc,n)
	 | _ ->
	    let path = route_thru_detour src v dst in 
	    if has_loop path then (acc,n)
	    else (path::acc, n + 1))
	topo 
	([],0) in
    List.fold_left
      paths
      ~init:PathMap.empty
      ~f:(fun acc path -> 
	  PathMap.add acc path (1.0 /. Float.of_int n)) in 
  
  (* NB: folding over apsp just to get all src-dst pairs *)
  let scheme = 
    List.fold_left 
      apsp 
      ~init:SrcDstMap.empty
      ~f:(fun acc (_,v1,v2,_) ->
	  SrcDstMap.add acc ~key:(v1,v2) ~data:( vlb_pps v1 v2 ) ) in 
  scheme
