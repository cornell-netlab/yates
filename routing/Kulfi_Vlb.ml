open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net

let dump_path_prob_set (t:topology) (pps:probability PathMap.t) : string =
  let buf = Buffer.create 101 in
  PathMap.iter 
    pps 
    ~f:(fun ~key:path ~data:prob -> Printf.bprintf buf "[%s] @ %f\n" (dump_edges t path) prob);
  Buffer.contents buf
       
let dump_scheme (t:topology) (s:scheme) : string = 
  let buf = Buffer.create 101 in
  SrcDstMap.iter s ~f:(fun ~key:(v1,v2) ~data:pps ->
                       Printf.bprintf buf "%s -> %s :\n  %s\n"
                                      (Node.name (Net.Topology.vertex_to_label t v1))
                                      (Node.name (Net.Topology.vertex_to_label t v2))
                                      (dump_path_prob_set t pps));
  Buffer.contents buf

let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo ~f:(fun _ _ -> true) in
  
  let spf_table =
    List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) -> 
    SrcDstMap.add acc ~key:(v1,v2) ~data:p) in 

  let find_path src dst = SrcDstMap.find_exn spf_table (src,dst) in  

  let route_thru_detour src det dst =
    let p = (find_path src det @ find_path det dst) in
    (* assert (not (List.is_empty p));       *)
    let p' = Kulfi_Frt.FRT.remove_cycles p in
    (* assert (not (List.is_empty p'));       *)
    p' in

  (* let has_loop path =  *)
  (*   let rec loop acc = function *)
  (*     | [] ->  *)
  (* 	 false *)
  (*     | e::rest -> 	  *)
  (* 	 let src,_ = Topology.edge_src e in  *)
  (* 	 (Topology.VertexSet.mem acc src) *)
  (* 	 || loop (Topology.VertexSet.add acc src) rest in  *)
  (*   loop Topology.VertexSet.empty path in *)

  let nv = Float.of_int (Topology.fold_vertexes (fun _ -> succ) topo 0) in 
      
  let vlb_pps src dst = 
    let paths = 
      Topology.fold_vertexes 
	(fun v acc ->
	 match device v with
	 | Node.Host -> 
	    (* Don't include hosts as detour nodes *)
	    acc
	 | _ ->
	    (route_thru_detour src v dst)::acc)
	topo 
	[] in
    List.fold_left
      paths
      ~init:PathMap.empty
      ~f:(fun acc path -> 
	  add_or_increment_path acc path (1.0 /. nv)) in 
  
  (* NB: folding over apsp just to get all src-dst pairs *)
  let scheme = 
    List.fold_left 
      apsp 
      ~init:SrcDstMap.empty
      ~f:(fun acc (_,v1,v2,_) ->
	  SrcDstMap.add acc ~key:(v1,v2) ~data:( vlb_pps v1 v2 ) ) in
  (* Printf.printf "%s\n" (dump_scheme topo scheme); *)
  scheme
