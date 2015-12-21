open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net
open Net.Topology
open Kulfi_Globals
open Kulfi_Apsp

let () = Random.self_init ()

let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  let mpapsp = all_pairs_multi_shortest_path topo in
  (* let _ = print_mpapsp mpapsp topo in *)
  let spf_table =
    SrcDstMap.fold
      mpapsp
      ~init:SrcDstMap.empty
      ~f:(fun ~key:(v1,v2) ~data:_ acc ->
        let rand_path =  get_random_path v1 v2 topo mpapsp in
        SrcDstMap.add acc ~key:(v1,v2) ~data:rand_path) in

  let find_path src dst = SrcDstMap.find_exn spf_table (src,dst) in

  let route_thru_detour src det dst =
    let p = (find_path src det @ find_path det dst) in
    (* assert (not (List.is_empty p));       *)
    let p' = if !Kulfi_Globals.deloop then Kulfi_Frt.FRT.remove_cycles p
             else p in
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

  let vlb_pps src dst =
    let (paths,num_switches) =
      Topology.fold_vertexes
	(fun v (p_acc,ns_acc) ->
	 match device v with
	 | Node.Switch ->
	    (* Only route through switches *)
	    ((route_thru_detour src v dst)::p_acc, ns_acc +. 1.)
	 | _ ->
            (p_acc, ns_acc) )
	topo
	([], 0.) in
    List.fold_left
      paths
      ~init:PathMap.empty
      ~f:(fun acc path ->
	  add_or_increment_path acc path (1.0 /. num_switches)) in

  (* NB: folding over mpapsp just to get all src-dst pairs *)
  let scheme =
    SrcDstMap.fold
      mpapsp
      ~init:SrcDstMap.empty
      ~f:(fun ~key:(v1,v2) ~data:_ acc ->
	    match (device v1, device v2) with
	    | (Node.Host,Node.Host) ->
	       SrcDstMap.add acc ~key:(v1,v2) ~data:( vlb_pps v1 v2 )
	    | _ -> acc
      ) in
  (* Printf.printf "%s\n" (dump_scheme topo scheme); *)
  scheme
