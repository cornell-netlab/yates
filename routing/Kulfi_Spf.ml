open Core.Std
open Kulfi_Types
open Frenetic_Network

let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
    ~f:(fun x y -> true) in
  List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) -> 
    SrcDstMap.add (v1,v2) ( PathProbabilitySet.singleton (p,1.0) ) 
    acc)

(*  print_endline "Kulfi_Spf";
  SrcDstMap.empty *)

                
