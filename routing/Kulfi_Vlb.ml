open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net

let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
    ~f:(fun x y -> true) in  

  let spf_table =     
    List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) -> 
    SrcDstMap.add acc ~key:(v1,v2) ~data:( PathProbabilitySet.singleton (p,1.) ) ) in

  let find_path = (fun src dst ->
                   match SrcDstMap.find spf_table (src,dst) with
                   | None -> assert false
                   | Some x ->
                      fst ( match PathProbabilitySet.choose x with | None -> assert false | Some y -> y ) )
  in

  let route_thru_detour = (fun src det dst -> 
    (find_path src det) @ (find_path det dst) ) in

  let nv = Float.of_int (Topology.num_vertexes topo) in

  let vlb_pps = (fun src dst ->
    ( Topology.fold_vertexes 
      (fun v acc -> 
         ( PathProbabilitySet.add acc
           ( (route_thru_detour src v dst), 1. /. nv ) 
         )
      ) 
    )
    topo 
    PathProbabilitySet.empty ) in

  List.fold_left apsp 
    (* slightly confusing aspect: perform fold_left on apsp merely
       because it is a list containing one entry for each src-dest pair.
       the fact that it contains paths is irrelevant here *)
    ~init:SrcDstMap.empty
    ~f:(fun acc (c,v1,v2,p) ->
        SrcDstMap.add acc ~key:(v1,v2) ~data:( vlb_pps v1 v2 ) )
    

                
