open Net.Topology
open Core.Std
open Kulfi_Apsp

let rec getAllSp (v1:Topology.vertex) (v2:Topology.vertex) (topo:topology) (apsp: (bool * int * (Topology.vertex * float) List.t)
SrcDstMap.t) (max_tot: int) : (Topology.edge List.t) List.t=
  if (v1=v2) then
    [[]]
  else
    let _,_,nhop_list= SrcDstMap.find_exn apsp (v1, v2) in
    let p1=List.fold_left nhop_list
        ~init: []
        ~f:
        (fun acc (nhop,_)  ->
          let remains=max_tot-(List.length acc) in
          if remains<0 then
            acc
          else
            let nextPaths=getAllSp nhop v2 topo apsp remains in
            let myans=List.fold_left nextPaths
              ~init: []
              ~f:(fun acc2 path ->
                let thisedge=Topology.find_edge topo v1 nhop in
                let newp=List.append [thisedge] path in
                  List.append acc2 [newp]
              ) in
            let newacc=List.append acc myans in
              newacc
        ) in
    p1

let solve (topo:topology) (_:demands) (_:scheme) : scheme =
  (*
   * Yang: my implementation is the following:
     * If there are less than max_tot=1000 different shortest paths between s and t,
     * we will pick all of them, and assign 1.0/tot prob
     * If there are more than max_tot=1000 different shortest paths between s and t,
     * we simply pick random 1000 of the shortest paths
   *)
  let mpapsp=all_pairs_multi_shortest_path topo in
  let max_tot=1000 in
  let sp_table=
    SrcDstMap.fold
        mpapsp
        ~init: SrcDstMap.empty
        ~f:(fun ~key:(v1,v2) ~data:(_,n,probs) acc ->
              let paths=getAllSp v1 v2 topo mpapsp max_tot in
              let prob=1.0 /. Float.of_int (List.length paths) in
              let path_dist= List.fold_left paths
                ~init: PathMap.empty
                ~f:( fun acc path ->
                  PathMap.add acc ~key:path ~data:prob
                  ) in
                SrcDstMap.add acc ~key:(v1,v2) ~data: path_dist
        ) in
  sp_table


