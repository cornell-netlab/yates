open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net

let solve (topo:topology) (d:demands) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
    ~f:(fun x y ->
          (match (device x, device y) with | (Node.Host,Node.Host) -> true | _ -> false)
    ) in
  List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) ->
    SrcDstMap.add acc ~key:(v1,v2) ~data:( PathMap.singleton p 1.0) )

let initialize _ = ()
