open Kulfi_Types
open Frenetic_Network
open Net
open Net.Topology
open Core.Std
open Kulfi_Apsp

let prev_scheme = ref SrcDstMap.empty

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

let solve (topo:topology) (_:demands) : scheme =
  let new_scheme =
  if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
  else
  (* (pk): run ksp with budget;
   * find shortest path;
   * select paths with weight <= 1.2 times shortest *)
  let host_set = get_hosts_set topo in
  let all_ksp = all_pair_k_shortest_path topo (min !Kulfi_Globals.budget 100) host_set in
  let thresh = 1.2 in
  SrcDstMap.fold all_ksp
    ~init: SrcDstMap.empty
    ~f:(fun ~key:(u,v) ~data:paths acc ->
      if (u = v) then acc
      else
        if List.is_empty paths then acc
        else
          let sorted_paths = List.sort ~cmp:(fun x y -> Pervasives.compare (get_path_weight topo x) (get_path_weight topo y)) paths in
          let shortest_path_weight = get_path_weight topo (match List.hd sorted_paths with
              | None -> assert false
              | Some x -> x) in
          (*Printf.printf "Shortest path weight: %f\n" shortest_path_weight;*)
          let selected_paths = List.filter sorted_paths
            ~f:(fun p -> (get_path_weight topo p) <= (thresh *. shortest_path_weight)) in
          (*List.iter selected_paths ~f:(fun p ->
            Printf.printf "%f : %s\n%!" (get_path_weight topo p) (dump_edges topo p);); *)
          let prob = 1. /. Float.of_int (List.length selected_paths) in
          let path_dist = List.fold_left selected_paths ~init: PathMap.empty
                    ~f:(fun acc p -> PathMap.add ~key:p ~data:prob acc) in
          SrcDstMap.add ~key:(u,v) ~data:path_dist acc) in
  prev_scheme := new_scheme;
  new_scheme


let solve_mpapsp (topo:topology) (_:demands) : scheme =
  let new_scheme =
  if not (SrcDstMap.is_empty !prev_scheme) then !prev_scheme
  else
  (*
   * Yang: my implementation is the following:
     * If there are less than max_tot=1000 different shortest paths between s and t,
     * we will pick all of them, and assign 1.0/tot prob
     * If there are more than max_tot=1000 different shortest paths between s and t,
     * we simply pick random 1000 of the shortest paths
   *)
  let mpapsp=all_pairs_multi_shortest_path topo in
  let host_set =
    VertexSet.filter
      (vertexes topo)
      ~f:(fun v ->
          let lbl = vertex_to_label topo v in
          Node.device lbl = Node.Host) in

  let max_tot=20 in
  SrcDstMap.fold
        mpapsp
        ~init: SrcDstMap.empty
        ~f:(fun ~key:(v1,v2) ~data:(_,n,probs) acc ->
              if (VertexSet.mem host_set v1) && (VertexSet.mem host_set v2) then
                let paths=getAllSp v1 v2 topo mpapsp max_tot in
                let prob=1.0 /. Float.of_int (List.length paths) in
                let path_dist= List.fold_left paths
                  ~init: PathMap.empty
                  ~f:( fun acc path ->
                    PathMap.add acc ~key:path ~data:prob;) in
                  SrcDstMap.add acc ~key:(v1,v2) ~data: path_dist
              else
                acc
        ) in
  (*Printf.printf "Done calculating paths %!";*)
  (*Printf.printf "%s" (dump_scheme topo sp_table);*)
  prev_scheme := new_scheme;
  new_scheme

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = Kulfi_Types.normalization_recovery
