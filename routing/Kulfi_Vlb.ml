open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net
open Net.Topology
open Kulfi_Globals
open Kulfi_Apsp

let () = Random.self_init ()

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

(* Multiple APSP ---------------------------------------------- *)

let abs_fl (n:float) =
  if n > 0.0 then n else -.n

let rec dp_calc_numpaths (i:Topology.vertex) (j:Topology.vertex) (t:topology)
(dist: float SrcDstMap.t)
(numpath: (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t) =
  let  (v, n, l) = SrcDstMap.find_exn numpath (i,j) in
  let n_numpath = SrcDstMap.add numpath ~key:(i,j) ~data:(true, n, l) in
  let neigh = Topology.neighbors t i in
  let n3_numpath = Topology.fold_vertexes
    (fun nextHop acc ->
      if (nextHop=i) then acc
      else if not (VertexSet.mem neigh nextHop) then acc
      else
        let d_ih = SrcDstMap.find_exn dist (i,nextHop) in
        let d_hj = SrcDstMap.find_exn dist (nextHop,j) in
        let d_ij = SrcDstMap.find_exn dist (i,j) in
        if (abs_fl(d_ih +. d_hj -. d_ij) < 0.00001) then
          let t_visited,_,_ = SrcDstMap.find_exn acc  (nextHop,j) in
          let n2_numpath = if t_visited then acc
            else dp_calc_numpaths nextHop j t dist acc  in
          let _,np_nj,_ = SrcDstMap.find_exn n2_numpath (nextHop,j) in
          let _,np_ij,l_ij = SrcDstMap.find_exn n2_numpath (i,j) in
          let t_np_ij = np_ij + np_nj in
          SrcDstMap.add n2_numpath ~key:(i,j) ~data:(true, t_np_ij,
          List.append l_ij [(nextHop, Float.of_int np_nj)])
        else acc
    ) t n_numpath in
  let _,normalizer,l_ij = SrcDstMap.find_exn n3_numpath (i,j) in
  let path_probs,_ = List.fold_left l_ij ~init:([], 0.0) ~f:(fun acc (nextHop, np_h) ->
    let l, preprob = acc in
    let n_prob = np_h /. Float.of_int normalizer in
    (List.append l [(nextHop, preprob +. n_prob)], preprob +. n_prob)
  ) in
  SrcDstMap.add n3_numpath ~key:(i,j) ~data:(true,normalizer, path_probs)


let multiple_apsp (topo:topology) =
  (* in: topology: out: map (s,t) -> list(path, prob)*)
  let dist_mat = Topology.fold_vertexes
    (fun i dist_mat -> Topology.fold_vertexes
        (fun j dist_mat2 ->
          let ans = if (i=j) then 0.0
          else 10000000.0 in
          SrcDstMap.add dist_mat2 ~key:(i,j) ~data:ans)
        topo
        dist_mat
        )
    topo
    SrcDstMap.empty in
  let dist_mat_init = Topology.fold_edges
  (fun e acc ->
    let src,_ = Topology.edge_src e in
    let dst,_ = Topology.edge_dst e in
    let weight = 1.0 in (*TODO: read correct weight*)
    let _ = SrcDstMap.add acc ~key:(src, dst) ~data:weight in
    SrcDstMap.add acc ~key:(dst, src) ~data:weight)
  topo
  dist_mat in
  let dist_mat_sp = Topology.fold_vertexes
  (fun k acc ->
    let dist_mat_k = Topology.fold_vertexes
      (fun i acc_i ->
        let dist_mat_i = Topology.fold_vertexes
        (fun j acc_j ->
          let dij  = SrcDstMap.find_exn acc_j (i,j)  in
          let dik  = SrcDstMap.find_exn acc_j (i,k)  in
          let dkj  = SrcDstMap.find_exn acc_j (k,j)  in
          let upd_val = if (dik +. dkj < dij) then dik +. dkj else dij in
          SrcDstMap.add acc_j ~key:(i,j) ~data:upd_val
          ) topo acc_i in
        dist_mat_i
        ) topo  acc in
    dist_mat_k
      ) topo dist_mat_init
    in
    (* Floyd-Warshall complete *)
  let _ = SrcDstMap.iter
  dist_mat_sp
  ~f:(fun ~key:(src,dst) ~data:wt ->
    let sstr = Node.name (Topology.vertex_to_label topo src) in
    let dstr = Node.name (Topology.vertex_to_label topo dst) in
    Printf.printf "%s\t%s\t%f\n" sstr dstr wt
    ) in
  (* initialize visited to be true for i,i *)
  let init_vis_npath_pathlist_map = Topology.fold_vertexes
  (fun i acc_i ->
    Topology.fold_vertexes
      (fun j acc_j ->
        let visited = if (i=j) then true else false in
        let num_paths = if (i=j) then 1 else 0 in
        SrcDstMap.add acc_j ~key:(i,j) ~data:(visited,num_paths,[]))
      topo acc_i) topo SrcDstMap.empty in
  let numPath_map = Topology.fold_vertexes
  (fun i acc_i ->
    Topology.fold_vertexes
      (fun j acc_j ->
        let visited,np,_ = SrcDstMap.find_exn acc_j (i,j) in
        if (visited) then acc_j else
          dp_calc_numpaths i j topo dist_mat_sp acc_j)
      topo acc_i) topo init_vis_npath_pathlist_map in
  numPath_map

let print_mpapsp
  (numpath : (bool * int * (Topology.vertex * float) List.t) SrcDstMap.t)
  (topo : topology) =
  Topology.fold_vertexes
  (fun i acc_i ->
    Topology.fold_vertexes
    (fun j acc_j ->
      let v,np,pathlist = SrcDstMap.find_exn numpath (i,j) in
      Printf.printf "%s " (Node.name (Net.Topology.vertex_to_label topo i));
      Printf.printf "%s " (Node.name (Net.Topology.vertex_to_label topo j));
      Printf.printf "%B " v;
      Printf.printf "%d " np;
      Printf.printf "%d\n" (List.length pathlist);
      acc_j
    ) topo []) topo []


let get_random_path (i:Topology.vertex) (j:Topology.vertex) (numpath: (bool *
int * (Topology.vertex * float) List.t) SrcDstMap.t) (topo:topology) =
  Printf.printf "-------------------------------------\n";
  Printf.printf "%s " (Node.name (Net.Topology.vertex_to_label topo i));
  Printf.printf "%s\n" (Node.name (Net.Topology.vertex_to_label topo j));
  let p = ref [] in
  let curr = ref i in
  let stop_cond = ref false in
  while not !stop_cond do
    let _,_,nhop_list = SrcDstMap.find_exn numpath (!curr,j) in
    let rand = Random.float 1.0 in
    let chosen_hop,_ = List.fold_left nhop_list ~init:(i,false) ~f:(fun acc (nhop, prob) ->
      if prob < (rand -. 0.00001) then acc
      else let _, found_bool = acc in
      if not found_bool then (nhop,true)
      else acc
    ) in
    p := List.append !p [chosen_hop];
    curr := chosen_hop;
    stop_cond := if !curr = j then true else false;
  done;
  let v_path = !p in
  let _ = List.fold_left v_path ~init:[] ~f:(
    fun acc v -> Printf.printf "%s " (Node.name (Net.Topology.vertex_to_label topo v));
    acc) in
  Printf.printf "\n-------------------------------------\n";
  let e_path = ref [] in
  let _ = List.fold_left v_path ~init:i ~f:(
  fun last_v curr_v ->
    if last_v = curr_v then curr_v
    else
    let e = Topology.find_edge topo last_v curr_v in
    e_path := List.append !e_path [e];
    curr_v
  ) in
  !e_path


(*--------------------------------------------------------*)


let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo ~f:(fun _ _ -> true) in
  let mpapsp = multiple_apsp topo in
  let _ = print_mpapsp mpapsp topo in
  let spf_table =
    List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) -> 
      let rand_path =  get_random_path v1 v2 mpapsp topo in
    SrcDstMap.add acc ~key:(v1,v2) ~data:rand_path) in 

  let find_path src dst = SrcDstMap.find_exn spf_table (src,dst) in  

  (* TODO(rjs): replace the above code with a working version of the below code *)
  let host_set =
    VertexSet.filter
      (vertexes topo)
      ~f:(fun v ->
          let lbl = vertex_to_label topo v in
          Node.device lbl = Node.Host) in
  let paths_hash = all_shortest_paths_multi topo host_set in
  let find_path_random src dst =
    (* TODO(rjs): need to iterate over the list of destinations, and get paths.
       probably better to make a hash *)
    let _ = Hashtbl.Poly.find src paths_hash in 
    assert false in  
  
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
  
  (* NB: folding over apsp just to get all src-dst pairs *)
  let scheme = 
    List.fold_left 
      apsp 
      ~init:SrcDstMap.empty
      ~f:(fun acc (_,v1,v2,_) ->
	    match (device v1, device v2) with 
	    | (Node.Host,Node.Host) -> 
	       SrcDstMap.add acc ~key:(v1,v2) ~data:( vlb_pps v1 v2 ) 
	    | _ -> acc
      ) in
  (* Printf.printf "%s\n" (dump_scheme topo scheme); *)
  scheme
