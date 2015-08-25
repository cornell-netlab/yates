open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net

(* This implements the Algorithm in Greedy Distributed Optimization 
    of Multi-Commodity Flows by Awerbuch and Khandekar *)
       
let mu = ref Float.nan

(* Make epsilon bigger for faster solving and worse approximation *)             
let epsilon = 0.5
                
let alpha (t:topology) : float = epsilon /. (40.0 *. (log (Float.of_int (Topology.num_edges t))))

let beta (t:topology) : float = (alpha t) *. (epsilon /. (log (Float.of_int (Topology.num_edges t))))

let f_umlaut (e:edge) (muu:float) (t:topology) = 
  let fm = Float.of_int (Topology.num_edges t) in
  (muu /. (log fm)) *. ((capacity_of_edge t e) /. (1.0 +. (beta t))) *.
  ((log (1.0 +. ((alpha t) /. 8.0))) /. (log 2.0))

let find_max_flow f = EdgeMap.fold ~init:0.0 ~f:(fun ~key:e ~data:r acc -> Float.max_inan acc r) f                     

let derivative_phi (e:edge) (muu:float) (x:float) (t:topology) : float =
  let fm = Float.of_int (Topology.num_edges t) in
  ((log fm) /. ((capacity_of_edge t e) *. muu)) *. 
  fm ** (x /. ((capacity_of_edge t e) *. muu))

let path_gradient (p:path) (muu:float) (f:flow) (t:topology) : float =
  List.fold_left ~init:0.0 ~f:(fun acc e ->
                                 acc +. match EdgeMap.find f e with
                                 | None -> 0.0
                                 | Some x -> (derivative_phi e muu x t)
                              ) p
                                   
(* path_update adds a specified value (rate) to the flow on each edge in the path p*)             
let path_update (p:path) (rate:float) (f:flow) : flow =
  List.fold_left ~init:f ~f:(fun acc e -> let old_rate =
                                               match EdgeMap.find f e with
                                               | None -> assert false
                                               | Some demand -> demand in
                                             EdgeMap.add ~key:e ~data:(old_rate +. rate) acc ) p

let solve (topo:topology) (d:demands) (s:scheme) : scheme =
  (* First build HashMaps, keyed by edges, containing the
     values f(e), f_i(e), from the pseudocode. *)
  let f' = Topology.fold_edges (fun edge acc -> EdgeMap.add acc ~key:edge ~data:0.0 ) topo EdgeMap.empty in
  let f_i' = List.fold_left ~init:SrcDstMap.empty ~f:(fun acc (u,v,r) -> SrcDstMap.add ~key:(u,v) ~data:f' acc) d in

  (* populate f,f_i according to what we saw in the last scheme *)
  let (f,f_i) = List.fold_left
                  ~init:(f',f_i')
                  ~f:(fun (f,f_i) (u,v,r) ->
                      let path_map = match SrcDstMap.find s (u,v) with
                        | None -> assert false
                        | Some path_map -> path_map in
                      (* for each (k,v) pair: let f' = call path_update k r*v f in *)
                      PathMap.fold
                        ~init:(f,f_i)
                        ~f:(fun ~key:p
                                ~data:x acc ->
                            let f'' = path_update p (r*.x) f in
                            let f_i'' = let f = match SrcDstMap.find f_i (u,v) with
                                          | None -> assert false
                                          | Some f -> f in                                                                                                
                                        SrcDstMap.add ~key:(u,v) ~data:(path_update p (r*.x) f) f_i in
                            (f'', f_i'')) path_map ) d in
  

  (* recompute mu, RouteMetric line 1 *)
  mu := Float.min_inan (!mu) ( 2.0 ** (Float.round_down ((log (epsilon *. (find_max_flow f))) /. (log 2.0))) ) ;

  (* RouteMetric line 2 defines phi, never used. Instead the derivative is used. *)

  (* RouteMetric line 3 *)

  
  SrcDstMap.empty
    
