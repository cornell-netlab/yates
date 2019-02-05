open Core

open LP_Lang
open Util
open Yates_types.Types

let state_base_path_set = ref SrcDstMap.empty


let var_name_uid id =
  Printf.sprintf "f_%d" id

(* Same as above, but for the flow in the reverse direction (j,i). *)
let var_name_rev topo edge d_pair =
  let src,_ = Topology.edge_src edge in
  let dst,_ = Topology.edge_dst edge in
  Printf.sprintf "f_%s_%s--%s"
    (string_of_pair topo d_pair)
    (name_of_vertex topo dst)
    (name_of_vertex topo src)

let objective = Var "Z"

let capacity_constraints (pmap : path_uid_map) (emap : edge_uidlist_map)
      (topo : topology) (d : demands)
      (init_acc : constrain list) : constrain list =
  (* For every edge, there is a capacity constraint *)
  Topology.fold_edges
    (fun edge acc ->
       (* The sum of all commodity flows in both direction must exceed
          the capacity by less than Z * capacity. *)

       match (EdgeMap.find emap edge) with
       | None -> acc
       | Some uid_list ->
         let all_flows = List.map ~f:(fun x -> Var(var_name_uid x)) uid_list in
         (* Add them all up *)
         let total_flow = Sum (all_flows) in
         let scaled_cap = Times ((capacity_of_edge topo edge) /. cap_divisor, objective) in
         (* Total flow is at most the scaled capacity *)
         let constr = minus total_flow scaled_cap in
         let name = Printf.sprintf "cap_%s"
                      (string_of_edge topo edge) in
         (Leq (name, constr, 0.))::acc) topo init_acc

let demand_constraints (pmap : path_uid_map) (emap : edge_uidlist_map)
      (topo : topology) (d : demands) (base_path_set : (path List.t) SrcDstMap.t)
      (init_acc : constrain list) : constrain list =
  (* Every source-sink pair has a demand constraint *)
  SrcDstMap.fold
    ~init:init_acc
    ~f:(fun ~key:(src,dst) ~data:(demand) acc ->
      if (src = dst) then acc
      else
        (* We need to add up the rates for all paths in pmap(src,dst) *)
        match SrcDstMap.find base_path_set (src,dst) with
        | None -> if (demand <= 0.) then acc else (assert false)
        | Some path_list ->
          let all_flows =
            List.fold_left
              ~init:[]
              ~f:(fun acc p ->
                let pvar = match PathMap.find pmap p with
                  | None -> assert false
                  | Some id -> Var(var_name_uid id) in
                pvar::acc ) path_list in
          (* some code to generate a constraint *)
          let total_flow = Sum(all_flows) in
          let name = Printf.sprintf "dem-%s-%s" (name_of_vertex topo src)
                       (name_of_vertex topo dst) in
          (Geq (name, total_flow, demand /. demand_divisor))::acc) d


let lp_of_maps (pmap:path_uid_map) (emap:edge_uidlist_map) (topo:topology)
      (d:demands) (base_path_set : (path List.t) SrcDstMap.t) : (arith_exp * constrain list) =
  let cap_constrs =
    capacity_constraints pmap emap topo d [] in
  let cap_and_demand =
    demand_constraints pmap emap topo d base_path_set cap_constrs in
  assert (List.length cap_constrs > 0);
  assert (List.length cap_and_demand > 0);
  (objective, cap_and_demand)

let rec new_rand () : float =
  let rand = (Random.float 1.0) in
  let try_fn = (Printf.sprintf "/tmp/semimcf_%f.lp" rand) in
  match Sys.file_exists try_fn with
    `Yes -> new_rand ()
  | _ -> rand

(* Run everything. Given a topology and a set of pairs with demands,
   returns the optimal congestion ratio, the paths used, and the number
   of paths used. *)
(* let solver_paths topo pairs verbose = *)

let solve_lp (pmap:int PathMap.t) (emap:int list EdgeMap.t) (topo:topology)
      (d:demands) (base_path_set : (path List.t) SrcDstMap.t)
  : (float * (int * float) list)=
  let lp = lp_of_maps pmap emap topo d base_path_set in
  let rand = new_rand () in
  let lp_filename = (Printf.sprintf "/tmp/semimcf_%f.lp" rand) in
  let lp_solname = (Printf.sprintf "/tmp/semimcf_%f.sol" rand) in

  (* Serialize LP and call Gurobi *)
  serialize_lp lp lp_filename;
  call_gurobi lp_filename lp_solname;

  (* read back all the edge flows from the .sol file *)
  let ratio, flows =
    In_channel.with_file
      lp_solname
      ~f:(fun file ->
        let result_str = "^f_\\([a-zA-Z0-9]+\\) \\([0-9.e+-]+\\)$" in
        let regex = Str.regexp result_str in
        In_channel.fold_lines
          file
          ~init:(0.,[])
          ~f:(fun (opt_z,flows) line ->
            match line with
            | "" -> (opt_z,flows)
            | _ ->
              if line.[0] = '#' then (opt_z, flows)
              else if line.[0] = 'Z' then
                let ratio_str = Str.string_after line 2 in
                let ratio = Float.of_string ratio_str in
                (ratio *. demand_divisor /. cap_divisor, flows)
              else
              if (Str.string_match regex line 0)
              then
                let id = Int.of_string (Str.matched_group 1 line) in
                let flow_amt = Float.of_string (Str.matched_group 2 line) in
                (* Printf.printf "%d %f\n" id flow_amt;  *)
                let tup = (id, flow_amt) in
                (opt_z, tup::flows)
              else
                (opt_z, flows))) in
  ignore (Sys.remove lp_filename);
  ignore (Sys.remove lp_solname);
  (ratio,flows)


let scheme_and_flows flows umap : (scheme * float SrcDstMap.t) =
  let (unnormalized_scheme, flow_sum) =
    List.fold_left
      flows
      ~init:(SrcDstMap.empty, SrcDstMap.empty)
      ~f:(fun (us,fs) (id,flow_val) ->
        match UidMap.find umap id with
        | None -> failwith "unrecognized uid in Gurobi solution"
        (* This should never happen, so if the Gurobi output
           contains an unrecognized UID, throw an error. *)
        | Some (u,v,path) -> (* u = source, v = destination, p = path *)
          let new_us_data = match SrcDstMap.find us (u,v) with
            | None -> let pm = PathMap.empty in PathMap.set ~key:path ~data:flow_val pm
            | Some pm -> add_or_increment_path pm path flow_val in
          let new_fs_data = match SrcDstMap.find fs (u,v) with
            | None -> flow_val
            | Some fv -> fv +. flow_val in
          let new_us = SrcDstMap.set ~key:(u,v) ~data:new_us_data us in
          let new_fs = SrcDstMap.set ~key:(u,v) ~data:new_fs_data fs in
          (new_us,new_fs) )  in
  (unnormalized_scheme, flow_sum)

(* Now normalize the values in the scheme so that they sum to 1 for each source-dest pair *)
let normalize (unnormalized_scheme:scheme) (flow_sum:float SrcDstMap.t) : scheme =
  SrcDstMap.fold
    unnormalized_scheme
    ~init:(SrcDstMap.empty)
    ~f:(fun ~key:(u,v) ~data:f_decomp acc  ->
       match SrcDstMap.find flow_sum (u,v) with
       | None -> assert false
       | Some sum_rate ->
           ignore (if (sum_rate <= 0.) then Printf.printf "sum_rate = %f on flow\n" sum_rate);
           let default_value = 1.0 /. (Float.of_int (PathMap.length f_decomp) ) in
           let normalized_f_decomp =
             PathMap.fold
             ~init:(PathMap.empty)
             ~f:(fun ~key:path ~data:rate acc ->
               let normalized_rate =
                 if sum_rate <= 0. then
                   default_value
                 else
                   rate /. sum_rate in
               PathMap.set ~key:path ~data:normalized_rate acc) f_decomp in
             SrcDstMap.set ~key:(u,v) ~data:normalized_f_decomp acc)

let initialize (s:scheme) : unit =
  ignore (if (SrcDstMap.is_empty s) then
            failwith "SemiMcf must be initialized with a non-empty scheme"
          else ());
  let b = SrcDstMap.fold s ~init:SrcDstMap.empty
            ~f:(fun ~key:(u,v) ~data:pp_map acc ->
              SrcDstMap.set ~key:(u,v)
                ~data:(List.map ~f:fst (PathMap.to_alist pp_map)) acc) in
  state_base_path_set := b;
  ()

let restricted_mcf (topo:topology) (d:demands)
      (base_path_set : (path List.t) SrcDstMap.t) : scheme =
  let uuid = ref (-1) in
  let fresh_uid () =
    uuid := !uuid + 1;
    !uuid
  in

  let (umap,pmap,emap) =
    SrcDstMap.fold
      base_path_set (* fold over the base path set *)
      ~init:(UidMap.empty, PathMap.empty, EdgeMap.empty)
      (* for every pair of hosts u,v *)
      ~f:(fun ~key:(u,v) ~data:path_list acc ->
        if (u = v) then acc
        else
          begin
            List.fold_left
              path_list
              ~init:acc
              (* get the possible paths, and for every path *)
              ~f:(fun (umap,pmap,emap) path ->
                let id = fresh_uid () in
                (*Printf.printf "\npath %d\t%d : " id (List.length path);*)
                let umap' = UidMap.set ~key:id ~data:(u,v,path) umap in
                let pmap' = PathMap.set ~key:path ~data:id pmap in
                (* get the edges in the path *)
                (* This assertion fails because we have some paths with no edges *)
                assert (not (List.is_empty path));
                let emap' =
                  List.fold_left
                    path
                    ~init:emap
                    ~f:(fun emap e ->
                      let ids = match (EdgeMap.find emap e ) with
                        | None -> [id]
                        | Some ids -> id::ids in
                      (*Printf.printf "%s " (string_of_edge topo e) ;*)
                      EdgeMap.set ~key:e ~data:ids emap) in
                (umap',pmap',emap'))
          end) in

  assert (not (EdgeMap.is_empty emap));

  let (ratio, flows) =  solve_lp pmap emap topo d base_path_set in
  let (unnormalized_scheme, flow_sum) = scheme_and_flows flows umap in
  let new_scheme = normalize unnormalized_scheme flow_sum in
  if (SrcDstMap.is_empty new_scheme) then assert false;
  new_scheme


let solve (topo:topology) (d:demands) : scheme =
  ignore (if (SrcDstMap.is_empty !state_base_path_set) then
            failwith "Yates_SemiMcf: stateful base path set empty!"
          else ());
  let base_path_set = !state_base_path_set in
  restricted_mcf topo d base_path_set

let local_recovery (_:scheme) (topo:topology) (failed_links:failure)
      (d:demands) : scheme =
  Printf.printf "\t\t\t\t\t\t\t\t\t   L-REC\r";
  let is_path_alive (p:path) : bool =
    List.fold_left p
      ~init:true
      ~f:(fun valid edge ->
        let edge_is_safe = not (EdgeSet.mem failed_links edge) in
        valid && edge_is_safe) in
  let new_base_path_set =
    SrcDstMap.fold !state_base_path_set
      ~init:SrcDstMap.empty
      ~f:(fun ~key:(src,dst) ~data:path_list acc ->
        let n_paths = List.fold_left path_list
                        ~init:[]
                        ~f:(fun acc p -> if (is_path_alive p) then p::acc else acc) in
        SrcDstMap.set ~key:(src,dst) ~data:n_paths acc) in
  (* If there is no path in base set for a u-v pair, then MCF produces an
   * empty scheme. To avoid this, we set u-v demand = 0  *)
  let new_demands =
    SrcDstMap.fold new_base_path_set
      ~init:SrcDstMap.empty
      ~f:(fun ~key:(u,v) ~data:bpset acc ->
        let uv_dem = if (List.length bpset) = 0
          then 0.
          else SrcDstMap.find_exn d (u,v) in
        SrcDstMap.set ~key:(u,v) ~data:uv_dem acc) in
  let new_scheme = restricted_mcf topo new_demands new_base_path_set in
  new_scheme
