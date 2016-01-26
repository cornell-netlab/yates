open Core.Std
open Frenetic_Network
open Net
open Kulfi_Types
open Kulfi_Routing
open Kulfi_Traffic
open Simulate_Exps
open Simulate_Demands
open RunningStat
open ExperimentalData
open AutoTimer
open Kulfi_Globals
open Kulfi_Util

let demand_envelope = ref SrcDstMap.empty

let edge_connects_switches (e:edge) (topo:topology) : bool =
  let src,_ = Topology.edge_src e in
  let src_label = Topology.vertex_to_label topo src in
  let dst,_ = Topology.edge_dst e in
  let dst_label = Topology.vertex_to_label topo dst in
  Node.device src_label = Node.Switch && Node.device dst_label = Node.Switch


(* create a scheme by running solver over all failures and aggregating the schemes *)
let all_failures_envelope solver (topo:topology) (envelope:demands) : scheme =
  (* consider each edge can fail and compute the corresponding scheme *)
  let hosts = get_hosts topo in
  let failure_scheme_map,_ = EdgeSet.fold (Topology.edges topo)
    ~init:(EdgeMap.empty, EdgeSet.empty)
    ~f:(fun (acc, handled_edges) e ->
      if (EdgeSet.mem handled_edges e) then (acc, handled_edges)
      else
        let e' = match Topology.inverse_edge topo e with
          | None -> Printf.printf "%s\n%!" (string_of_edge topo e); failwith "No reverse edge found"
          | Some x -> x in
        let handled_edges' = EdgeSet.add handled_edges e in
        let handled_edges' = EdgeSet.add handled_edges' e' in
 
        let failure_scen = EdgeSet.add (EdgeSet.singleton e) e' in
        if edge_connects_switches e topo then
          let topo' = EdgeSet.fold failure_scen
            ~init:topo
            ~f:(fun acc link -> Topology.remove_edge acc link) in
          (* consider only the failures which do not partition the network *)
          let spf_scheme = Kulfi_Spf.solve topo' SrcDstMap.empty in
          if all_pairs_connectivity topo' hosts spf_scheme then
            begin
            let sch = solver topo' envelope in
            assert (not (SrcDstMap.is_empty sch));
            (EdgeMap.add ~key:e ~data:sch acc, handled_edges')
            end
          else (acc, handled_edges')
        else (acc, handled_edges')) in
  (* merge all the schemes *)
  assert (not (EdgeMap.is_empty failure_scheme_map));
  let agg_scheme = EdgeMap.fold failure_scheme_map
    ~init:SrcDstMap.empty
    ~f:(fun ~key:e ~data:edge_scheme agg ->
      (* merge edge_schme and agg *)
      assert (not (SrcDstMap.is_empty edge_scheme));
      SrcDstMap.fold edge_scheme
        ~init:agg
        ~f:(fun ~key:(s,d) ~data:f_pp_map res ->
          let acc_pp_map = match SrcDstMap.find res (s,d) with
            | None -> PathMap.empty
            | Some x -> x in
          if (PathMap.is_empty f_pp_map) then
            begin
              Printf.printf "%s: Scheme %s" (string_of_edge topo e) (dump_scheme topo edge_scheme);
              assert false
            end
          else
          let n_pp_map = PathMap.fold f_pp_map
            ~init:acc_pp_map
            ~f:(fun ~key:path ~data:f_prob acc_paths ->
              let acc_prob = match PathMap.find acc_paths path with
                | None -> 0.
                | Some x -> x in
              PathMap.add ~key:path ~data:(acc_prob +. f_prob) acc_paths) in
          SrcDstMap.add ~key:(s,d) ~data:n_pp_map res)) in
  (* normalize scheme *)
  assert (not (SrcDstMap.is_empty agg_scheme));
  normalize_scheme_opt agg_scheme


type solver_type =
  | Mcf | MwMcf | Vlb | Ecmp | Ksp | Spf | Raeke
  | AkMcf | AkVlb | AkRaeke | AkEcmp | AkKsp
  | SemiMcfMcf | SemiMcfMcfEnv | SemiMcfMcfFTEnv | SemiMcfVlb | SemiMcfRaeke | SemiMcfRaekeFT  | SemiMcfEcmp | SemiMcfKsp | SemiMcfKspFT
  | OptimalMcf

let solver_to_string (s:solver_type) : string =
  match s with
  | Mcf -> "mcf"
  | MwMcf -> "mwmcf"
  | Vlb -> "vlb"
  | Ecmp -> "ecmp"
  | Ksp -> "ksp"
  | Spf -> "spf"
  | Raeke -> "raeke"
  | AkMcf -> "akmcf"
  | AkVlb -> "akvlb"
  | AkRaeke -> "akraeke"
  | AkEcmp -> "akecmp"
  | AkKsp -> "akksp"
  | SemiMcfMcf -> "semimcfmcf"
  | SemiMcfMcfEnv -> "semimcfmcfenv"
  | SemiMcfMcfFTEnv -> "semimcfmcfftenv"
  | SemiMcfVlb -> "semimcfvlb"
  | SemiMcfRaeke -> "semimcfraeke"
  | SemiMcfRaekeFT -> "semimcfraekeft"
  | SemiMcfEcmp -> "semimcfecmp"
  | SemiMcfKsp -> "semimcfksp"
  | SemiMcfKspFT -> "semimcfkspft"
  | OptimalMcf -> "optimalmcf"

let select_algorithm solver = match solver with
  | Mcf -> Kulfi_Routing.Mcf.solve
  | OptimalMcf -> Kulfi_Routing.Mcf.solve
  | MwMcf -> Kulfi_Routing.MwMcf.solve
  | Vlb -> Kulfi_Routing.Vlb.solve
  | Ecmp -> Kulfi_Routing.Ecmp.solve
  | Ksp -> Kulfi_Routing.Ksp.solve
  | Spf -> Kulfi_Routing.Spf.solve
  | Raeke -> Kulfi_Routing.Raeke.solve
  | AkMcf
  | AkVlb
  | AkRaeke
  | AkKsp
  | AkEcmp -> Kulfi_Routing.Ak.solve
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfVlb
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfEcmp -> Kulfi_Routing.SemiMcf.solve

let select_local_recovery solver = match solver with
  | Mcf -> Kulfi_Routing.Mcf.local_recovery
  | OptimalMcf -> failwith "No local recovery for optimal mcf" 
  | MwMcf -> Kulfi_Routing.MwMcf.local_recovery
  | Vlb -> Kulfi_Routing.Vlb.local_recovery
  | Ecmp -> Kulfi_Routing.Ecmp.local_recovery
  | Ksp -> Kulfi_Routing.Ksp.local_recovery
  | Spf -> Kulfi_Routing.Spf.local_recovery
  | Raeke -> Kulfi_Routing.Raeke.local_recovery
  | AkMcf
  | AkVlb
  | AkRaeke
  | AkKsp
  | AkEcmp -> Kulfi_Routing.Ak.local_recovery
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfVlb
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfEcmp -> Kulfi_Routing.SemiMcf.local_recovery


let initial_scheme algorithm topo predict : scheme =
  match algorithm with
  | SemiMcfMcfEnv ->
     Kulfi_Routing.Mcf.solve topo !demand_envelope
  | SemiMcfMcfFTEnv ->
     all_failures_envelope Kulfi_Routing.Mcf.solve topo !demand_envelope
  | SemiMcfMcf
  | AkMcf ->
     Kulfi_Routing.Mcf.solve topo predict
  | SemiMcfVlb
  | AkVlb ->
     let _ = Kulfi_Routing.Vlb.initialize SrcDstMap.empty in
     Kulfi_Routing.Vlb.solve topo SrcDstMap.empty
  | SemiMcfRaekeFT ->
     let _ = Kulfi_Routing.Raeke.initialize SrcDstMap.empty in
     all_failures_envelope Kulfi_Routing.Raeke.solve topo SrcDstMap.empty 
  | SemiMcfRaeke
  | AkRaeke ->
     let _ = Kulfi_Routing.Raeke.initialize SrcDstMap.empty in
     Kulfi_Routing.Raeke.solve topo SrcDstMap.empty
  | SemiMcfEcmp
  | AkEcmp ->
     let _ = Kulfi_Routing.Ecmp.initialize SrcDstMap.empty in
     Kulfi_Routing.Ecmp.solve topo SrcDstMap.empty
  | SemiMcfKspFT ->
     let _ = Kulfi_Routing.Ksp.initialize SrcDstMap.empty in
     all_failures_envelope Kulfi_Routing.Ksp.solve topo SrcDstMap.empty 
  | SemiMcfKsp
  | AkKsp ->
     let _ = Kulfi_Routing.Ksp.initialize SrcDstMap.empty in
     Kulfi_Routing.Ksp.solve topo SrcDstMap.empty
  | _ -> SrcDstMap.empty

let initialize_scheme algorithm topo predict: unit =
  Printf.printf "[Init...] \r";
  let start_scheme = initial_scheme algorithm topo predict in
  let pruned_scheme = if SrcDstMap.is_empty start_scheme
    then start_scheme
    else prune_scheme topo start_scheme !Kulfi_Globals.budget in
  (*Printf.printf "%s\n%!" (dump_scheme topo start_scheme);*)
  match algorithm with
  | SemiMcfEcmp
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfVlb -> Kulfi_Routing.SemiMcf.initialize pruned_scheme
  | AkEcmp
  | AkKsp
  | AkMcf
  | AkRaeke
  | AkVlb -> Kulfi_Routing.Ak.initialize pruned_scheme
  | Ecmp -> Kulfi_Routing.Ecmp.initialize SrcDstMap.empty
  | Ksp -> Kulfi_Routing.Ksp.initialize SrcDstMap.empty
  | Raeke -> Kulfi_Routing.Raeke.initialize SrcDstMap.empty
  | Vlb -> Kulfi_Routing.Vlb.initialize SrcDstMap.empty
  | _ -> ()

let solve_within_budget algorithm topo predict actual: (scheme * float) =
  let at = make_auto_timer () in
  start at;
  let solve = select_algorithm algorithm in
  let budget' = match algorithm with
    | OptimalMcf -> Int.max_value / 100
    | _ -> !Kulfi_Globals.budget in
	let sch = match algorithm with 
    | OptimalMcf -> prune_scheme topo (solve topo actual) budget' 
    | _ -> prune_scheme topo (solve topo predict) budget' in
  stop at;
  assert (probabilities_sum_to_one sch);
  sch, (get_time_in_seconds at)

let congestion_of_paths (s:scheme) (t:topology) (d:demands) : (float EdgeMap.t) =
  let sent_on_each_edge =
    SrcDstMap.fold
      s
      ~init:EdgeMap.empty
      ~f:(fun ~key:(src,dst) ~data:paths acc ->
          PathMap.fold paths
            ~init:acc
            ~f:(fun ~key:path ~data:prob acc ->
                List.fold_left path
                  ~init:acc
                  ~f:(fun acc e ->
                      let demand =
                        match SrcDstMap.find d (src,dst) with
                        | None -> 0.0
                        | Some x -> x in
                      match EdgeMap.find acc e with
                      | None -> EdgeMap.add ~key:e ~data:(demand *. prob) acc
                      | Some x ->  EdgeMap.add ~key:e ~data:((demand *. prob) +. x) acc)))
  in
  EdgeMap.fold
    ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:amount_sent acc ->
        EdgeMap.add ~key:e ~data:(amount_sent /. (capacity_of_edge t e)) acc) sent_on_each_edge

(* TODO(rjs): Do we count paths that have 0 flow ? *)
let get_churn (old_scheme:scheme) (new_scheme:scheme) : float =
  let get_path_sets (s:scheme) : PathSet.t =
    SrcDstMap.fold
      ~init:PathSet.empty
      ~f:(fun ~key:_
	      ~data:d acc ->
	  PathMap.fold
	    ~init:acc
	    ~f:(fun ~key:p ~data:_ acc ->
		PathSet.add acc p ) d) s in
  let set1 = get_path_sets old_scheme in
  let set2 = get_path_sets new_scheme in
  let union = PathSet.union set1 set2 in
  let inter = PathSet.inter set1 set2 in
  Float.of_int (PathSet.length (PathSet.diff union inter))

(* compare paths based on string representation *)
let get_churn_string (topo:topology) (old_scheme:scheme) (new_scheme:scheme) : float =
  let get_path_sets (s:scheme) : StringSet.t =
    SrcDstMap.fold
      ~init:StringSet.empty
      ~f:(fun ~key:_
	      ~data:d acc ->
	  PathMap.fold
	    ~init:acc
	    ~f:(fun ~key:p ~data:_ acc ->
		StringSet.add acc (dump_edges topo p)) d) s in
  let set1 = get_path_sets old_scheme in
  let set2 = get_path_sets new_scheme in
  let union = StringSet.union set1 set2 in
  let inter = StringSet.inter set1 set2 in
  (*let _ = StringSet.iter (StringSet.diff union inter) ~f:(fun s -> Printf.printf
  "%s\n%!"
  s) in*)
  Float.of_int (StringSet.length (StringSet.diff union inter))

(* Get a map from path to it's probability and src-dst demand *)
let get_path_prob_demand_map (s:scheme) (d:demands) : (probability * demand) PathMap.t =
  (* (Probability, net s-d demand) for each path *)
  SrcDstMap.fold s
  ~init:PathMap.empty
  ~f:(fun ~key:(src,dst) ~data:paths acc ->
    let demand = match SrcDstMap.find d (src,dst) with
                 | None -> 0.0
                 | Some x -> x in
    PathMap.fold
    paths
    ~init:acc
    ~f:(fun ~key:path ~data:prob acc ->
      match PathMap.find acc path with
              | None ->  PathMap.add ~key:path ~data:(prob, demand) acc
              | Some x -> if path <> [] then failwith "Duplicate paths should not be present"
                          else acc))

(* Get a map from path to it's probability and src-dst demand *)
let get_path_prob_demand_arr (s:scheme) (d:demands) =
  (* (Probability, net s-d demand) for each path *)
  SrcDstMap.fold s
  ~init:[]
  ~f:(fun ~key:(src,dst) ~data:paths acc ->
    let demand = match SrcDstMap.find d (src,dst) with
                 | None -> 0.0
                 | Some x -> x in
    PathMap.fold
    paths
    ~init:acc
    ~f:(fun ~key:path ~data:prob acc ->
            let arr_path = Array.of_list path in
    (arr_path, 0, (prob, demand))::acc))


(* Sum througput over all src-dst pairs *)
let get_total_tput (sd_tput:throughput SrcDstMap.t) : throughput =
  SrcDstMap.fold
  sd_tput
  ~init:0.0
  ~f:(fun ~key:_ ~data:delivered acc -> acc +. delivered)

(* Aggregate latency-tput over all sd-pairs *)
let get_aggregate_latency (sd_lat_tput_map_map:(throughput LatencyMap.t) SrcDstMap.t) (num_iter:int): (throughput LatencyMap.t) =
  SrcDstMap.fold sd_lat_tput_map_map
  ~init:LatencyMap.empty
  ~f:(fun ~key:_ ~data:lat_tput_map acc ->
    LatencyMap.fold lat_tput_map
    ~init:acc
    ~f:(fun ~key:latency ~data:tput acc ->
      let prev_agg_tput = match LatencyMap.find acc latency with
                      | None -> 0.0
                      | Some x -> x in
      let agg_tput = prev_agg_tput +. (tput /. (Float.of_int num_iter)) in
      LatencyMap.add ~key:latency ~data:(agg_tput) acc))

(* Global recovery: recompute routing scheme after removing failed links *)
let global_recovery (failed_links:failure) (predict:demands) (actual:demands) (algorithm:solver_type) (topo:topology) : (scheme * float) =
  Printf.printf "\t\t\t\t\t\t\t\t\t\t\tGlobal\r";
  let topo' = EdgeSet.fold failed_links
    ~init:topo
    ~f:(fun acc link -> Topology.remove_edge acc link) in

  ignore(EdgeSet.iter failed_links
  ~f:(fun e -> assert ((EdgeSet.mem (Topology.edges topo) e) &&
      not (EdgeSet.mem (Topology.edges topo') e)) ););
  ignore(EdgeSet.iter (Topology.edges topo')
      ~f:(fun e -> assert (EdgeSet.mem (Topology.edges topo) e)));

  initialize_scheme algorithm topo' predict;
  let new_scheme,solver_time = solve_within_budget algorithm topo' predict actual in 
  ignore (if (SrcDstMap.is_empty new_scheme) then failwith "new_scheme is empty in global driver" else ());
  (*Printf.printf "New scheme: %s\n-----\n%!" (dump_scheme topo' new_scheme);*)
  Printf.printf "\t\t\t\t\t\t\t\t\t\t\tGLOBAL\r";
  new_scheme, solver_time

(* Return src and dst for a given path *)
let get_src_dst_for_path (p:path) =
  if p = [] then None
  else
    let first_link = match List.hd p with
                        | None -> failwith "Empty path"
                        | Some x -> x in
    let last_link = list_last p in
    let src,_ = Net.Topology.edge_src first_link in
    let dst,_ = Net.Topology.edge_dst last_link in
    Some (src, dst)

(* Return src and dst for a given path *)
let get_src_dst_for_path_arr (p:edge Array.t) =
  if Array.length p = 0 then None
  else
    let first_link = p.(0) in
    let last_link = p.((Array.length p)-1) in
    let src,_ = Net.Topology.edge_src first_link in
    let dst,_ = Net.Topology.edge_dst last_link in
    Some (src, dst)


(* Capacity of a link in a given failure scenario *)
let curr_capacity_of_edge (topo:topology) (link:edge) (fail:failure) : float =
  if EdgeSet.mem fail link
    then 0.0
    else capacity_of_edge topo link

(* check all-pairs connectivity after failure *)
let check_connectivity_after_failure (topo:topology) (fail:failure) : bool =
  let topo' = EdgeSet.fold fail
    ~init:topo
    ~f:(fun acc link -> Topology.remove_edge acc link) in
  let hosts = get_hosts topo' in
  let spf_scheme = Kulfi_Routing.Spf.solve topo' SrcDstMap.empty in
  all_pairs_connectivity topo' hosts spf_scheme


let rec get_util_based_failure_scenario (topo:topology) (utils: congestion EdgeMap.t) (alpha:float) : failure =
  (* failure prob is proportional to congestion ^ alpha *)
  let edge_utils = List.filter (EdgeMap.to_alist utils) (* don't fail links connecting hosts *)
    ~f:(fun (e,_) -> edge_connects_switches e topo) in
  let failure_weights = List.map ~f:(fun (e,c) -> (e, c ** alpha)) edge_utils in
  let total_weight = List.fold_left failure_weights ~init:0.0 ~f:(fun acc (_,w) -> acc +. w) in
  if total_weight = 0. then EdgeSet.empty
  else
    let rand = Random.float total_weight in
    let first_el = match List.hd failure_weights with
                | None -> failwith "empty list of utilizations"
                | Some x -> x in
    let (e,_),_ = List.fold_left failure_weights
      ~init:(first_el, rand)
      ~f:(fun (selected,sum) (e,w) ->
        if sum <= 0.0 then (selected,sum)
        else ((e,w), sum -. w)) in
    let e' = match Topology.inverse_edge topo e with
            | Some x -> x
            | None -> assert false in
    let fail = EdgeSet.add (EdgeSet.singleton e) e' in
    if check_connectivity_after_failure topo fail then fail
    else get_util_based_failure_scenario topo utils alpha

  
let rec get_scheme_max_util_failure (topo:topology) (utils: congestion EdgeMap.t) : failure =
  let edge_utils = List.filter (EdgeMap.to_alist utils) (* don't fail links connecting hosts *)
    ~f:(fun (e,_) -> edge_connects_switches e topo) in
	let sorted_edge_utils = List.sort ~cmp:(fun x y -> Float.compare (snd y) (snd x)) edge_utils in
  (*let _ = List.iter sorted_edge_utils ~f:(fun (e,c) -> Printf.printf "%s : %f\n%!" (string_of_edge topo e) c;) in*)
  let (e,_) = match List.hd sorted_edge_utils with
      | Some x -> x
      | None -> failwith "empty utilization list" in
  let e' = match Topology.inverse_edge topo e with
          | Some x -> x
          | None -> assert false in
  let fail = EdgeSet.add (EdgeSet.singleton e) e' in
  if check_connectivity_after_failure topo fail then fail
  else
    let rest_utils = EdgeMap.remove utils e in
    get_scheme_max_util_failure topo rest_utils 

let get_spf_util_based_failure (topo:topology) (actual:demands) (alpha:float) : failure =
  let spf_scheme = Kulfi_Routing.Spf.solve topo SrcDstMap.empty in
  let exp_utils = (congestion_of_paths spf_scheme topo actual) in
  get_util_based_failure_scenario topo exp_utils alpha
 
let get_spf_max_util_link (topo:topology) (actual:demands) : failure =
  let spf_scheme = Kulfi_Routing.Spf.solve topo SrcDstMap.empty in
  let exp_utils = (congestion_of_paths spf_scheme topo actual) in
  get_scheme_max_util_failure topo exp_utils
   
(* get a random failure of n edges*)
let rec get_random_failure (topo:topology) (num_fail:int) : failure =
  let rec add_new_elem (selected : EdgeSet.t) (all_edges : edge List.t) =
    let rand = Random.int (List.length all_edges) in
    let rand_edge = match List.nth all_edges rand with
        | None -> assert false
        | Some x -> x in
    if EdgeSet.mem selected rand_edge then add_new_elem selected all_edges
    else 
      let rev_edge = match Topology.inverse_edge topo rand_edge with
        | Some x -> x
        | None -> assert false in
      let selected = EdgeSet.add selected rand_edge in
      EdgeSet.add selected rev_edge in

  let all_edges = EdgeSet.elements (Topology.edges topo) in
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let fail_set = List.fold_left (range 0 num_fail)
    ~init:EdgeSet.empty
    ~f:(fun acc i ->
      add_new_elem acc all_edges) in
  if check_connectivity_after_failure topo fail_set then fail_set
  else get_random_failure topo num_fail

		  
(* Create some failure scenario *)
let rec get_test_failure_scenario (topo:topology) (actual:demands) (iter_pos:float) (num_fail:int): failure =
  if (Float.of_int (Topology.num_edges topo))/.2. -.
  (Float.of_int(Topology.num_vertexes topo)) < (Float.of_int (num_fail)) then failwith "Not good enough topo for num_fail failures"
  else
  if num_fail = 0 then EdgeSet.empty else
  if num_fail > 1 then get_random_failure topo num_fail else

  let iter_pos = min 1. iter_pos in
  let spf_scheme = Kulfi_Routing.Spf.solve topo SrcDstMap.empty in
(*  SrcDstMap.iter ~f:(fun ~key:(s,d) ~data:path ->
  Printf.printf "%s %s : %d\n%!" 
  (Node.name (Net.Topology.vertex_to_label topo s))
  (Node.name (Net.Topology.vertex_to_label topo d))
  (PathMap.fold path ~init:10000 ~f:(fun ~key:p ~data:_ acc -> List.length p))) spf_scheme;*)

  let exp_utils = (congestion_of_paths spf_scheme topo actual) in
  let edge_utils = List.filter (EdgeMap.to_alist exp_utils) (* don't fail links connecting hosts *)
    ~f:(fun (e,_) -> edge_connects_switches e topo) in
	let sorted_edge_utils = List.sort ~cmp:(fun x y -> Float.compare (snd y) (snd x)) edge_utils in
  (*let _ = List.iter sorted_edge_utils ~f:(fun (e,c) -> Printf.printf "%s : %f\n%!" (string_of_edge topo e) c;) in*)
  let f_sel_pos = ((Float.of_int (List.length sorted_edge_utils - 1))) *. iter_pos in
  let sel_pos = Int.of_float (Float.round_down f_sel_pos) in
  let (e,_) = match List.nth sorted_edge_utils sel_pos with
      | Some x -> x
      | None -> failwith "empty utilization list" in
  let e' = match Topology.inverse_edge topo e with
          | Some x -> x
          | None -> assert false in
  let f = EdgeSet.add (EdgeSet.singleton e) e' in
  if check_connectivity_after_failure topo f then f
  else if iter_pos >= 1. then EdgeSet.empty
  else get_test_failure_scenario topo actual (iter_pos +. (1. /. Float.of_int (List.length sorted_edge_utils))) num_fail


(*
  let all_edge_list = EdgeSet.elements (Topology.edges topo) in
  let edge_list = List.filter all_edge_list 
    ~f:(fun e -> edge_connects_switches e topo) in
  let f_sel_pos = (Float.of_int (List.length edge_list) -. 1.0) *. iter_pos in
  let sel_pos = Int.of_float (Float.round_down f_sel_pos) in
  let sel_edge = match List.nth edge_list sel_pos with
                  | None -> failwith "Invalid index to select edge"
                  | Some x -> x in
  Printf.printf "%d / %d\t%s\n%!" sel_pos (List.length edge_list) (string_of_edge topo sel_edge);
  EdgeSet.add (EdgeSet.singleton sel_edge) (match Topology.inverse_edge topo sel_edge with | Some e -> e | None -> assert false)
*)

(* For a given scheme, find the number of paths through each edge *)
let count_paths_through_edge (s:scheme) : (int EdgeMap.t) =
  SrcDstMap.fold s
  ~init:EdgeMap.empty
  ~f:(fun ~key:_ ~data:ppm acc ->
    PathMap.fold ppm
    ~init:acc
    ~f:(fun ~key:path ~data:_ acc ->
      List.fold_left path
      ~init:acc
      ~f:(fun acc edge ->
        let c = match EdgeMap.find acc edge with
                | None -> 0
                | Some x -> x in
        EdgeMap.add ~key:edge ~data:(c+1) acc)))

let get_failure_scenarios (topo:topology) (demand_file:string) (host_file:string) (iters:int) (number_failures:int) (scale:float) : (EdgeSet.t List.t) =
  let num_tm = min iters (List.length (In_channel.read_lines demand_file)) in
  let (demand_host_map, demand_ic) = open_demands demand_file host_file topo in
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let iterations = range 2 num_tm in
  let failure_scenarios = List.rev(List.fold_left iterations 
    ~init:[EdgeSet.empty; EdgeSet.empty]
    ~f:(fun acc n ->
		  let actual = next_demand ~scale:scale demand_ic demand_host_map in
      let failing_edges =
      (* get_scheme_max_util_link topo actual in*)
      (* get_util_based_failure_scenario topo exp_congestions in*)
      (* get_spf_util_based_failure topo actual exp_congestions in*)
      get_test_failure_scenario topo actual ((Float.of_int (n-2)) /. (Float.of_int num_tm)) number_failures in
      (*Printf.printf "Selected failure: %s\n%!" (dump_edges topo (EdgeSet.elements failing_edges));*)
      failing_edges::acc)) in
  close_demands demand_ic;
  failure_scenarios


let check_flash (topo:topology) (actual: demands) (predict: demands) : bool =
  Printf.printf "\t\t\t\t\tChecking flash\r%!";
  let hosts = get_hosts_set topo in
  VertexSet.fold hosts 
    ~init:false
    ~f:(fun acc sink ->
      let pred,act = VertexSet.fold hosts ~init:(0.,0.)
        ~f:(fun (pred_acc,act_acc) src ->
          if src = sink then (pred_acc,act_acc) else
          let p = SrcDstMap.find_exn predict (src,sink) in
          let a = SrcDstMap.find_exn actual (src,sink) in
          (pred_acc+.p, act_acc+.a)) in
      if (act /. pred > 1.) then true
      else acc)


(* Model flash *)
let flash_decrease (x:float) (t:float) (total_t:float) : float =
  total_t *. x /. (t *. 2.0 +. total_t)

let flash_demand_t x t d total_t =
  d *. (flash_decrease x t total_t)

let total_flow_to_sink (sink) (topo:topology) (actual:demands) : float =
  let hosts = get_hosts_set topo in
  VertexSet.fold hosts ~init:0. 
    ~f:(fun acc src -> if (src = sink) then acc else
      let d = SrcDstMap.find_exn actual (src,sink) in
      acc +. d)

let update_flash_demand topo sink dem flash_t per_src_flash_factor total_t: demands =
  if flash_t < 0 then dem
  else
    begin
    let hosts = get_hosts_set topo in
    VertexSet.fold hosts ~init:dem
      ~f:(fun acc src ->
        if (src = sink) then acc
        else
          let sd_dem = SrcDstMap.find_exn dem (src,sink) in
          let sd_dem' = flash_demand_t per_src_flash_factor (Float.of_int flash_t) sd_dem total_t in
          SrcDstMap.add ~key:(src,sink) ~data:sd_dem' acc)
    end
    
(* find the destination for flash crowd *)
let pick_flash_sinks (topo:topology) (iters:int) =
  let hosts = get_hosts topo in
  let num_hosts = (List.length hosts) in
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  List.fold_left (range 0 iters) ~init:[]
  ~f:(fun acc n ->
    let selected = match List.nth hosts (n % num_hosts) with
      | None -> assert false
      | Some x -> x in
    selected::acc)



let sum_demands (d:demands) : float =
  SrcDstMap.fold d ~init:0.
    ~f:(fun ~key:_ ~data:x acc -> acc +. x)

(* Simulate routing for one TM *)
let simulate_tm (start_scheme:scheme) (topo:topology) (dem:demands) (fail_edges:failure) (predict:demands) (algorithm:solver_type) (is_flash:bool) (flash_ba:float) (flash_sink) =
  (*
   * At each time-step:
     * For each path:
       * add traffic at source link
     * For each edge:
       * process incoming traffic based on fair share
       * add traffic to corresponding next hop links
       * or deliver to end host if last link in a path
  * *)
  let local_debug = false in
  let num_iterations = 100 in
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in

  let steady_state_time = 0 in
  let wait_out_time = 50 in (* ideally, should be >= diameter of graph*)
  let failure_time = if (EdgeSet.is_empty fail_edges) || (!Kulfi_Globals.failure_time > num_iterations)  then Int.max_value/100
                     else !Kulfi_Globals.failure_time + steady_state_time in
  let local_recovery_delay = !Kulfi_Globals.local_recovery_delay in
  let global_recovery_delay = !Kulfi_Globals.global_recovery_delay in
  let agg_dem = ref 0. in
  let recovery_churn = ref 0. in
  let solver_time = ref 0. in
  (*flash*)
  let flash_step_time = 20 in
  let flash_pred_delay = 10 in

  let iterations = range 0 (num_iterations + steady_state_time + wait_out_time) in
  if local_debug then Printf.printf "%s\n%!" (dump_scheme topo start_scheme);

  (* flash *)
  agg_dem := sum_demands dem;
  let per_src_flash_factor = flash_ba *. !agg_dem /. (total_flow_to_sink flash_sink topo dem) in
  if is_flash then Printf.printf "\t\t\t\t\tSink: %s\r" (string_of_vertex topo flash_sink);
  let _,delivered_map,lat_tput_map_map,link_utils,final_scheme,_,fail_drop,cong_drop,_,_=
  List.fold_left iterations
    (* ingress link traffic, sd-delivered, sd-latency-tput-map-map, utilization on each edge, scheme, failure, fail_drop, cong_drop *)
    ~init:(EdgeMap.empty, SrcDstMap.empty, SrcDstMap.empty, EdgeMap.empty, start_scheme, EdgeSet.empty, 0.0, 0.0, dem, predict)
    ~f:(fun current_state iter ->
      (* begin iteration - time *)
      Printf.printf "\t\t\t [Time : %3d]\r%!" (iter - steady_state_time);
      let (in_traffic, curr_delivered_map, curr_lat_tput_map_map, curr_link_utils, curr_scheme, curr_failed_links, curr_fail_drop, curr_cong_drop, actual_t, predict_t) = current_state in

      (* Reset stats when steady state is reached *)
      let curr_delivered_map, curr_lat_tput_map_map, curr_link_utils, curr_fail_drop, curr_cong_drop =
        if iter = steady_state_time then
          begin
            agg_dem := 0.;
            (SrcDstMap.empty, SrcDstMap.empty, EdgeMap.empty, 0.0, 0.0)
          end
        else
          (curr_delivered_map, curr_lat_tput_map_map, curr_link_utils, curr_fail_drop, curr_cong_drop) in

      (* introduce failures *)
      let failed_links = if iter = failure_time then
        begin
          ignore(Printf.printf "\t\t\t\t\tFail %s\r" (dump_edges topo (EdgeSet.elements fail_edges)););
          fail_edges
        end
        else curr_failed_links in

      (* update tms and scheme for flash *)
      let actual_t, predict_t, new_scheme =
        if is_flash && (iter >= steady_state_time) && (iter < steady_state_time + num_iterations) then
          begin
          let flash_t = iter - steady_state_time in
            if flash_t % flash_step_time = 0 then
              begin (* update actual & pred demand  and scheme *)
                let act' = update_flash_demand topo flash_sink dem flash_t per_src_flash_factor (Float.of_int num_iterations) in
                let pred' = update_flash_demand topo flash_sink dem (flash_t-flash_pred_delay) per_src_flash_factor (Float.of_int num_iterations) in
                let sch' = match algorithm with
                  | OptimalMcf ->
                    begin
                      let sch,rec_solve_time = global_recovery failed_links pred' act' algorithm topo in
                      recovery_churn := !recovery_churn +. (get_churn curr_scheme sch);
                      solver_time := !solver_time +. rec_solve_time;
                      sch
                    end
                  | _ -> (select_local_recovery algorithm) curr_scheme topo failed_links pred' in
                act',pred',sch'
                end
            else
              actual_t, predict_t, curr_scheme
          end
        else
        actual_t, predict_t, curr_scheme in

      (*debug*)
      (*VertexSet.iter (get_hosts_set topo)
        ~f:(fun src ->
          if not (src = flash_sink) then
          let act_d = SrcDstMap.find_exn dem (src,flash_sink) in
          let f_act_d = SrcDstMap.find_exn actual_t (src,flash_sink) in
          let p_act_d = SrcDstMap.find_exn predict_t (src,flash_sink) in
          Printf.printf "%f\t%f\n" (f_act_d /. act_d) (p_act_d /. act_d) ;);*)

      (* failures: local and global recovery *)
      let new_scheme = 
        if iter < steady_state_time + num_iterations then
        match algorithm with
          | OptimalMcf ->
              if iter = failure_time then 
                begin
                  let sch,rec_solve_time = global_recovery failed_links predict_t actual_t algorithm topo in
                  recovery_churn := !recovery_churn +. (get_churn new_scheme sch);
                  solver_time := !solver_time +. rec_solve_time;
                  sch
                end
              else new_scheme
          | _ -> 
              if iter = (local_recovery_delay + failure_time) then ((select_local_recovery algorithm) new_scheme topo failed_links predict_t)
              else if iter = (global_recovery_delay + failure_time) then
                begin
                  let sch,rec_solve_time = global_recovery failed_links predict_t actual_t algorithm topo in
                  recovery_churn := !recovery_churn +. (get_churn new_scheme sch);
                  solver_time := !solver_time +. rec_solve_time;
                  sch
                end 
              else new_scheme
        else SrcDstMap.empty in

      let actual_t = if iter < (steady_state_time + num_iterations) then actual_t else SrcDstMap.empty in


  (* Add traffic at source of every path *)
      if iter < (num_iterations + steady_state_time) then agg_dem := !agg_dem +. (sum_demands actual_t);
      (* probability of taking each path *)
      let path_prob_arr = get_path_prob_demand_arr new_scheme actual_t in

      (* next_iter_traffic : map edge -> in_traffic in next iter *)
      let next_iter_traffic = List.fold_left path_prob_arr
          ~init:EdgeMap.empty
          ~f:(fun acc (path_arr, dist, (prob,sd_demand)) ->
              if Array.length path_arr = 0 then acc else
              let first_link = path_arr.(0) in 
              let sched_traf_first_link = match EdgeMap.find acc first_link with
                                          | None -> []
                                          | Some v -> v in
              let traf_first_link = (path_arr, 1, (prob *. sd_demand))::sched_traf_first_link in
              EdgeMap.add ~key:first_link ~data:traf_first_link acc) in

      (* if no (s-d) path, then entire demand is dropped due to failure *)
      (* if no (s-d) key, then entire demand is dropped due to failure *)
      let curr_fail_drop = SrcDstMap.fold actual_t
      ~init:curr_fail_drop
      ~f:(fun ~key:(src,dst) ~data:d acc ->
        match SrcDstMap.find new_scheme (src,dst) with
        | None -> acc +. d
        | Some x -> if PathMap.is_empty x then acc +. d else acc) in
  (* Done generating traffic at source *)

      (* For each link, forward fair share of flows to next links or deliver to destination *)
      let next_iter_traffic, new_delivered_map, new_lat_tput_map_map, new_link_utils, new_fail_drop, new_cong_drop =
        EdgeMap.fold in_traffic
        ~init:(next_iter_traffic, curr_delivered_map, curr_lat_tput_map_map, curr_link_utils, curr_fail_drop, curr_cong_drop)
        ~f:(fun ~key:e ~data:in_queue_edge link_iter_acc ->
          (* total ingress traffic on link *)
          let demand_on_link = List.fold_left in_queue_edge ~init:0.0
            ~f:(fun link_dem (_,_,flow_dem) -> 
              if is_nan flow_dem then Printf.printf "flow_dem is nan!!\n"; link_dem +. flow_dem) in

          if local_debug then Printf.printf "%s: %f / %f\n%!" (string_of_edge topo e) (demand_on_link /. 1e9) ((curr_capacity_of_edge topo e failed_links) /. 1e9);

          (* calculate each flow's fair share *)
          let fs_in_queue_edge =
            if demand_on_link <= (curr_capacity_of_edge topo e failed_links) then in_queue_edge
            else fair_share_at_edge_arr (curr_capacity_of_edge topo e failed_links) in_queue_edge in

          (* Update traffic dropped due to failure or congestion *)
          let forwarded_by_link = List.fold_left fs_in_queue_edge
            ~init:0.0
            ~f:(fun fwd_acc (_,_,flow_dem) -> fwd_acc +. flow_dem) in
          let dropped = demand_on_link -. forwarded_by_link in
          let (_,_,_,_,curr_fail_drop,curr_cong_drop) = link_iter_acc in
          let new_fail_drop,new_cong_drop = 
            if EdgeSet.mem failed_links e then curr_fail_drop +. dropped, curr_cong_drop
            else curr_fail_drop, curr_cong_drop +. dropped in
          if (is_nan new_cong_drop) && (is_nan dropped) then Printf.printf "dem = %f\tfwd = %f\n"
          demand_on_link forwarded_by_link;
          (* Forward/deliver traffic on this edge *)
          List.fold_left fs_in_queue_edge
          ~init:link_iter_acc
          ~f:(fun acc (path,dist,flow_fair_share) ->
            let (nit,dlvd_map,ltm_map,lutil_map,_,_) = acc in
            let next_link_opt = next_hop_arr path dist in
            match next_link_opt with
            | None -> (* End of path, deliver traffic to dst *)
                let (src,dst) = match get_src_dst_for_path_arr path with
                                | None -> failwith "Empty path"
                                | Some x -> x in
                (* Update delivered traffic for (src,dst) *)
                let prev_sd_dlvd = match SrcDstMap.find dlvd_map (src,dst) with
                                      | None -> 0.0
                                      | Some x -> x in
                let new_dlvd = SrcDstMap.add ~key:(src,dst) ~data:(prev_sd_dlvd +. flow_fair_share) dlvd_map in

                (* Update latency-tput distribution for (src,dst) *)
                let prev_sd_ltm = match SrcDstMap.find ltm_map (src,dst) with
                                      | None -> LatencyMap.empty
                                      | Some x -> x in
                let path_latency = get_path_weight_arr topo path in
                let prev_sd_tput_for_latency =  match LatencyMap.find prev_sd_ltm path_latency with
                                                | None -> 0.0
                                                | Some x -> x in
                let new_sd_ltm = LatencyMap.add ~key:path_latency ~data:(prev_sd_tput_for_latency +. flow_fair_share) prev_sd_ltm in
                let new_ltm_map = SrcDstMap.add ~key:(src,dst) ~data:new_sd_ltm ltm_map in
                
                (* Update link utilization for edge *)
                let prev_lutil_link = match EdgeMap.find lutil_map e with
                              | None -> 0.0
                              | Some x -> x in
                let new_lutil_map = EdgeMap.add ~key:e ~data:(prev_lutil_link +. flow_fair_share) lutil_map in
                (nit, new_dlvd, new_ltm_map, new_lutil_map, new_fail_drop, new_cong_drop)
            | Some next_link -> (* Forward traffic to next hop *)
                (* Update link ingress queue for next hop *)
                let sched_traf_next_link = match EdgeMap.find nit next_link with
                    | None -> []
                    | Some v -> v in
                if is_nan flow_fair_share then assert false;
                let traf_next_link = (path,dist+1,flow_fair_share)::sched_traf_next_link in
                let new_nit = EdgeMap.add ~key:next_link ~data:traf_next_link nit in

                (* Update link utilization for edge *)
                let prev_lutil_link = match EdgeMap.find lutil_map e with
                              | None -> 0.0
                              | Some x -> x in
                let new_lutil_map = EdgeMap.add ~key:e ~data:(prev_lutil_link +. flow_fair_share) lutil_map in
                (new_nit, dlvd_map, ltm_map, new_lutil_map, new_fail_drop, new_cong_drop))) in
      (* Done forwarding for each link*)

      (* Print state for debugging *)
      if local_debug then
          EdgeMap.iter next_iter_traffic
            ~f:(fun ~key:e ~data:paths_demand ->
              Printf.printf "%s\n%!" (string_of_edge topo e);
              List.iter paths_demand
              ~f:(fun (path,_,d) -> Printf.printf "%s\t%f\n%!" (dump_edges topo (Array.to_list path)) d));
      if local_debug then SrcDstMap.iter new_delivered_map
            ~f:(fun ~key:(src,dst) ~data:delvd ->
              Printf.printf "%s %s\t%f\n%!" (Node.name (Net.Topology.vertex_to_label topo src))
            (Node.name (Net.Topology.vertex_to_label topo dst)) delvd);
      (* State carried over to next iter *)
      let next_state = (next_iter_traffic, new_delivered_map, new_lat_tput_map_map, new_link_utils, new_scheme, failed_links, new_fail_drop, new_cong_drop, actual_t, predict_t) in
      next_state)
      (* end iteration *) in

  agg_dem := !agg_dem /. (Float.of_int num_iterations);
  (* Generate stats *)
  let tput = SrcDstMap.fold delivered_map
    ~init:SrcDstMap.empty
    ~f:(fun ~key:sd ~data:dlvd acc ->
      SrcDstMap.add ~key:sd ~data:(dlvd /. (Float.of_int num_iterations) /. !agg_dem) acc) in

  let latency_dist = (get_aggregate_latency lat_tput_map_map num_iterations) in

  let avg_congestions = EdgeMap.fold link_utils
    ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:util acc ->
      EdgeMap.add ~key:e ~data:(util /. (Float.of_int (num_iterations)) /. (capacity_of_edge topo e)) acc) in
  let fail_drop = fail_drop /. (Float.of_int num_iterations) /. !agg_dem in
  let cong_drop = cong_drop /. (Float.of_int num_iterations) /. !agg_dem in

  (*
  let total_tput = get_total_tput tput in
  Printf.printf "\nTotal traffic: %f + %f + %f\t= %f" total_tput fail_drop cong_drop (total_tput+.fail_drop+.cong_drop);*)
  tput, latency_dist, avg_congestions, fail_drop, cong_drop, !agg_dem, !recovery_churn, final_scheme, !solver_time
  (* end simulate_tm *)

let is_int v =
  let p = (Float.modf v) in
  let f = Float.Parts.fractional p in
  let c = Float.classify f in
  c = Float.Class.Zero

(* assumes l is sorted *)
let kth_percentile (l:float list) (k:float) : float =
  let n = List.length l in
  let x = (Float.of_int n) *. k in
  (*Printf.printf "%f / %d\n%!" x n;*)
  if is_int x then
    let i = Int.of_float (Float.round_up x) in
    let lhs = match (List.nth l i) with
      | Some f -> f
      | None -> assert false in
    let rhs = match List.nth l (min (i+1) (n-1)) with
      | Some f -> f
      | None -> assert false in
    ((lhs +. rhs)/.2.)
  else
    let i = Int.of_float x in
    match (List.nth l i) with
    | Some f -> f
    | None -> assert false

let get_mean_congestion (l:float list) =
  (List.fold_left ~init:0. ~f:( +. )  l) /. (Float.of_int (List.length l))

(*  assume that flow is fractionally split in the proportions indicated by the probabilities. *)
let get_max_congestion (congestions:float list) : float =
  List.fold_left ~init:Float.nan ~f:(fun a acc -> Float.max_inan a acc) congestions

let get_num_paths (s:scheme) : float =
  let count = SrcDstMap.fold
		~init:0
		~f:(fun ~key:_ ~data:d acc ->
		    acc + (PathMap.length d))
		s in
  Float.of_int count

  (* Generate latency percentile based on throughput *)
let get_latency_percentiles (lat_tput_map : throughput LatencyMap.t) (agg_dem:float) : (float LatencyMap.t) =
(*  let total_tput = LatencyMap.fold lat_tput_map
    ~init:0.0
    ~f:(fun ~key:_ ~data:tput acc -> tput +. acc) in*)
  let latency_percentiles,_ = LatencyMap.fold lat_tput_map
    ~init:(LatencyMap.empty,0.0)
    ~f:(fun ~key:latency ~data:tput acc ->
      let lat_percentile_map,sum_tput = acc in
      let sum_tput' = sum_tput +. tput in
      (LatencyMap.add ~key:latency ~data:(sum_tput' /. agg_dem)
      lat_percentile_map, sum_tput')) in
  latency_percentiles

let set_weight topo edge w =
    let label = Topology.edge_to_label topo edge in
    Link.set_weight label w;
    topo

let reset_topo_weights edge_weights topo =
  EdgeSet.fold (Topology.edges topo)
    ~init:topo
    ~f:(fun acc e ->
      let w = StringMap.find_exn edge_weights (string_of_edge topo e) in
      set_weight acc e w) 

let set_topo_weights topo =
  EdgeSet.fold (Topology.edges topo) ~init:StringMap.empty
    ~f:(fun acc e -> 
      let w = (0.5 +. Random.float 1.) in
      let topo = set_weight topo e w in
      StringMap.add ~key:(string_of_edge topo e) ~data:w acc)



(* Calculate a demand matrix equal to max (envelope) of all TMs *) 
let calculate_demand_envelope (topo:topology) (predict_file:string) (host_file:string) (iters:int) =
  let num_tm = min iters (List.length (In_channel.read_lines predict_file)) in
  let (predict_host_map, predict_ic) = open_demands predict_file host_file topo in
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let iterations = range 0 num_tm in
  let envelope = List.fold_left iterations ~init:SrcDstMap.empty
    ~f:(fun acc n ->
      let predict = next_demand ~scale:1.0 predict_ic predict_host_map in
      SrcDstMap.fold predict ~init:acc
      ~f:(fun ~key:(s,d) ~data:pred acc ->
        let env_sd = match SrcDstMap.find acc (s,d) with
        | None -> 0.
        | Some x -> x in
        SrcDstMap.add ~key:(s,d) ~data:(max env_sd pred) acc)) in
  close_demands predict_ic;
  envelope

let simulate
    (spec_solvers:solver_type list)
	     (topology_file:string)
	     (demand_file:string)
	     (predict_file:string)
	     (host_file:string)
	     (iterations:int)
       (scale:float)
       (number_failures:int)
       (is_flash:bool)
       (flash_burst_amount:float)
       (out_dir:string option)
             () : unit =

  (* Do some error checking on input *)

  let demand_lines_length = List.length (In_channel.read_lines demand_file) in
  let predict_lines_length = List.length (In_channel.read_lines predict_file) in

  ignore (if (demand_lines_length < iterations) then failwith "Iterations greater than demand file length" else());
  ignore (if (predict_lines_length < iterations) then failwith "Iterations greater than predict file length" else());

  let topo = Parse.from_dotfile topology_file in
  let edge_weights = set_topo_weights topo in
  let host_set = VertexSet.filter (Topology.vertexes topo)
				  ~f:(fun v ->
				      let label = Topology.vertex_to_label topo v in
				      Node.device label = Node.Host) in

  Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
  Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);


  let time_data = make_data "Iteratives Vs Time" in
  let tm_churn_data = make_data "TM Churn Vs Time" in
  let rec_churn_data = make_data "Recovery Churn Vs Time" in
  let edge_congestion_data = make_data "Edge Congestion Vs Time" in
  let latency_percentiles_data = make_data "Latency distribution vs Time" in
  let edge_exp_congestion_data = make_data "Edge Expected Congestion Vs Time" in
  let num_paths_data = make_data "Num. Paths Vs Time" in
  let max_congestion_data = make_data "Max Congestion Vs Time" in
  let mean_congestion_data = make_data "Mean Congestion Vs Time" in
  let max_exp_congestion_data = make_data "Max Expected Congestion Vs Time" in
  let mean_exp_congestion_data = make_data "Mean Expected Congestion Vs Time" in
  let total_tput_data = make_data "Total Throughput vs Time" in
  let failure_drop_data = make_data "Drop due to failure vs Time" in
  let congestion_drop_data = make_data "Drop due to congestion vs Time" in
  let percentiles = [0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 0.95] in
  let create_percentile_data (metric:string) =
    List.fold_left
      percentiles
      ~init:[]
      ~f:(fun acc p ->
	  let i = Int.of_float (p *. 100.) in
	  let s = Printf.sprintf "%d-th %s Vs Time" i metric in
    acc@[(make_data s)] ) in

  let percentile_data = create_percentile_data "Congestion" in
  let exp_percentile_data = create_percentile_data "Expected Congestion" in

  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let is = range 0 iterations in

  let failure_scenarios = get_failure_scenarios topo demand_file host_file iterations number_failures scale in
  let flash_sinks = pick_flash_sinks topo iterations in
  List.iter
    spec_solvers
    ~f:(fun algorithm ->

	(* TODO(rjs): Raeke mutates the topology. As a fast fix, I'll just create
	 a new copy of topology for every algorithm. A better fix would be to understand
	 Raeke and ensure that it mutates a copy of the topology, not the actual
	 topology
   (praveenk): Raeke changes edge weights. Let's just reset them. *)
  ignore(reset_topo_weights edge_weights topo;);

  let _ = match algorithm with
    | SemiMcfMcfFTEnv
    | SemiMcfMcfEnv -> demand_envelope := (calculate_demand_envelope topo predict_file host_file iterations);
    | _ -> () in

	let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
	let (predict_host_map, predict_ic) = open_demands predict_file host_file topo in
  Printf.printf "\n";
	(* we may need to initialize the scheme, and advance both traffic files *)

	ignore (
	    List.fold_left
	      is (* 0..iterations *)
	      ~init:SrcDstMap.empty
	      ~f:(fun scheme n ->

      Printf.printf "\nAlgo: %s TM: %d\r%!" (solver_to_string algorithm) n;
		  (* get the next demand *)
		  let actual = next_demand ~scale:scale actual_ic actual_host_map in
		  let predict = next_demand ~scale:scale predict_ic predict_host_map in

      (* initialize algorithm *)
      if n = 0 then initialize_scheme algorithm topo predict;

		  (* solve *)
		  let scheme',solver_time = solve_within_budget algorithm topo predict actual in
      ignore(reset_topo_weights edge_weights topo;);
  	  let exp_congestions = (congestion_of_paths scheme' topo actual) in
		  let list_of_exp_congestions = List.map ~f:snd (EdgeMap.to_alist exp_congestions) in
		  let sorted_exp_congestions = List.sort ~cmp:(Float.compare) list_of_exp_congestions in
      let failing_edges =  match List.nth failure_scenarios n with 
          | Some x -> x
          | None -> assert false in
	
      let flash_sink = match List.nth flash_sinks n with
        | Some x -> x
        | None -> assert false in
		  let tput,latency_dist,congestions,failure_drop,congestion_drop,agg_dem,recovery_churn,final_scheme, rec_solver_time =
        (simulate_tm scheme' topo actual failing_edges predict algorithm is_flash flash_burst_amount flash_sink) in
		  let list_of_congestions = List.map ~f:snd (EdgeMap.to_alist congestions) in
		  let sorted_congestions = List.sort ~cmp:(Float.compare) list_of_congestions in
		  (* record *)
		  let tm = solver_time +. rec_solver_time in
      let tm_churn = (get_churn scheme scheme') in
		  let np = (get_num_paths scheme') in
		  let cmax = (get_max_congestion list_of_congestions) in
		  let cmean = (get_mean_congestion list_of_congestions) in
		  let expcmax = (get_max_congestion list_of_exp_congestions) in
		  let expcmean = (get_mean_congestion list_of_exp_congestions) in
      let total_tput = (get_total_tput tput) in
      let latency_percentiles = (get_latency_percentiles latency_dist agg_dem) in

		  let percentile_values sort_cong =
		    List.fold_left
		      percentiles
		      ~init:[]
		      ~f:(fun acc p ->
            acc@[(kth_percentile sort_cong p)]) in

		  let sname = (solver_to_string algorithm) in
		  add_record time_data sname {iteration = n; time=tm; time_dev=0.0; };
		  add_record tm_churn_data sname {iteration = n; churn=tm_churn; churn_dev=0.0; };
		  add_record rec_churn_data sname {iteration = n; churn=recovery_churn; churn_dev=0.0; };
		  add_record num_paths_data sname {iteration = n; num_paths=np; num_paths_dev=0.0; };
		  add_record max_congestion_data sname {iteration = n; congestion=cmax; congestion_dev=0.0; };
		  add_record mean_congestion_data sname {iteration = n; congestion=cmean; congestion_dev=0.0; };
		  add_record max_exp_congestion_data sname {iteration = n; congestion=expcmax; congestion_dev=0.0; };
		  add_record mean_exp_congestion_data sname {iteration = n; congestion=expcmean; congestion_dev=0.0; };
		  add_record edge_congestion_data sname {iteration = n; edge_congestions=congestions; };
		  add_record edge_exp_congestion_data sname {iteration = n; edge_congestions=exp_congestions; };
		  add_record latency_percentiles_data sname {iteration = n; latency_percentiles=latency_percentiles; };
		  add_record total_tput_data sname {iteration = n; throughput=total_tput; throughput_dev=0.0; };
		  add_record failure_drop_data sname {iteration = n; throughput=failure_drop; throughput_dev=0.0; };
		  add_record congestion_drop_data sname {iteration = n; throughput=congestion_drop; throughput_dev=0.0; };
		  List.iter2_exn
		    percentile_data
		    (percentile_values sorted_congestions)
        ~f:(fun d v -> add_record d sname {iteration = n; congestion=v; congestion_dev=0.0;});
		  List.iter2_exn
		    exp_percentile_data
		    (percentile_values sorted_exp_congestions)
		    ~f:(fun d v -> add_record d sname {iteration = n; congestion=v; congestion_dev=0.0;});

		  scheme') );

	(* start at beginning of demands for next algorithm *)
	close_demands actual_ic;
	close_demands predict_ic;
       );

  (* Store results in a directory name = topology name or provided name in expData *)
  let output_dir = match out_dir with
      | Some x -> x
      | None ->
        let split_dot_file_list = String.split_on_chars topology_file ~on:['/';'.'] in
        let suffix =List.nth split_dot_file_list (List.length split_dot_file_list -2) in
        match suffix with 
          | Some x -> x
          | _ -> "default" in
  let dir = "./expData/" ^ output_dir ^ "/" in
  to_file dir "TMChurnVsIterations.dat" tm_churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string;
  to_file dir "RecoveryChurnVsIterations.dat" rec_churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string;
  to_file dir "NumPathsVsIterations.dat" num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string;
  to_file dir "TimeVsIterations.dat" time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string;
  to_file dir "MaxCongestionVsIterations.dat" max_congestion_data "# solver\titer\tmax-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "MeanCongestionVsIterations.dat" mean_congestion_data "# solver\titer\tmean-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "MaxExpCongestionVsIterations.dat" max_exp_congestion_data "# solver\titer\tmax-exp-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "MeanExpCongestionVsIterations.dat" mean_exp_congestion_data "# solver\titer\tmean-exp-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "TotalThroughputVsIterations.dat" total_tput_data "# solver\titer\ttotal-throughput\tstddev" iter_vs_throughput_to_string;
  to_file dir "FailureLossVsIterations.dat" failure_drop_data "# solver\titer\tfailure-drop\tstddev" iter_vs_throughput_to_string;
  to_file dir "CongestionLossVsIterations.dat" congestion_drop_data "# solver\titer\tcongestion-drop\tstddev" iter_vs_throughput_to_string;

  List.iter2_exn
    percentile_data
    percentiles
    ~f:(fun d p ->
	let s1 = Printf.sprintf "k%dCongestionVsIterations.dat" (Int.of_float (p *. 100.)) in
	let s2 = Printf.sprintf "# solver\titer\t.%f-congestion\tstddev" p in
	to_file dir s1 d s2  iter_vs_congestion_to_string) ;

  List.iter2_exn
    exp_percentile_data
    percentiles
    ~f:(fun d p ->
	let s1 = Printf.sprintf "k%dExpCongestionVsIterations.dat" (Int.of_float (p *. 100.)) in
	let s2 = Printf.sprintf "# solver\titer\t.%f-exp-congestion\tstddev" p in
	to_file dir s1 d s2  iter_vs_congestion_to_string) ;

  to_file dir "EdgeCongestionVsIterations.dat" edge_congestion_data "# solver\titer\tedge-congestion" (iter_vs_edge_congestions_to_string topo);
  to_file dir "EdgeExpCongestionVsIterations.dat" edge_exp_congestion_data "# solver\titer\tedge-exp-congestion" (iter_vs_edge_congestions_to_string topo);

  to_file dir "LatencyDistributionVsIterations.dat" latency_percentiles_data "#solver\titer\tlatency-throughput" (iter_vs_latency_percentiles_to_string);

  (*Printf.printf "%s" (to_string time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string);
  Printf.printf "%s" (to_string tm_churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string);
  Printf.printf "%s" (to_string rec_churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string);
  Printf.printf "%s" (to_string max_congestion_data "# solver\titer\tmax-congestion\tstddev" iter_vs_congestion_to_string);
  Printf.printf "%s" (to_string total_tput_data "# solver\titer\ttotal-throughput\tstddev" iter_vs_throughput_to_string);
  Printf.printf "%s" (to_string failure_drop_data "# solver\titer\tfailure-drop\tstddev" iter_vs_throughput_to_string);
  Printf.printf "%s" (to_string congestion_drop_data "# solver\titer\tcongestion-drop\tstddev" iter_vs_throughput_to_string);
  Printf.printf "%s" (to_string num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string)*)
  Printf.printf "\nComplete.\n"

(* For synthetic demands based on Abilene, scale them to current topology by multiplying by X/mcf_congestion,
   where X is the max congestion we expect to get when run with the new demands *)
let calculate_syn_scale (topology:string) (demand_file:string) (host_file:string) =
  let topo = Parse.from_dotfile topology in
  let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
  let actual = next_demand ~scale:1.0 actual_ic actual_host_map in
  let s = Kulfi_Mcf.solve topo actual in
  let expected_congestions = congestion_of_paths s topo actual in
  let list_of_congestions = List.map ~f:snd (EdgeMap.to_alist expected_congestions) in
  let cmax = (get_max_congestion list_of_congestions) in
  let scale_factor = 0.4/.cmax in
  close_demands actual_ic;
  scale_factor


let command =
  Command.basic
    ~summary:"Simulate run of routing strategies"
    Command.Spec.(
    empty
    +> flag "-mcf" no_arg ~doc:" run mcf"
    +> flag "-mwmcf" no_arg ~doc:" run mwmcf"
    +> flag "-vlb" no_arg ~doc:" run vlb"
    +> flag "-ecmp" no_arg ~doc:" run ecmp"
    +> flag "-ksp" no_arg ~doc:" run ksp"
    +> flag "-spf" no_arg ~doc:" run spf"
    +> flag "-akmcf" no_arg ~doc:" run ak+mcf"
    +> flag "-akvlb" no_arg ~doc:" run ak+vlb"
    +> flag "-akraeke" no_arg ~doc:" run ak+raeke"
    +> flag "-akecmp" no_arg ~doc:" run ak+ecmp"
    +> flag "-akksp" no_arg ~doc:" run ak+ksp"
    +> flag "-semimcfmcf" no_arg ~doc:" run semi mcf+mcf"
    +> flag "-semimcfmcfenv" no_arg ~doc:" run semi mcf+mcf init with envelope"
    +> flag "-semimcfmcfftenv" no_arg ~doc:" run semi mcf+mcf init with envelope considering all fail scenarios"
    +> flag "-semimcfvlb" no_arg ~doc:" run semi mcf+vlb"
    +> flag "-semimcfraeke" no_arg ~doc:" run semi mcf+raeke"
    +> flag "-semimcfraekeft" no_arg ~doc:" run semi mcf+raeke considering all failure scenario"
    +> flag "-semimcfecmp" no_arg ~doc:" run semi mcf+ecmp"
    +> flag "-semimcfksp" no_arg ~doc:" run semi mcf+ksp"
    +> flag "-semimcfkspft" no_arg ~doc:" run semi mcf+ksp considering all failure scenario"
    +> flag "-raeke" no_arg ~doc:" run raeke"
    +> flag "-optimalmcf" no_arg ~doc:" run optimal mcf"
    +> flag "-all" no_arg ~doc:" run all schemes"
    +> flag "-scalesyn" no_arg ~doc:" scale synthetic demands to achieve max congestion 1"
    +> flag "-deloop" no_arg ~doc:" remove loops in paths"
    +> flag "-scale" (optional float) ~doc:" scale demands by this factor"
    +> flag "-budget" (optional int) ~doc:" max paths between each pair of hosts"
    +> flag "-fail-time" (optional int) ~doc:" simulation time to introduce failure at"
    +> flag "-lr-delay" (optional int) ~doc:" delay between failure and local recovery"
    +> flag "-gr-delay" (optional int) ~doc:" delay between failure and global recovery"
    +> flag "-flash-det-delay" (optional int) ~doc:" delay between flash and detection"
    +> flag "-is-flash" no_arg ~doc:" simulate flash or not"
    +> flag "-flash-ba" (optional float) ~doc:" fraction of total traffic to add as flash"
    +> flag "-fail-num" (optional int) ~doc:" number of links to fail"
    +> flag "-out" (optional string) ~doc:" name of directory in expData to store results"
    +> anon ("topology-file" %: string)
    +> anon ("demand-file" %: string)
    +> anon ("predict-file" %: string)
    +> anon ("host-file" %: string)
    +> anon ("iterations" %: int)
  ) (fun (mcf:bool)
         (mwmcf:bool)
	 (vlb:bool)
	 (ecmp:bool)
	 (ksp:bool)
	 (spf:bool)
	 (akmcf:bool)
	 (akvlb:bool)
	 (akraeke:bool)
	 (akecmp:bool)
	 (akksp:bool)
	 (semimcfmcf:bool)
	 (semimcfmcfenv:bool)
	 (semimcfmcfftenv:bool)
	 (semimcfvlb:bool)
	 (semimcfraeke:bool)
	 (semimcfraekeft:bool)
	 (semimcfecmp:bool)
	 (semimcfksp:bool)
	 (semimcfkspft:bool)
	 (raeke:bool)
	 (optimalmcf:bool)
	 (all:bool)
   (scalesyn:bool)
   (deloop:bool)
   (scale:float option)
   (budget:int option)
   (fail_time:int option)
   (lr_delay:int option)
   (gr_delay:int option)
   (flash_det_delay:int option)
   (is_flash:bool)
   (flash_ba:float option)
   (fail_num:int option)
   (out:string option)
	 (topology_file:string)
	 (demand_file:string)
	 (predict_file:string)
	 (host_file:string)
	 (iterations:int)
   () ->
     let algorithms =
       List.filter_map
         ~f:(fun x -> x)
         [ if mcf || all then Some Mcf else None
         ; if optimalmcf || all then Some OptimalMcf else None
         ; if mwmcf then Some MwMcf else None
         ; if vlb || all then Some Vlb else None
         ; if ecmp || all then Some Ecmp else None
         ; if ksp || all then Some Ksp else None
         ; if spf || all then Some Spf else None
	 ; if akmcf then Some AkMcf else None
	 ; if akvlb then Some AkVlb else None
	 ; if akecmp then Some AkEcmp else None
	 ; if akksp then Some AkKsp else None
	 ; if akraeke then Some AkRaeke else None
   ; if raeke || all then Some Raeke else None
   ; if semimcfmcf then Some SemiMcfMcf else None
   ; if semimcfmcfenv || all then Some SemiMcfMcfEnv else None
   ; if semimcfmcfftenv || all then Some SemiMcfMcfFTEnv else None
	 ; if semimcfecmp || all then Some SemiMcfEcmp else None
	 ; if semimcfksp || all then Some SemiMcfKsp else None
	 ; if semimcfkspft then Some SemiMcfKspFT else None
	 ; if semimcfvlb || all then Some SemiMcfVlb else None
	 ; if semimcfraeke || all then Some SemiMcfRaeke else None
	 ; if semimcfraekeft then Some SemiMcfRaekeFT else None ] in
     let syn_scale = if scalesyn then calculate_syn_scale topology_file demand_file host_file else 1.0 in
     let tot_scale = match scale with | None -> syn_scale | Some x -> x *. syn_scale in
     let fail_num = match fail_num with | Some x -> x | None -> 1 in
     let flash_ba = match flash_ba with | Some x -> x | None -> 0. in
     Printf.printf "Scale factor: %f\n\n" (tot_scale);
     Kulfi_Globals.deloop := deloop;
     ignore(Kulfi_Globals.budget := match budget with | None -> Int.max_value/100 | Some x -> x);
     ignore(Kulfi_Globals.failure_time := match fail_time with | None -> Int.max_value/100 | Some x -> x);
     ignore(Kulfi_Globals.local_recovery_delay := match lr_delay with | None -> Int.max_value/100 | Some x -> x);
     ignore(Kulfi_Globals.global_recovery_delay := match gr_delay with | None -> Int.max_value/100 | Some x -> x);
     ignore(Kulfi_Globals.flash_detection_delay := match flash_det_delay with | None -> Int.max_value/100 | Some x -> x);
     simulate algorithms topology_file demand_file predict_file host_file iterations tot_scale fail_num is_flash flash_ba out ()
  )

let main = Command.run command

let _ = main

