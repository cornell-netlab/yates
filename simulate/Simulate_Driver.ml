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
open Simulate_Failure
open Simulate_Switch
open Simulation_Types
open Simulation_Util

let demand_envelope = ref SrcDstMap.empty

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

(* compute routing schemes for each link failure and merge the schemes *)
let all_failures_envelope solver (topo:topology) (envelope:demands) : scheme =
  let hosts = get_hosts topo in
  let failure_scheme_map,_ = EdgeSet.fold (Topology.edges topo)
    ~init:(EdgeMap.empty, EdgeSet.empty)
    ~f:(fun (acc, handled_edges) e ->
      if (EdgeSet.mem handled_edges e) then (acc, handled_edges)
      else
        let e' = match Topology.inverse_edge topo e with
          | None ->
              Printf.printf "%s\n%!" (string_of_edge topo e);
              failwith "No reverse edge found"
          | Some x -> x in
        let handled_edges = EdgeSet.add (EdgeSet.add handled_edges e) e' in
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
            (EdgeMap.add ~key:e ~data:sch acc, handled_edges)
            end
          else (acc, handled_edges)
        else (acc, handled_edges)) in
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
          if s = d then res else
          let acc_pp_map = match SrcDstMap.find res (s,d) with
            | None -> PathMap.empty
            | Some x -> x in
          if (PathMap.is_empty f_pp_map) then
            begin
              Printf.printf "sd: (%s, %s) Edge: %s: Scheme %s"
                (string_of_vertex topo s) (string_of_vertex topo d)
                (string_of_edge topo e) (dump_scheme topo edge_scheme);
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

(* Compute the initial scheme for a TE algorithm *)
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

(* Initialize a TE algorithm *)
let initialize_scheme algorithm topo predict : unit =
  Printf.printf "[Init...] \r";
  let start_scheme = initial_scheme algorithm topo predict in
  let pruned_scheme =
    if SrcDstMap.is_empty start_scheme then start_scheme
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


(* Compute a routing scheme for an algorithm and apply budget by pruning the top-k paths *)
let solve_within_budget algorithm topo predict actual: (scheme * float) =
  let at = make_auto_timer () in
  start at;
  let solve = select_algorithm algorithm in
  let budget' = match algorithm with
    | OptimalMcf ->
        Int.max_value / 100
    | _ ->
        !Kulfi_Globals.budget in
  let sch = match algorithm with
    | OptimalMcf -> (* Use actual demands for Optimal *)
        prune_scheme topo (solve topo actual) budget'
    | _ ->
        prune_scheme topo (solve topo predict) budget' in
  stop at;
  assert (probabilities_sum_to_one sch);
  (sch, (get_time_in_seconds at))

(* TODO(rjs): Do we count paths that have 0 flow ? *)
let get_churn (old_scheme:scheme) (new_scheme:scheme) : float =
  let get_path_sets (s:scheme) : PathSet.t =
    SrcDstMap.fold s
      ~init:PathSet.empty
      ~f:(fun ~key:_ ~data:d acc ->
          PathMap.fold
            ~init:acc
            ~f:(fun ~key:p ~data:_ acc ->
                PathSet.add acc p ) d) in
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
      ~f:(fun ~key:_ ~data:d acc ->
          PathMap.fold
            ~init:acc
            ~f:(fun ~key:p ~data:_ acc ->
              StringSet.add acc (dump_edges topo p)) d) s in
  let set1 = get_path_sets old_scheme in
  let set2 = get_path_sets new_scheme in
  let union = StringSet.union set1 set2 in
  let inter = StringSet.inter set1 set2 in
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
      PathMap.fold paths
        ~init:acc
        ~f:(fun ~key:path ~data:prob acc ->
          match PathMap.find acc path with
            | None ->
                PathMap.add ~key:path ~data:(prob, demand) acc
            | Some x ->
                if List.is_empty path then acc
                else failwith "Duplicate paths should not be present"))

(* Get a map from path to it's probability and src-dst demand *)
let get_path_prob_demand_arr (s:scheme) (d:demands) =
  (* (Probability, net s-d demand) for each path *)
  SrcDstMap.fold s
    ~init:[]
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
      let demand = match SrcDstMap.find d (src,dst) with
                 | None -> 0.0
                 | Some x -> x in
      PathMap.fold paths
        ~init:acc
        ~f:(fun ~key:path ~data:prob acc ->
          let arr_path = Array.of_list path in
          (arr_path, 0, (prob, demand))::acc))


(* Sum througput over all src-dst pairs *)
let get_total_tput (sd_tput:throughput SrcDstMap.t) : throughput =
  SrcDstMap.fold sd_tput
    ~init:0.0
    ~f:(fun ~key:_ ~data:dlvd acc ->
      acc +. dlvd)

(* Aggregate latency-tput over all sd-pairs *)
let get_aggregate_latency (sd_lat_tput_map_map:(throughput LatencyMap.t) SrcDstMap.t) (num_iter:int) : (throughput LatencyMap.t) =
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
    ~f:(fun acc link ->
      Topology.remove_edge acc link) in
  ignore(EdgeSet.iter failed_links
  ~f:(fun e -> assert ((EdgeSet.mem (Topology.edges topo) e) &&
      not (EdgeSet.mem (Topology.edges topo') e)) ););
  ignore(EdgeSet.iter (Topology.edges topo')
      ~f:(fun e -> assert (EdgeSet.mem (Topology.edges topo) e)));

  initialize_scheme algorithm topo' predict;
  let new_scheme,solver_time = solve_within_budget algorithm topo' predict actual in
  ignore (if (SrcDstMap.is_empty new_scheme) then failwith "new_scheme is empty in global driver" else ());
  Printf.printf "\t\t\t\t\t\t\t\t\t\t\tGLOBAL\r";
  new_scheme, solver_time


(* Model flash *)
let flash_decrease (x:float) (t:float) (total_t:float) : float =
  total_t *. x /. (t *. 2.0 +. total_t)

let flash_demand_t x t total_t d =
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
    let hosts = get_hosts_set topo in
    VertexSet.fold hosts ~init:dem
      ~f:(fun acc src ->
        if src = sink then acc
        else
          let sd_dem =
            SrcDstMap.find_exn dem (src,sink)
            |> flash_demand_t per_src_flash_factor (Float.of_int flash_t) total_t in
          SrcDstMap.add ~key:(src,sink) ~data:sd_dem acc)

(* find the destination for flash crowd *)
let pick_flash_sinks (topo:topology) (iters:int) =
  let hosts = get_hosts topo in
  let num_hosts = List.length hosts in
  List.fold_left (range 0 iters) ~init:[]
  ~f:(fun acc n ->
    let selected = List.nth_exn hosts (n % num_hosts) in
    (*let selected = match List.nth_in hosts 4 in (*NOTE TODO: selecting h5. change back ^^^^^^*)*)
    selected::acc)

let sum_demands (d:demands) : float =
  SrcDstMap.fold d ~init:0.
    ~f:(fun ~key:_ ~data:x acc -> acc +. x)

let sum_sink_demands (d:demands) sink : float =
  SrcDstMap.fold d ~init:0.
    ~f:(fun ~key:(src,dst) ~data:x acc ->
      if dst = sink then acc +. x
      else acc)

(***********************************************************)
(************** Simulate routing for one TM ****************)
let simulate_tm (start_scheme:scheme)
    (topo : topology)
    (dem : demands)
    (fail_edges : failure)
    (predict : demands)
    (algorithm : solver_type)
    (is_flash : bool)
    (flash_ba : float)
    (flash_sink) =
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
  let num_iterations = !Kulfi_Globals.tm_sim_iters in
  let steady_state_time = 0 in
  let wait_out_time = 50 in (* wait for this time for the network to reach steady state *)
  let failure_time =
    if (EdgeSet.is_empty fail_edges) || (!Kulfi_Globals.failure_time > num_iterations)  then Int.max_value/100
    else !Kulfi_Globals.failure_time + steady_state_time in
  let local_recovery_delay = !Kulfi_Globals.local_recovery_delay in
  let global_recovery_delay = !Kulfi_Globals.global_recovery_delay in
  let agg_dem = ref 0. in
  let agg_sink_dem = ref 0. in
  let recovery_churn = ref 0. in
  let solver_time = ref 0. in
  (*flash*)
  let flash_step_time = num_iterations/5 in
  let flash_pred_delay = 10 in

  let iterations = range 0 (num_iterations + steady_state_time + wait_out_time) in
  if local_debug then Printf.printf "%s\n%!" (dump_scheme topo start_scheme);

  (* flash *)
  agg_dem := sum_demands dem;
  let per_src_flash_factor = flash_ba *. !agg_dem /. (total_flow_to_sink flash_sink topo dem) /. (Float.of_int (List.length (get_hosts topo))) in
  if is_flash then Printf.printf "\t\t\t\t\tSink: %s\r" (string_of_vertex topo flash_sink);
  let final_network_state =
  List.fold_left iterations
    ~init:(make_network_iter_state ~scheme:start_scheme ~real_tm:dem ~predict_tm:predict ())
    ~f:(fun current_state iter ->
      (* begin iteration - time *)
      Printf.printf "\t\t\t [Time : %3d]\r%!" (iter - steady_state_time);

      (* Reset stats when steady state is reached *)
      let current_state =
        if iter = steady_state_time then
          begin
            agg_dem := 0.;
            agg_sink_dem := 0.;
            { current_state with
                delivered       = SrcDstMap.empty;
                latency         = SrcDstMap.empty;
                utilization     = EdgeMap.empty;
                failure_drop    = 0.0;
                congestion_drop = 0.0; }
          end
        else
          current_state in

      (* introduce failures *)
      let failed_links =
        if iter = failure_time then
          begin
            Printf.printf "\t\t\t\t\tFail %s\r" (dump_edges topo (EdgeSet.elements fail_edges));
            fail_edges
          end
        else current_state.failures in

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
                      recovery_churn := !recovery_churn +. (get_churn current_state.scheme sch);
                      solver_time := !solver_time +. rec_solve_time;
                      sch
                    end
                  | _ ->
                      if !Kulfi_Globals.flash_recover then (select_local_recovery algorithm) current_state.scheme topo failed_links pred'
                      else current_state.scheme in
                act',pred',sch'
              end
            else
              current_state.real_tm, current_state.predict_tm, current_state.scheme
          end
        else
        current_state.real_tm, current_state.predict_tm, current_state.scheme in

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

      let actual_t =
        if iter < (steady_state_time + num_iterations) then actual_t
        else SrcDstMap.empty in

      (* Add traffic at source of every path *)
      if iter < (num_iterations + steady_state_time) then agg_dem := !agg_dem +. (sum_demands actual_t);
      if iter < (num_iterations + steady_state_time) then agg_sink_dem := !agg_sink_dem +. (sum_sink_demands actual_t flash_sink);

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
      let cumul_fail_drop = SrcDstMap.fold actual_t
        ~init:current_state.failure_drop
        ~f:(fun ~key:(src,dst) ~data:d acc ->
          match SrcDstMap.find new_scheme (src,dst) with
          | None ->
              acc +. d
          | Some x ->
              if PathMap.is_empty x then acc +. d
              else acc) in
      (* Done generating traffic at source *)

      (* For each link, forward fair share of flows to next links or deliver to destination *)
      let next_iter_traffic, new_delivered_map, new_lat_tput_map_map, new_link_utils, new_fail_drop, new_cong_drop =
        EdgeMap.fold current_state.ingress_link_traffic
        ~init:(next_iter_traffic, current_state.delivered, current_state.latency, current_state.utilization, cumul_fail_drop, current_state.congestion_drop)
        ~f:(fun ~key:e ~data:in_queue_edge link_iter_acc ->
          (* total ingress traffic on link *)
          let demand_on_link = List.fold_left in_queue_edge
            ~init:0.0
            ~f:(fun link_dem (_,_,flow_dem) ->
              if is_nan flow_dem then Printf.printf "flow_dem is nan!!\n";
              link_dem +. flow_dem) in

          if local_debug then Printf.printf "%s: %f / %f\n%!"
            (string_of_edge topo e) (demand_on_link /. 1e9)
            ((curr_capacity_of_edge topo e failed_links) /. 1e9);

          (* calculate each flow's fair share *)
          let fs_in_queue_edge =
            if demand_on_link <= (curr_capacity_of_edge topo e failed_links) then in_queue_edge
            else fair_share_at_edge_arr (curr_capacity_of_edge topo e failed_links) in_queue_edge in

          (* Update traffic dropped due to failure or congestion *)
          let forwarded_by_link = List.fold_left fs_in_queue_edge
            ~init:0.0
            ~f:(fun fwd_acc (_,_,flow_dem) -> fwd_acc +. flow_dem) in
          let dropped = demand_on_link -. forwarded_by_link in
          let (_,_,_,curr_lutil_map,curr_fail_drop,curr_cong_drop) = link_iter_acc in
          let new_fail_drop,new_cong_drop =
            if EdgeSet.mem failed_links e then curr_fail_drop +. dropped, curr_cong_drop
            else curr_fail_drop, curr_cong_drop +. dropped in
          if (is_nan new_cong_drop) && (is_nan dropped) then Printf.printf "dem = %f\tfwd = %f\n" demand_on_link forwarded_by_link;
          (*if dropped > 0. then Printf.printf "%d : %f\n%!" iter dropped;*)
          (* Forward/deliver traffic on this edge *)
          let fl_init_nit,fl_init_dlvd_map,fl_init_ltm_map,_,_,_ = link_iter_acc in
          let new_nit, dlvd_map, ltm_map =
            List.fold_left fs_in_queue_edge
            ~init:(fl_init_nit, fl_init_dlvd_map, fl_init_ltm_map)
            ~f:(fun acc (path,dist,flow_fair_share) ->
              let nit,dlvd_map,ltm_map = acc in
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
                  let new_dlvd = SrcDstMap.add dlvd_map
                    ~key:(src,dst)
                    ~data:(prev_sd_dlvd +. flow_fair_share) in

                  (* Update latency-tput distribution for (src,dst) *)
                  let prev_sd_ltm = match SrcDstMap.find ltm_map (src,dst) with
                                      | None -> LatencyMap.empty
                                      | Some x -> x in
                  let path_latency = get_path_weight_arr topo path in
                  let prev_sd_tput_for_latency =
                    match LatencyMap.find prev_sd_ltm path_latency with
                                      | None -> 0.0
                                      | Some x -> x in
                  let new_sd_ltm = LatencyMap.add prev_sd_ltm
                    ~key:path_latency
                    ~data:(prev_sd_tput_for_latency +. flow_fair_share) in
                  let new_ltm_map = SrcDstMap.add ltm_map
                    ~key:(src,dst)
                    ~data:new_sd_ltm in
                  (nit, new_dlvd, new_ltm_map)
              | Some next_link -> (* Forward traffic to next hop *)
                  (* Update link ingress queue for next hop *)
                  let sched_traf_next_link = match EdgeMap.find nit next_link with
                      | None -> []
                      | Some v -> v in
                  if is_nan flow_fair_share then assert false;
                  let traf_next_link = (path,dist+1,flow_fair_share)::sched_traf_next_link in
                  let new_nit = EdgeMap.add ~key:next_link ~data:traf_next_link nit in
                  (new_nit, dlvd_map, ltm_map)) in
          let new_lutil_map =
            if iter < num_iterations + steady_state_time then
              begin
                let curr_lutil_e = match (EdgeMap.find curr_lutil_map e) with
                  | None -> []
                  | Some x -> x in
                EdgeMap.add ~key:e ~data:(forwarded_by_link::curr_lutil_e) curr_lutil_map
              end
            else
              curr_lutil_map in
          (new_nit, dlvd_map, ltm_map, new_lutil_map, new_fail_drop, new_cong_drop)) in
      (* Done forwarding for each link*)

      (* Print state for debugging *)
      if local_debug then
          EdgeMap.iteri next_iter_traffic
            ~f:(fun ~key:e ~data:paths_demand ->
              Printf.printf "%s\n%!" (string_of_edge topo e);
              List.iter paths_demand
              ~f:(fun (path,_,d) ->
                Printf.printf "%s\t%f\n%!" (dump_edges topo (Array.to_list path)) d));
      if local_debug then SrcDstMap.iteri new_delivered_map
            ~f:(fun ~key:(src,dst) ~data:delvd ->
              Printf.printf "%s %s\t%f\n%!"
                (Node.name (Net.Topology.vertex_to_label topo src))
                (Node.name (Net.Topology.vertex_to_label topo dst)) delvd);

      (* State carried over to next iter *)
      { ingress_link_traffic  = next_iter_traffic;
        delivered             = new_delivered_map;
        latency               = new_lat_tput_map_map;
        utilization           = new_link_utils;
        scheme                = new_scheme;
        failures              = failed_links;
        failure_drop          = new_fail_drop;
        congestion_drop       = new_cong_drop;
        real_tm               = actual_t;
        predict_tm            = predict_t; })
      (* end iteration *) in

  agg_dem := !agg_dem /. (Float.of_int num_iterations);
  agg_sink_dem := !agg_sink_dem /. (Float.of_int num_iterations);
  (* Generate stats *)
  {
    throughput =
      SrcDstMap.fold final_network_state.delivered
        ~init:SrcDstMap.empty
        ~f:(fun ~key:sd ~data:dlvd acc ->
          SrcDstMap.add acc
            ~key:sd
            ~data:(dlvd /. (Float.of_int num_iterations) /. !agg_dem));

    latency =
      get_aggregate_latency final_network_state.latency num_iterations;

    congestion =
      EdgeMap.fold final_network_state.utilization
        ~init:EdgeMap.empty
        ~f:(fun ~key:e ~data:util_list acc ->
          EdgeMap.add acc
            ~key:e
            ~data:((average_list util_list) /. (capacity_of_edge topo e),
                   (max_list util_list)    /. (capacity_of_edge topo e)));

    failure_drop =
      final_network_state.failure_drop /. (Float.of_int num_iterations) /. !agg_dem;

    congestion_drop =
      final_network_state.congestion_drop /. (Float.of_int num_iterations) /. !agg_dem;

    flash_throughput =
      if is_flash then SrcDstMap.fold final_network_state.delivered
        ~init:0.0
        ~f:(fun ~key:(src,dst) ~data:dlvd acc ->
          if dst = flash_sink then
            acc +. dlvd /. (Float.of_int num_iterations) /. !agg_sink_dem
          else acc)
      else 0.0;

    aggregate_demand = !agg_dem;
    recovery_churn = !recovery_churn;
    scheme = final_network_state.scheme;
    solver_time = !solver_time; }
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

let get_max_congestion (congestions:float list) : float =
  List.fold_left ~init:Float.nan ~f:(fun a acc -> Float.max_inan a acc) congestions

let get_num_paths (s:scheme) : float =
  let count = SrcDstMap.fold s
    ~init:0
    ~f:(fun ~key:_ ~data:d acc ->
      acc + (PathMap.length d)) in
  Float.of_int count

(* Generate latency percentile based on throughput *)
let get_latency_percentiles (lat_tput_map : throughput LatencyMap.t) (agg_dem:float) : (float LatencyMap.t) =
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
      string_of_edge topo e
      |> StringMap.find_exn edge_weights
      |> set_weight acc e)

let set_topo_weights topo =
  (*Random.init (Topology.num_vertexes topo);*)
  let topo' = List.fold_left (EdgeSet.elements (Topology.edges topo))
    ~init:StringMap.empty
    ~f:(fun acc e ->
      let w = 1. in
      (*let w = (0.5 +. Random.float 1.) in*)
      let topo = set_weight topo e w in
      StringMap.add ~key:(string_of_edge topo e) ~data:w acc) in
  (*Random.self_init ();*)
  topo'



(* Calculate a demand matrix equal to max (envelope) of all TMs *)
let calculate_demand_envelope (topo:topology) (predict_file:string) (host_file:string) (iters:int) =
  let num_tm = min iters (List.length (In_channel.read_lines predict_file)) in
  let (predict_host_map, predict_ic) = open_demands predict_file host_file topo in
  let iterations = range 0 num_tm in
  let envelope = List.fold_left iterations ~init:SrcDstMap.empty
    ~f:(fun acc n ->
      let predict = next_demand ~scale:1.0 predict_ic predict_host_map in
      SrcDstMap.fold predict
        ~init:acc
        ~f:(fun ~key:(s,d) ~data:pred acc ->
          let env_sd = match SrcDstMap.find acc (s,d) with
            | None -> 0.
            | Some x -> x in
          SrcDstMap.add ~key:(s,d) ~data:(max env_sd pred) acc)) in
  close_demands predict_ic;
  envelope

(****************** Main Simulation Function ******************)
let simulate
    (spec_solvers:solver_type list)
    (topology_file:string)
    (demand_file:string)
    (predict_file:string)
    (host_file:string)
    (num_tms_opt:int option)
    (robustness_test:bool)
    (scale:float)
    (num_failures:int)
    (is_flash:bool)
    (flash_burst_amount:float)
    (out_dir:string option) () : unit =

  let topo = Parse.from_dotfile topology_file in
  let edge_weights = set_topo_weights topo in

  (************************************************************)
  (********* Create records to store statisitics **************)
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
  let total_sink_tput_data = make_data "Total Sink Throughput vs Time" in
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
  (*******************************************************)


  (************** Compute number of iterations ***********)
  let demand_lines_length = List.length (In_channel.read_lines demand_file) in
  let predict_lines_length = List.length (In_channel.read_lines predict_file) in

  (* If number of iterations isn't specified, then set it to number of TMs in TM file,
   * or compute possible scenarios for robustness test *)
  let num_tms = match num_tms_opt with
    | Some x ->
        if robustness_test then failwith "Number of TMs is automatically decided for robustness test"
        else x
    | None ->
        min demand_lines_length predict_lines_length in

  (* precompute network scenarios to introduce - to be consistent across schemes *)
  let failure_scenarios =
    if robustness_test then
      get_all_possible_failures topo num_failures
    else
      get_failure_scenarios topo demand_file host_file num_tms num_failures scale in

  (* update number of iterations based on failure scenarios, if performing robustness test*)
  let num_tms = if robustness_test then List.length failure_scenarios else num_tms in

  (* Compute flash sink node for each iteration *)
  let flash_sinks = pick_flash_sinks topo num_tms in


  (******************************************************************)
  (******************** Iterate on routing algorithms ***************)
  List.iter
    spec_solvers
    ~f:(fun algorithm ->
      (*(praveenk): Raeke changes edge weights. Let's just reset them. *)
      ignore(reset_topo_weights edge_weights topo;);

      (* compute demand envelope if needed *)
      ignore(match algorithm with
        | SemiMcfMcfFTEnv
        | SemiMcfMcfEnv ->
            demand_envelope :=
              (calculate_demand_envelope topo predict_file host_file num_tms);
        | _ -> (););

      let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
      let (predict_host_map, predict_ic) = open_demands predict_file host_file topo in
      Printf.printf "\n";
      (* we may need to initialize the scheme, and advance both traffic files *)

      (************************************************************************)
      (*********** Iterate over all traffic matrices and accumulate stats *****)
      let _ =
        List.fold_left (range 0 num_tms) (* 0..num_tms *)
          ~init:SrcDstMap.empty
          ~f:(fun prev_scheme n ->
            Printf.printf "\nAlgo: %s TM: %d\r%!" (solver_to_string algorithm) n;
            (* get the next demand *)
            let actual = next_demand ~scale:scale actual_ic actual_host_map in
            let predict = next_demand ~scale:scale predict_ic predict_host_map in

            (* initialize algorithm *)
            if n = 0 then initialize_scheme algorithm topo predict;

            (* solve *)
            let scheme,solver_time = solve_within_budget algorithm topo predict actual in
            ignore(reset_topo_weights edge_weights topo;);

            (* simulate current traffic matrix *)
            let failing_edges = List.nth_exn failure_scenarios n in
            let flash_sink = List.nth_exn flash_sinks n in
            let tm_sim_stats =
              simulate_tm scheme topo actual failing_edges predict algorithm is_flash flash_burst_amount flash_sink in
            (* simulation done *)

            (* Accumulate statistics *)
            let exp_congestions = congestion_of_paths topo actual scheme in
            let list_of_exp_congestions = List.map
              ~f:snd
              (EdgeMap.to_alist exp_congestions) in
            let sorted_exp_congestions = List.sort
              ~cmp:(Float.compare)
              list_of_exp_congestions in
            let list_of_avg_congestions, list_of_max_congestions =
              List.map ~f:snd (EdgeMap.to_alist tm_sim_stats.congestion)
              |> split_alist in
            let max_congestions_map = EdgeMap.map ~f:snd tm_sim_stats.congestion in
            let sorted_congestions = List.sort ~cmp:(Float.compare) list_of_max_congestions in
            let total_solver_time = solver_time +. tm_sim_stats.solver_time in
            let tm_churn = get_churn prev_scheme scheme in
            let num_paths = get_num_paths scheme in
            let cmax = get_max_congestion list_of_max_congestions in
            let cmean = get_mean_congestion list_of_avg_congestions in
            let expcmax = get_max_congestion list_of_exp_congestions in
            let expcmean = get_mean_congestion list_of_exp_congestions in
            let total_tput = get_total_tput tm_sim_stats.throughput in
            let latency_percentiles =
              get_latency_percentiles tm_sim_stats.latency tm_sim_stats.aggregate_demand in

            let percentile_values sort_cong =
              List.fold_left percentiles
                ~init:[]
                ~f:(fun acc p ->
                  acc@[(kth_percentile sort_cong p)]) in

            let sname = solver_to_string algorithm in
            add_record time_data sname
              {iteration = n; time=total_solver_time; time_dev=0.0; };
            add_record tm_churn_data sname
              {iteration = n; churn=tm_churn; churn_dev=0.0; };
            add_record rec_churn_data sname
              {iteration = n; churn=tm_sim_stats.recovery_churn; churn_dev=0.0; };
            add_record num_paths_data sname
              {iteration = n; num_paths=num_paths; num_paths_dev=0.0; };
            add_record max_congestion_data sname
              {iteration = n; congestion=cmax; congestion_dev=0.0; };
            add_record mean_congestion_data sname
              {iteration = n; congestion=cmean; congestion_dev=0.0; };
            add_record max_exp_congestion_data sname
              {iteration = n; congestion=expcmax; congestion_dev=0.0; };
            add_record mean_exp_congestion_data sname
              {iteration = n; congestion=expcmean; congestion_dev=0.0; };
            add_record edge_congestion_data sname
              {iteration = n; edge_congestions=max_congestions_map; };
            add_record edge_exp_congestion_data sname
              {iteration = n; edge_congestions=exp_congestions; };
            add_record latency_percentiles_data sname
              {iteration = n; latency_percentiles=latency_percentiles; };
            add_record total_tput_data sname
              {iteration = n; throughput=total_tput; throughput_dev=0.0; };
            add_record total_sink_tput_data sname
              {iteration = n; throughput=tm_sim_stats.flash_throughput; throughput_dev=0.0; };
            add_record failure_drop_data sname
              {iteration = n; throughput=tm_sim_stats.failure_drop; throughput_dev=0.0; };
            add_record congestion_drop_data sname
              {iteration = n; throughput=tm_sim_stats.congestion_drop; throughput_dev=0.0; };
            List.iter2_exn
              percentile_data (percentile_values sorted_congestions)
              ~f:(fun d v ->
                add_record d sname
                  {iteration = n; congestion=v; congestion_dev=0.0;});
            List.iter2_exn
              exp_percentile_data (percentile_values sorted_exp_congestions)
              ~f:(fun d v ->
                add_record d sname
                  {iteration = n; congestion=v; congestion_dev=0.0;});

          scheme) in

      (* start at beginning of demands for next algorithm *)
      close_demands actual_ic;
      close_demands predict_ic;);

    (* Store results in a directory name = topology name or provided name in expData *)
    let output_dir = match out_dir with
      | Some x -> x
      | None ->
        let split_dot_file_list = String.split_on_chars topology_file ~on:['/';'.'] in
        let suffix = List.nth split_dot_file_list (List.length split_dot_file_list -2) in
        match suffix with
          | Some x -> x
          | None -> "default" in
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
    to_file dir "TotalSinkThroughputVsIterations.dat" total_sink_tput_data "# solver\titer\ttotal-throughput\tstddev" iter_vs_throughput_to_string;
    to_file dir "FailureLossVsIterations.dat" failure_drop_data "# solver\titer\tfailure-drop\tstddev" iter_vs_throughput_to_string;
    to_file dir "CongestionLossVsIterations.dat" congestion_drop_data "# solver\titer\tcongestion-drop\tstddev" iter_vs_throughput_to_string;
    to_file dir "EdgeCongestionVsIterations.dat" edge_congestion_data "# solver\titer\tedge-congestion" (iter_vs_edge_congestions_to_string topo);
    to_file dir "EdgeExpCongestionVsIterations.dat" edge_exp_congestion_data "# solver\titer\tedge-exp-congestion" (iter_vs_edge_congestions_to_string topo);
    to_file dir "LatencyDistributionVsIterations.dat" latency_percentiles_data "#solver\titer\tlatency-throughput" iter_vs_latency_percentiles_to_string;
    List.iter2_exn
      percentile_data percentiles
      ~f:(fun d p ->
        let file_name = Printf.sprintf "k%dCongestionVsIterations.dat"
                          (Int.of_float (p *. 100.)) in
        let header = Printf.sprintf "# solver\titer\t.%f-congestion\tstddev" p in
        to_file dir file_name d header iter_vs_congestion_to_string) ;
    List.iter2_exn
      exp_percentile_data percentiles
      ~f:(fun d p ->
        let file_name = Printf.sprintf "k%dExpCongestionVsIterations.dat"
                          (Int.of_float (p *. 100.)) in
        let header = Printf.sprintf "# solver\titer\t.%f-exp-congestion\tstddev" p in
        to_file dir file_name d header iter_vs_congestion_to_string) ;
    Printf.printf "\nComplete.\n"

  (************** End simulation ******************************************)
  (************************************************************************)

(* For synthetic demands, scale them by multiplying by X/mcf_congestion,
 * where X (= 0.4) is the max congestion we expect to get when run with the new demands *)
let calculate_syn_scale (topology:string) (demand_file:string) (host_file:string) =
  let topo = Parse.from_dotfile topology in
  let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
  let actual = next_demand ~scale:1.0 actual_ic actual_host_map in
  let cmax = Kulfi_Mcf.solve topo actual
          |> congestion_of_paths topo actual
          |> EdgeMap.to_alist
          |> List.map ~f:snd
          |> get_max_congestion in
  close_demands actual_ic;
  0.4 /. cmax

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
    +> flag "-semimcfmcfenv" no_arg ~doc:" run semi mcf+mcf with envelope"
    +> flag "-semimcfmcfftenv" no_arg ~doc:" run semi mcf+mcf with envelope and joint failure opt"
    +> flag "-semimcfvlb" no_arg ~doc:" run semi mcf+vlb"
    +> flag "-semimcfraeke" no_arg ~doc:" run semi mcf+raeke"
    +> flag "-semimcfraekeft" no_arg ~doc:" run semi mcf+raeke with joint failure opt"
    +> flag "-semimcfecmp" no_arg ~doc:" run semi mcf+ecmp"
    +> flag "-semimcfksp" no_arg ~doc:" run semi mcf+ksp"
    +> flag "-semimcfkspft" no_arg ~doc:" run semi mcf+ksp with joint failure opt"
    +> flag "-raeke" no_arg ~doc:" run raeke"
    +> flag "-optimalmcf" no_arg ~doc:" run optimal mcf"
    +> flag "-all" no_arg ~doc:" run all schemes"
    +> flag "-scalesyn" no_arg ~doc:" scale synthetic demands to achieve max congestion 1"
    +> flag "-deloop" no_arg ~doc:" remove loops in paths"
    +> flag "-robust" no_arg ~doc:" perform robustness test - fail all combinations of fail-num links"
    +> flag "-fail-num" (optional_with_default 1 int) ~doc:" number of links to fail"
    +> flag "-fail-time" (optional_with_default (Int.max_value/100) int) ~doc:" simulation time to introduce failure at"
    +> flag "-lr-delay" (optional_with_default (Int.max_value/100) int) ~doc:" delay between failure and local recovery"
    +> flag "-gr-delay" (optional_with_default (Int.max_value/100) int) ~doc:" delay between failure and global recovery"
    +> flag "-is-flash" no_arg ~doc:" simulate flash or not"
    +> flag "-flash-ba" (optional_with_default 0. float) ~doc:" fraction of total traffic to add as flash"
    +> flag "-flash-recover" no_arg ~doc:" perform local recovery for flash"
    +> flag "-simtime" (optional_with_default 500 int) ~doc:" time steps to simulate each TM"
    +> flag "-budget" (optional_with_default (Int.max_value/100) int) ~doc:" max paths between each pair of hosts"
    +> flag "-scale" (optional_with_default 1. float) ~doc:" scale demands by this factor"
    +> flag "-out" (optional string) ~doc:" name of directory in expData to store results"
    +> flag "-rseed" (optional int) ~doc:" seed to initialize PRNG"
    +> flag "-num-tms" (optional int) ~doc:" number of TMs (-robust overrides this)"
    +> flag "-gurobi-method" (optional_with_default (-1) int) ~doc:" solver method used for Gurobi. -1=automatic, 0=primal simplex, 1=dual simplex, 2=barrier, 3=concurrent, 4=deterministic concurrent."
    +> anon ("topology-file" %: string)
    +> anon ("demand-file" %: string)
    +> anon ("predict-file" %: string)
    +> anon ("host-file" %: string)
  ) (fun
    (mcf:bool)
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
    (robust:bool)
    (fail_num:int)
    (fail_time:int)
    (lr_delay:int)
    (gr_delay:int)
    (is_flash:bool)
    (flash_ba:float)
    (flash_recover:bool)
    (simtime:int)
    (budget:int)
    (scale:float)
    (out:string option)
    (rseed:int option)
    (num_tms:int option)
    (grb_method:int)
    (topology_file:string)
    (demand_file:string)
    (predict_file:string)
    (host_file:string) () ->
      let algorithms =
        List.filter_map ~f:(fun x -> x)
         [ if mcf || all        then Some Mcf         else None
         ; if optimalmcf || all then Some OptimalMcf  else None
         ; if mwmcf             then Some MwMcf       else None
         ; if vlb || all        then Some Vlb         else None
         ; if ecmp || all       then Some Ecmp        else None
         ; if ksp || all        then Some Ksp         else None
         ; if spf || all        then Some Spf         else None
         ; if akmcf             then Some AkMcf       else None
         ; if akvlb             then Some AkVlb       else None
         ; if akecmp            then Some AkEcmp      else None
         ; if akksp             then Some AkKsp       else None
         ; if akraeke           then Some AkRaeke     else None
         ; if raeke || all      then Some Raeke       else None
         ; if semimcfmcf        then Some SemiMcfMcf  else None
         ; if semimcfmcfenv || all    then Some SemiMcfMcfEnv   else None
         ; if semimcfmcfftenv || all  then Some SemiMcfMcfFTEnv else None
         ; if semimcfecmp || all      then Some SemiMcfEcmp     else None
         ; if semimcfksp || all       then Some SemiMcfKsp      else None
         ; if semimcfkspft            then Some SemiMcfKspFT    else None
         ; if semimcfvlb || all       then Some SemiMcfVlb      else None
         ; if semimcfraeke || all     then Some SemiMcfRaeke    else None
         ; if semimcfraekeft || all   then Some SemiMcfRaekeFT  else None ] in
      let syn_scale =
        if scalesyn then
          calculate_syn_scale topology_file demand_file host_file
        else 1.0 in
      let tot_scale = scale *. syn_scale in
      Printf.printf "Scale factor: %f\n\n" tot_scale;
      Kulfi_Globals.deloop := deloop;
      Kulfi_Globals.tm_sim_iters  := simtime;
      Kulfi_Globals.flash_recover := flash_recover;
      Kulfi_Globals.gurobi_method := grb_method;
      Kulfi_Globals.budget        := budget;
      Kulfi_Globals.failure_time  := fail_time;
      Kulfi_Globals.rand_seed     := rseed;
      Kulfi_Globals.local_recovery_delay  := lr_delay;
      Kulfi_Globals.global_recovery_delay := gr_delay;
      if robust then
        Kulfi_Globals.failure_time  := 0;
     simulate algorithms topology_file demand_file predict_file host_file num_tms robust tot_scale fail_num is_flash flash_ba out ())

let main = Command.run command

let _ = main

