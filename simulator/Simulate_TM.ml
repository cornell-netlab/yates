open Core

open Simulate_Switch
open Simulation_Types
open Simulation_Util
open Yates_routing.Util
open Yates_solvers.Solvers
open Yates_types.Types
open Yates_utils
(***********************************************************)
(* Simulate one traffic matrix and generate statistics *)
(***********************************************************)

(* Compute a routing scheme for an algorithm and apply budget by pruning the
   top-k paths. Also, round path weights if nbins is specified. *)
let solve_within_budget algorithm topo predict actual : (scheme * float) =
  let at = AutoTimer.make_auto_timer () in
  AutoTimer.start at;
  let solve = select_algorithm algorithm in
  let budget = !Yates_routing.Globals.budget in
  let sch = match algorithm with
    | OptimalMcf ->
      (* Use actual demands for Optimal, without any budget restriction *)
      solve topo actual
    | _ ->
        prune_scheme topo (solve topo predict) budget in
  AutoTimer.stop at;
  let sch =
    match !Yates_routing.Globals.nbins with
    | None -> sch
    | Some nbins ->
      begin
        match algorithm with
        | OptimalMcf -> sch
        | _ -> fit_scheme_to_bins sch nbins
      end in
  (*assert (probabilities_sum_to_one sch);*)
  (sch, (AutoTimer.get_time_in_seconds at))

let linearly_combine_schemes hi_fraction hi_sch lo_sch =
  let updated_hi = SrcDstMap.fold hi_sch ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:hi_paths acc ->
      let hi_paths =
        PathMap.fold hi_paths ~init:PathMap.empty
          ~f:(fun ~key:path ~data:hi_prob acc ->
            PathMap.set ~key:path ~data:(hi_fraction *. hi_prob) acc) in
      let paths =
        match SrcDstMap.find lo_sch (src,dst) with
        | None -> hi_paths
        | Some lo_paths ->
          PathMap.fold lo_paths ~init:hi_paths
            ~f:(fun ~key:path ~data:lo_prob acc ->
              let lo_prob = (1. -. hi_fraction) *. lo_prob in
              let prob = match PathMap.find acc path with
                | None -> lo_prob
                | Some p -> lo_prob +. p in
              PathMap.set ~key:path ~data:prob acc) in
      SrcDstMap.set ~key:(src,dst) ~data:paths acc) in
  let sch = SrcDstMap.fold lo_sch ~init:updated_hi
    ~f:(fun ~key:(src,dst) ~data:lo_paths acc ->
      match SrcDstMap.find acc (src,dst) with
      | None -> (* if we hadn't seen this key earlier *)
        SrcDstMap.set ~key:(src, dst) ~data:lo_paths acc
      | Some _ -> (* we have already processed it *)
        acc) in
  normalize_scheme sch

(* Split a TM into two traffic matrices based on fraction of high priority
   traffic *)
let split_demands_pri (dem:demands) (hipri_fraction:float) : demands * demands =
  SrcDstMap.fold ~init:(SrcDstMap.empty, SrcDstMap.empty)
    ~f:(fun ~key:(src,dst) ~data:total_dem (hi_acc, low_acc) ->
      let hi_dem = hipri_fraction *. total_dem in
      let low_dem = total_dem -. hi_dem in
      (SrcDstMap.set ~key:(src,dst) ~data:hi_dem hi_acc,
       SrcDstMap.set ~key:(src,dst) ~data:low_dem low_acc)) dem

(* Return a topology with residual edge capacities after reservation *)
let reserve_bw (topo:topology) (reservation:float EdgeMap.t) : topology =
  EdgeMap.fold reservation ~init:topo
    ~f:(fun ~key:edge ~data:bw acc ->
      let new_cap = Float.to_int64 (capacity_of_edge acc edge -. bw) in
      let label = Topology.edge_to_label acc edge in
      let new_label = Link.create (Link.cost label) new_cap in
      Link.set_weight new_label (Link.weight label);
      let src_node, src_port = Net.Topology.edge_src edge in
      let dst_node, dst_port = Net.Topology.edge_dst edge in
      let t' = Topology.remove_edge acc edge in
      let new_topo,_ =
        Topology.add_edge t' src_node src_port new_label dst_node dst_port in
      new_topo)

(* Compute a routing scheme for an algorithm and apply budget by pruning the
   top-k paths. Also, round path weights if nbins is specified. Differentiate
   hipri an lowpri traffic. *)
let solve_within_budget_multipri algorithm topo predict actual hipri_fraction :
  (scheme * float) * (scheme * float) =
  (* compute traffic matrices for both priority classes *)
  let hipri_actual, lowpri_actual = split_demands_pri actual hipri_fraction in
  let hipri_predict, lowpri_predict = match algorithm with
    | OptimalMcf -> (hipri_actual, lowpri_actual)
    | _  -> split_demands_pri predict hipri_fraction in

  (* route high priority traffic using cSPF *)
  let hipri_scheme,hi_stime = solve_within_budget Cspf topo hipri_predict hipri_actual in

  (* reserve bandwidth for high priority traffic *)
  let hipri_reserved_bw = traffic_on_edge topo hipri_predict hipri_scheme in
  let lopri_topo = reserve_bw topo hipri_reserved_bw in
  let lopri_scheme,lo_stime =
    solve_within_budget algorithm lopri_topo lowpri_predict lowpri_actual in
  (* let sch = linearly_combine_schemes hipri_fraction hipri_scheme lopri_scheme in *)
  (* let sch = *)
    (* match algorithm with *)
    (* | OptimalMcf -> sch *)
    (* | _ -> *)
      (* begin *)
        (* let sch = prune_scheme topo sch !Yates_routing.Globals.budget in *)
        (* match !Yates_routing.Globals.nbins with *)
        (* | None -> sch *)
        (* | Some nbins -> fit_scheme_to_bins sch nbins *)
      (* end in *)
  ((hipri_scheme, hi_stime), (lopri_scheme, lo_stime))

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
                PathMap.set ~key:path ~data:(prob, demand) acc
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
let get_aggregate_latency (sd_lat_tput_map_map:(throughput LatencyMap.t) SrcDstMap.t)
    (num_iter:int) : (throughput LatencyMap.t) =
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
        LatencyMap.set ~key:latency ~data:(agg_tput) acc))

let get_num_paths (s:scheme) : float =
let count = SrcDstMap.fold s
  ~init:0
  ~f:(fun ~key:_ ~data:d acc ->
    acc + (PathMap.length d)) in
Float.of_int count

(* Generate latency percentile based on throughput *)
let get_latency_percentiles (lat_tput_map : throughput LatencyMap.t)
    (agg_dem:float) : (float LatencyMap.t) =
  let latency_percentiles,_ = LatencyMap.fold lat_tput_map
    ~init:(LatencyMap.empty,0.0)
    ~f:(fun ~key:latency ~data:tput acc ->
      let lat_percentile_map,sum_tput = acc in
      let sum_tput' = sum_tput +. tput in
      (LatencyMap.set ~key:latency ~data:(sum_tput' /. agg_dem)
      lat_percentile_map, sum_tput')) in
  latency_percentiles

(* Global recovery: recompute routing scheme after removing failed links *)
let global_recovery (failed_links:failure) (predict:demands) (actual:demands)
    (algorithm:solver_type) (topo:topology) : (scheme * float) =
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
          SrcDstMap.set ~key:(src,sink) ~data:sd_dem acc)

(* find the destination for flash crowd *)
let pick_flash_sinks (topo:topology) (iters:int) =
  let hosts = get_hosts topo in
  let num_hosts = List.length hosts in
  List.fold_left (range 0 iters) ~init:[]
  ~f:(fun acc n ->
    let selected = List.nth_exn hosts (n % num_hosts) in
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
(* Main function to simulate routing for one TM *)
(***********************************************************)
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
  (* Number of timesteps to simulate *)
  let num_iterations = !Yates_routing.Globals.tm_sim_iters in
  (* wait for network to reach steady state *)
  let steady_state_time = 0 in
  (* wait for in-flight pkts to be delivered at the end *)
  let wait_out_time = 50 in
  (* Timestamp at which failures, if any, are introduced *)
  let failure_time =
    if (EdgeSet.is_empty fail_edges) ||
       (!Yates_routing.Globals.failure_time > num_iterations)
    then Int.max_value/100
    else !Yates_routing.Globals.failure_time + steady_state_time in
  let local_recovery_delay = !Yates_routing.Globals.local_recovery_delay in
  let global_recovery_delay = !Yates_routing.Globals.global_recovery_delay in
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
  let per_src_flash_factor =
    flash_ba *. !agg_dem /. (total_flow_to_sink flash_sink topo dem)
    /. (Float.of_int (List.length (get_hosts topo))) in
  if is_flash then Printf.printf "\t\t\t\t\tSink: %s\r"
      (string_of_vertex topo flash_sink);

  (* Main loop: iterate over timesteps and update network state *)
  let initial_network_state =
    { ingress_link_traffic = EdgeMap.empty;
      delivered = SrcDstMap.empty;
      latency = SrcDstMap.empty;
      utilization = EdgeMap.empty;
      scheme = start_scheme;
      failures = EdgeSet.empty;
      failure_drop = 0.0;
      congestion_drop = 0.0;
      real_tm = dem;
      predict_tm = predict } in

  let final_network_state =
    List.fold_left iterations
      ~init:initial_network_state
      ~f:(fun current_state iter ->
          (* begin iteration - time *)
          Printf.printf "\t\t\t\t   %s\r%!"
            (progress_bar (iter - steady_state_time)
               (num_iterations + wait_out_time) 15);

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
                Printf.printf "\t\t\t\t\t\t     %s  \r"
                  (dump_edges topo (EdgeSet.elements fail_edges));
                fail_edges
              end
            else current_state.failures in

          (* update tms and scheme for flash *)
          let actual_t, predict_t, new_scheme =
            if is_flash &&
               (iter >= steady_state_time) &&
               (iter < steady_state_time + num_iterations) then
              begin
                let flash_t = iter - steady_state_time in
                if flash_t % flash_step_time = 0 then
                  begin (* update actual & pred demand  and scheme *)
                    let act' =
                      update_flash_demand topo flash_sink dem flash_t
                        per_src_flash_factor (Float.of_int num_iterations) in
                    let pred' = update_flash_demand topo flash_sink dem
                        (flash_t - flash_pred_delay) per_src_flash_factor
                        (Float.of_int num_iterations) in
                    let sch' = match algorithm with
                      | OptimalMcf ->
                        begin
                          let sch,rec_solve_time =
                            global_recovery failed_links pred' act' algorithm topo in
                          recovery_churn :=
                            !recovery_churn +. (get_churn current_state.scheme sch);
                          solver_time := !solver_time +. rec_solve_time;
                          sch
                        end
                      | _ ->
                        if !Yates_routing.Globals.flash_recover then
                          (select_local_recovery algorithm) current_state.scheme
                            topo failed_links pred'
                        else current_state.scheme in
                    act',
                    pred',
                    sch'
                  end
                else
                  current_state.real_tm,
                  current_state.predict_tm,
                  current_state.scheme
              end
            else
              current_state.real_tm,
              current_state.predict_tm,
              current_state.scheme in

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
                    let sch,rec_solve_time =
                      global_recovery failed_links predict_t actual_t algorithm topo in
                    recovery_churn := !recovery_churn +. (get_churn new_scheme sch);
                    solver_time := !solver_time +. rec_solve_time;
                    sch
                  end
                else new_scheme
              | _ ->
                if iter = (local_recovery_delay + failure_time) then
                  ((select_local_recovery algorithm) new_scheme topo failed_links predict_t)
                else if iter = (global_recovery_delay + failure_time) then
                  begin
                    let sch,rec_solve_time =
                      global_recovery failed_links predict_t actual_t algorithm topo in
                    recovery_churn := !recovery_churn +. (get_churn new_scheme sch);
                    solver_time := !solver_time +. rec_solve_time;
                    sch
                  end
                else new_scheme
            else SrcDstMap.empty in

          (* Stop traffic generation after simulation end time *)
          let actual_t =
            if iter < (steady_state_time + num_iterations) then actual_t
            else SrcDstMap.empty in

          (* Add traffic at source of every path *)
          if iter < (num_iterations + steady_state_time) then
            agg_dem := !agg_dem +. (sum_demands actual_t);
          if iter < (num_iterations + steady_state_time) then
            agg_sink_dem := !agg_sink_dem +. (sum_sink_demands actual_t flash_sink);

          (* probability of taking each path *)
          let path_prob_arr = get_path_prob_demand_arr new_scheme actual_t in

          (* next_iter_traffic : map edge -> in_traffic in next iter *)
          let next_iter_traffic = List.fold_left path_prob_arr
              ~init:EdgeMap.empty
              ~f:(fun acc (path_arr, dist, (prob,sd_demand)) ->
                  if Array.length path_arr = 0 then acc else
                    let first_link = path_arr.(0) in
                    let sched_traf_first_link =
                      match EdgeMap.find acc first_link with
                      | None -> []
                      | Some v -> v in
                    let traf_first_link =
                      (path_arr, 1, (prob *. sd_demand))::sched_traf_first_link in
                    EdgeMap.set ~key:first_link ~data:traf_first_link acc) in

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

          (*  For each link, *)
          (*    forward fair share of flows to next links, *)
          (*    or deliver to destination *)
          let next_iter_traffic,
              new_delivered_map,
              new_lat_tput_map_map,
              new_link_utils,
              new_fail_drop,
              new_cong_drop =
            EdgeMap.fold current_state.ingress_link_traffic
              ~init:(next_iter_traffic,
                     current_state.delivered,
                     current_state.latency,
                     current_state.utilization,
                     cumul_fail_drop,
                     current_state.congestion_drop)
              ~f:(fun ~key:e ~data:in_queue_edge link_iter_acc ->
                  (* edge capacity can change due to failures *)
                  let current_edge_capacity =
                    (curr_capacity_of_edge topo e failed_links) in

                  (* total ingress traffic on link *)
                  let demand_on_link = List.fold_left in_queue_edge
                      ~init:0.0
                      ~f:(fun link_dem (_,_,flow_dem) ->
                          if is_nan flow_dem then Printf.printf "flow_dem is nan!!\n";
                          link_dem +. flow_dem) in

                  if local_debug then Printf.printf "%s: %f / %f\n%!"
                      (string_of_edge topo e) (demand_on_link /. 1e9)
                      (current_edge_capacity /. 1e9);

                  (* calculate each flow's fair share *)
                  let fs_in_queue_edge =
                    if demand_on_link <= current_edge_capacity then
                      in_queue_edge
                    else
                      fair_share_at_edge_arr current_edge_capacity in_queue_edge in

                  (* Update traffic dropped due to failure or congestion *)
                  let forwarded_by_link = List.fold_left fs_in_queue_edge
                      ~init:0.0
                      ~f:(fun fwd_acc (_,_,flow_dem) -> fwd_acc +. flow_dem) in
                  let dropped = demand_on_link -. forwarded_by_link in

                  let (_,_,_,curr_lutil_map,curr_fail_drop,curr_cong_drop) = link_iter_acc in
                  let new_fail_drop,new_cong_drop =
                    if EdgeSet.mem failed_links e then
                      curr_fail_drop +. dropped, curr_cong_drop
                    else
                      curr_fail_drop, curr_cong_drop +. dropped in
                  if (is_nan new_cong_drop) && (is_nan dropped) then
                    Printf.printf "dem = %f\tfwd = %f\n"
                      demand_on_link forwarded_by_link;

                  (* Forward/deliver traffic on this edge by iterating over
                     every flow in the ingress queue *)
                  let fl_init_nit,fl_init_dlvd_map,fl_init_ltm_map,_,_,_ = link_iter_acc in
                  let new_nit,
                      dlvd_map,
                      ltm_map =
                    List.fold_left fs_in_queue_edge
                      ~init:(fl_init_nit,
                             fl_init_dlvd_map,
                             fl_init_ltm_map)
                      ~f:(fun acc (path, dist, flow_fair_share) ->
                          let nit,dlvd_map,ltm_map = acc in
                          let next_link_opt = next_hop_arr path dist in
                          match next_link_opt with
                          | None ->
                            (* End of path, deliver traffic to dst *)
                            let (src,dst) =
                              match get_src_dst_for_path_arr path with
                              | None -> failwith "Empty path"
                              | Some x -> x in
                            (* Update delivered traffic for (src,dst) *)
                            let prev_sd_dlvd =
                              match SrcDstMap.find dlvd_map (src,dst) with
                              | None -> 0.0
                              | Some x -> x in
                            let new_dlvd = SrcDstMap.set dlvd_map
                                ~key:(src,dst)
                                ~data:(prev_sd_dlvd +. flow_fair_share) in

                            (* Update latency-tput distribution for (src,dst) *)
                            let prev_sd_ltm =
                              match SrcDstMap.find ltm_map (src,dst) with
                              | None -> LatencyMap.empty
                              | Some x -> x in
                            let path_latency = get_path_weight_arr topo path in
                            let prev_sd_tput_for_latency =
                              match LatencyMap.find prev_sd_ltm path_latency with
                              | None -> 0.0
                              | Some x -> x in
                            let new_sd_ltm = LatencyMap.set prev_sd_ltm
                                ~key:path_latency
                                ~data:(prev_sd_tput_for_latency +. flow_fair_share) in
                            let new_ltm_map = SrcDstMap.set ltm_map
                                ~key:(src,dst)
                                ~data:new_sd_ltm in
                            (nit,
                             new_dlvd,
                             new_ltm_map)
                          | Some next_link ->
                            (* Else, Forward traffic to next hop *)
                            (* Update link ingress queue for next hop *)
                            let sched_traf_next_link =
                              match EdgeMap.find nit next_link with
                              | None -> []
                              | Some v -> v in
                            if is_nan flow_fair_share then assert false;
                            let traf_next_link =
                              (path,dist+1,flow_fair_share)::sched_traf_next_link in
                            let new_nit =
                              EdgeMap.set ~key:next_link ~data:traf_next_link nit in
                            (new_nit,
                             dlvd_map,
                             ltm_map)) in
                  (* end: iteration over flows *)
                  (* log link utilization for this edge & timestep *)
                  let new_lutil_map =
                    if iter < num_iterations + steady_state_time then
                      begin
                        let curr_lutil_e =
                          match (EdgeMap.find curr_lutil_map e) with
                          | None -> []
                          | Some x -> x in
                        EdgeMap.set curr_lutil_map
                          ~key:e
                          ~data:(forwarded_by_link::curr_lutil_e)
                      end
                    else
                      curr_lutil_map in
                  (new_nit,
                   dlvd_map,
                   ltm_map,
                   new_lutil_map,
                   new_fail_drop,
                   new_cong_drop)) in
          (* Done forwarding for each link *)

          (* Print state for debugging *)
          if local_debug then
            EdgeMap.iteri next_iter_traffic
              ~f:(fun ~key:e ~data:paths_demand ->
                  Printf.printf "%s\n%!" (string_of_edge topo e);
                  List.iter paths_demand
                    ~f:(fun (path,_,d) ->
                        Printf.printf "%s\t%f\n%!"
                          (dump_edges topo (Array.to_list path)) d));
          if local_debug then SrcDstMap.iteri new_delivered_map
              ~f:(fun ~key:(src,dst) ~data:delvd ->
                  Printf.printf "%s %s\t%f\n%!"
                    (Node.name (Net.Topology.vertex_to_label topo src))
                    (Node.name (Net.Topology.vertex_to_label topo dst)) delvd);

          (* State carried over to next timestep *)
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
      (* end iteration over timesteps *) in

  agg_dem := !agg_dem /. (Float.of_int num_iterations);
  agg_sink_dem := !agg_sink_dem /. (Float.of_int num_iterations);
  (* Generate stats *)
  {
    throughput =
      SrcDstMap.fold final_network_state.delivered
        ~init:SrcDstMap.empty
        ~f:(fun ~key:sd ~data:dlvd acc ->
            SrcDstMap.set acc
              ~key:sd
              ~data:(dlvd /. (Float.of_int num_iterations) /. !agg_dem));

    latency =
      get_aggregate_latency final_network_state.latency num_iterations;

    congestion =
      EdgeMap.fold final_network_state.utilization
        ~init:EdgeMap.empty
        ~f:(fun ~key:e ~data:util_list acc ->
            EdgeMap.set acc
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
