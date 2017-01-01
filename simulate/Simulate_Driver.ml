open Core.Std
open Frenetic_Network
open Net
open Kulfi_Types
open Kulfi_Routing
open Kulfi_Routing_Util
open Kulfi_Traffic
open Simulate_Exps
open Simulate_Demands
open RunningStat
open ExperimentalData
open Kulfi_Globals
open Kulfi_Util
open Simulate_Failure
open Simulate_TM
open Simulation_Types
open Simulation_Util

(* Set weight of a specified edge *)
let set_weight topo edge wt : unit =
  let label = Topology.edge_to_label topo edge in
  Link.set_weight label wt

(* Reset topology with edge weights as specified *)
let reset_topo_weights edge_weights topo =
  EdgeSet.iter (Topology.edges topo)
    ~f:(fun edge ->
      string_of_edge topo edge
      |> StringMap.find_exn edge_weights
      |> set_weight topo edge)

(* Parse a file specifying RTT of links.
 * Return a map from edge's string repr to its RTT *)
let parse_rtt_file (rtt_file_opt : string option) : (float StringMap.t) =
  match rtt_file_opt with
  | None -> StringMap.empty
  | Some rtt_file ->
      In_channel.with_file rtt_file
      ~f:(fun file ->
        In_channel.fold_lines file ~init:StringMap.empty
        ~f:(fun acc line ->
          let entries = Array.of_list (String.split line ~on:' ') in
          let edge_str = entries.(0) in
          let edge_rtt = Float.of_string entries.(1) in
          StringMap.add ~key:edge_str ~data:edge_rtt acc))

(* Set edge weights from the specified RTT file.
 * If RTT of edge is not specified, the set weight = 1 *)
let set_topo_weights (topo : topology) (rtt_file : string option) =
  List.fold_left (EdgeSet.elements (Topology.edges topo))
    ~init:(parse_rtt_file rtt_file)
    ~f:(fun acc edge ->
      let edge_str = string_of_edge topo edge in
      match StringMap.find acc edge_str with
      | Some wt -> begin
          set_weight topo edge wt;
          acc
      end
      | None -> begin
          let wt = 1. in (* default weight if unspecified *)
          set_weight topo edge wt;
          StringMap.add ~key:edge_str ~data:wt acc
      end)

(* Calculate a demand matrix equal to max (envelope) of all TMs *)
let calculate_demand_envelope (topo:topology) (predict_file:string)
    (host_file:string) (iters:int) =
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

(* Measure vulnerability of routing schemes to link failures *)
let accumulate_vulnerability_stats topology_file topo algorithm scheme  =
  let solver_name = (solver_to_string algorithm) in
  let split_dot_file_list = String.split_on_chars topology_file ~on:['/';'.'] in
  let suffix = List.nth split_dot_file_list (List.length split_dot_file_list -2) in
  let mult = 2520 in
  let suffix = match suffix with
          | Some x -> x
          | None -> "default" in
  let file_name = suffix ^ "_b" ^ (string_of_int !Kulfi_Globals.budget) ^ ".vscore" in
  let vuln_score_count = SrcDstMap.fold scheme
      ~init:IntMap.empty
      ~f:(fun ~key:(s,d) ~data:path_prob_map acc ->
        let num_paths = PathMap.length path_prob_map in
        let count_edge_usage = PathMap.fold path_prob_map
        ~init:EdgeMap.empty
        ~f:(fun ~key:path ~data:_ acc ->
          List.fold_left path ~init:acc
          ~f:(fun acc e ->
            if edge_connects_switches e topo then (* consider only switch-switch edges *)
              let e_count = match EdgeMap.find acc e with
                  | Some x -> x
                  | None -> 0 in
              (* normalize by num_paths*)
              EdgeMap.add ~key:e ~data:(e_count+(mult/num_paths)) acc
            else acc)) in
        EdgeMap.fold count_edge_usage ~init:acc
          ~f:(fun ~key:e ~data:vuln_score acc ->
              let count = match IntMap.find acc vuln_score with
                | Some x -> x
                | None -> 0. in
              IntMap.add ~key:vuln_score ~data:(count+.1.) acc)) in

  let buf = Buffer.create 101 in
  Printf.bprintf buf "\n\n%s\n" solver_name;
  let tot_count = IntMap.fold vuln_score_count ~init:0.
      ~f:(fun ~key:_ ~data:d acc -> acc +. d) in
  IntMap.iteri vuln_score_count ~f:(fun ~key:n ~data:c ->
    Printf.bprintf buf "%f : %f\n" ((Float.of_int n) /. (Float.of_int mult))
    (c /.  tot_count));
  let dir = "./expData/" in
  let _ = match (Sys.file_exists dir) with | `No -> Unix.mkdir dir | _ -> () in
  let oc = Out_channel.create (dir ^ file_name) ~append:true in
  fprintf oc "%s\n" (Buffer.contents buf);
  Out_channel.close oc

(****************** Main Simulation Function ******************)
let simulate
    (spec_solvers:solver_type list)
    (topology_file:string)
    (demand_file:string)
    (predict_file:string)
    (host_file:string)
    (num_tms_opt:int option)
    (robustness_test:bool)
    (vulnerability_test:bool)
    (scale:float)
    (num_failures:int)
    (is_flash:bool)
    (flash_burst_amount:float)
    (rtt_file_opt:string option)
    (out_dir:string option) () : unit =

  let topo = Parse.from_dotfile topology_file in
  let edge_weights = set_topo_weights topo rtt_file_opt in

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

  Printf.printf "TE Algorithm  TMs \t\t   Simulation        Failures              Recovery%!";

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
            Printf.printf "%12s  %s\r%!" (solver_to_string algorithm) (progress_bar n num_tms 15);
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
            (* measure robustness of routing algs in terms of shared edges between paths *)
            if vulnerability_test then
              begin
                accumulate_vulnerability_stats topology_file topo algorithm scheme ;
                scheme
              end
            else

            let tm_sim_stats =
              simulate_tm scheme topo actual failing_edges predict algorithm
                is_flash flash_burst_amount flash_sink in
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
            let latency_percentiles = get_latency_percentiles
                tm_sim_stats.latency tm_sim_stats.aggregate_demand in

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
    to_file dir "TMChurnVsIterations.dat" tm_churn_data
      "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string;
    to_file dir "RecoveryChurnVsIterations.dat" rec_churn_data
      "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string;
    to_file dir "NumPathsVsIterations.dat" num_paths_data
      "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string;
    to_file dir "TimeVsIterations.dat" time_data
      "# solver\titer\ttime\tstddev" iter_vs_time_to_string;
    to_file dir "MaxCongestionVsIterations.dat" max_congestion_data
      "# solver\titer\tmax-congestion\tstddev" iter_vs_congestion_to_string;
    to_file dir "MeanCongestionVsIterations.dat" mean_congestion_data
      "# solver\titer\tmean-congestion\tstddev" iter_vs_congestion_to_string;
    to_file dir "MaxExpCongestionVsIterations.dat" max_exp_congestion_data
      "# solver\titer\tmax-exp-congestion\tstddev" iter_vs_congestion_to_string;
    to_file dir "MeanExpCongestionVsIterations.dat" mean_exp_congestion_data
      "# solver\titer\tmean-exp-congestion\tstddev" iter_vs_congestion_to_string;
    to_file dir "TotalThroughputVsIterations.dat" total_tput_data
      "# solver\titer\ttotal-throughput\tstddev" iter_vs_throughput_to_string;
    to_file dir "TotalSinkThroughputVsIterations.dat" total_sink_tput_data
      "# solver\titer\ttotal-throughput\tstddev" iter_vs_throughput_to_string;
    to_file dir "FailureLossVsIterations.dat" failure_drop_data
      "# solver\titer\tfailure-drop\tstddev" iter_vs_throughput_to_string;
    to_file dir "CongestionLossVsIterations.dat" congestion_drop_data
      "# solver\titer\tcongestion-drop\tstddev" iter_vs_throughput_to_string;
    to_file dir "EdgeCongestionVsIterations.dat" edge_congestion_data
      "# solver\titer\tedge-congestion" (iter_vs_edge_congestions_to_string topo);
    to_file dir "EdgeExpCongestionVsIterations.dat" edge_exp_congestion_data
      "# solver\titer\tedge-exp-congestion" (iter_vs_edge_congestions_to_string topo);
    to_file dir "LatencyDistributionVsIterations.dat" latency_percentiles_data
      "#solver\titer\tlatency-throughput" iter_vs_latency_percentiles_to_string;
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

(* Estimate max congestion for a given topology, tm, solver and scale*)
let estimate_max_cong (topology:string) (demand_file:string) (host_file:string)
    (solver) (scale) : float =
  let topo = Parse.from_dotfile topology in
  let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
  let actual = next_demand ~scale:scale actual_ic actual_host_map in
  let cmax = solver topo actual
          |> congestion_of_paths topo actual
          |> EdgeMap.to_alist
          |> List.map ~f:snd
          |> get_max_congestion in
  close_demands actual_ic;
  cmax

(* For synthetic demands, scale them by multiplying by X/mcf_congestion,
 * where X (= 0.4) is the max congestion we expect to get when run with the new demands *)
let calculate_syn_scale (topology:string) (demand_file:string) (host_file:string) =
  let cmax = estimate_max_cong topology demand_file host_file Kulfi_Mcf.solve 1.0 in
  0.4 /. cmax


let compare_scaling_limit algorithms (num_tms:int option) (topology:string)
    (demand_file:string) (host_file:string) (rtt_file_opt:string option)
    (out_dir:string option) () =
  Printf.printf "Scale test\n%!";
  let split_dot_file_list = String.split_on_chars topology ~on:['/';'.'] in
  let suffix = List.nth split_dot_file_list (List.length split_dot_file_list -2) in
  let suffix = match suffix with
          | Some x -> x
          | None -> "default" in
  let out_dir = match out_dir with
    | None -> suffix
    | Some x -> x in
  let file_name = "LimitingScale.dat" in
  let num_tms = match num_tms with
    | Some x -> x
    | None -> 24 in
  let norm = calculate_syn_scale topology demand_file host_file in
  let topo = Parse.from_dotfile topology in
  let edge_weights = set_topo_weights topo rtt_file_opt in
  let buf = Buffer.create 101 in
  List.iter algorithms ~f:(fun algorithm ->
    Printf.printf "%s\n%!" (solver_to_string algorithm);
    let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
    List.iter (range 0 num_tms) ~f:(fun i ->
      let actual = next_demand ~scale:norm actual_ic actual_host_map in
      ignore(match algorithm with
        | SemiMcfMcfEnv
        | SemiMcfMcfFTEnv ->
            (* avoid recomputing the same scheme for envelope-based MCF *)
            if i = 0 then
              begin
                demand_envelope :=
                  (calculate_demand_envelope topo demand_file host_file num_tms);
              initialize_scheme algorithm topo actual;
              end
        | _ -> (* initialize every time for average case analysis *)
            initialize_scheme algorithm topo actual;);
      let scheme,_ = solve_within_budget algorithm topo actual actual in
      ignore(reset_topo_weights edge_weights topo;);
      let cmax = congestion_of_paths topo actual scheme
                |> EdgeMap.to_alist
                |> List.map ~f:snd
                |> get_max_congestion in
      Printf.bprintf buf "%s\t%d\t%f\t0.0\n"
        (solver_to_string algorithm) i (1. /. cmax) );
      close_demands actual_ic);

  let dir = "./expData/" ^ out_dir ^ "/" in
  let _ = match (Sys.file_exists dir) with | `No -> Unix.mkdir dir | _ -> () in
  let oc = Out_channel.create (dir ^ file_name) in
  fprintf oc "%s\n" (Buffer.contents buf);
  Out_channel.close oc



let command =
  Command.basic
    ~summary:"Simulate run of routing strategies"
    Command.Spec.(
    empty
    +> flag "-ac" no_arg ~doc:" run ac"
    +> flag "-akecmp" no_arg ~doc:" run ak+ecmp"
    +> flag "-akksp" no_arg ~doc:" run ak+ksp"
    +> flag "-akmcf" no_arg ~doc:" run ak+mcf"
    +> flag "-akraeke" no_arg ~doc:" run ak+raeke"
    +> flag "-akvlb" no_arg ~doc:" run ak+vlb"
    +> flag "-ecmp" no_arg ~doc:" run ecmp"
    +> flag "-edksp" no_arg ~doc:" run edge-disjoint ksp"
    +> flag "-ffc" no_arg ~doc:" run FFC with KSP base path set"
    +> flag "-ffced" no_arg ~doc:" run FFC with Edge-disjoint KSP base path set"
    +> flag "-ksp" no_arg ~doc:" run ksp"
    +> flag "-mcf" no_arg ~doc:" run mcf"
    +> flag "-mwmcf" no_arg ~doc:" run mwmcf"
    +> flag "-optimalmcf" no_arg ~doc:" run optimal mcf"
    +> flag "-raeke" no_arg ~doc:" run raeke"
    +> flag "-semimcfac" no_arg ~doc:" run semi mcf+ac"
    +> flag "-semimcfecmp" no_arg ~doc:" run semi mcf+ecmp"
    +> flag "-semimcfedksp" no_arg ~doc:" run semi mcf+edksp"
    +> flag "-semimcfksp" no_arg ~doc:" run semi mcf+ksp"
    +> flag "-semimcfkspft" no_arg ~doc:" run semi mcf+ksp with joint failure opt"
    +> flag "-semimcfmcf" no_arg ~doc:" run semi mcf+mcf"
    +> flag "-semimcfmcfenv" no_arg ~doc:" run semi mcf+mcf with envelope"
    +> flag "-semimcfmcfftenv" no_arg ~doc:" run semi mcf+mcf with envelope and joint failure opt"
    +> flag "-semimcfraeke" no_arg ~doc:" run semi mcf+raeke"
    +> flag "-semimcfraekeft" no_arg ~doc:" run semi mcf+raeke with joint failure opt"
    +> flag "-semimcfvlb" no_arg ~doc:" run semi mcf+vlb"
    +> flag "-spf" no_arg ~doc:" run spf"
    +> flag "-vlb" no_arg ~doc:" run vlb"
    +> flag "-all" no_arg ~doc:" run all schemes"
    +> flag "-deloop" no_arg ~doc:" remove loops in paths"
    +> flag "-limittest" no_arg ~doc:" test at what scale congestion loss starts "
    +> flag "-robust" no_arg ~doc:" perform robustness test - fail all combinations of fail-num links"
    +> flag "-scalesyn" no_arg ~doc:" scale synthetic demands to achieve max congestion 1"
    +> flag "-vulnerability" no_arg ~doc:" perform path vulnerability test "
    +> flag "-fail-num" (optional_with_default 1 int) ~doc:" number of links to fail"
    +> flag "-fail-time" (optional_with_default (Int.max_value/100) int)
      ~doc:" simulation time to introduce failure at"
    +> flag "-lr-delay" (optional_with_default (Int.max_value/100) int)
      ~doc:" delay between failure and local recovery"
    +> flag "-gr-delay" (optional_with_default (Int.max_value/100) int)
      ~doc:" delay between failure and global recovery"
    +> flag "-is-flash" no_arg ~doc:" simulate flash or not"
    +> flag "-flash-ba" (optional_with_default 0. float) ~doc:" fraction of total traffic to add as flash"
    +> flag "-flash-recover" no_arg ~doc:" perform local recovery for flash"
    +> flag "-simtime" (optional_with_default 500 int) ~doc:" time steps to simulate each TM"
    +> flag "-budget" (optional_with_default (Int.max_value/100) int) ~doc:" max paths between each pair of hosts"
    +> flag "-scale" (optional_with_default 1. float) ~doc:" scale demands by this factor"
    +> flag "-out" (optional string) ~doc:" name of directory in expData to store results"
    +> flag "-appendout" no_arg ~doc:" append to results file instead of over-writing"
    +> flag "-rseed" (optional int) ~doc:" seed to initialize PRNG"
    +> flag "-num-tms" (optional int) ~doc:" number of TMs (-robust overrides this)"
    +> flag "-rtt-file" (optional string) ~doc:" file containing RTT values to be used as edge weights"
    +> flag "-gurobi-method" (optional_with_default (-1) int)
      ~doc:" solver method used for Gurobi. -1=automatic, 0=primal simplex, 1=dual simplex, 2=barrier, 3=concurrent, 4=deterministic concurrent."
    +> anon ("topology-file" %: string)
    +> anon ("demand-file" %: string)
    +> anon ("predict-file" %: string)
    +> anon ("host-file" %: string)
  ) (fun
    (ac:bool)
    (akecmp:bool)
    (akksp:bool)
    (akmcf:bool)
    (akraeke:bool)
    (akvlb:bool)
    (ecmp:bool)
    (edksp:bool)
    (ffc:bool)
    (ffced:bool)
    (ksp:bool)
    (mcf:bool)
    (mwmcf:bool)
    (optimalmcf:bool)
    (raeke:bool)
    (semimcfac:bool)
    (semimcfecmp:bool)
    (semimcfedksp:bool)
    (semimcfksp:bool)
    (semimcfkspft:bool)
    (semimcfmcf:bool)
    (semimcfmcfenv:bool)
    (semimcfmcfftenv:bool)
    (semimcfraeke:bool)
    (semimcfraekeft:bool)
    (semimcfvlb:bool)
    (spf:bool)
    (vlb:bool)
    (all:bool)
    (deloop:bool)
    (limittest:bool)
    (robust:bool)
    (scalesyn:bool)
    (vulnerability:bool)
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
    (appendout:bool)
    (rseed:int option)
    (num_tms:int option)
    (rtt_file:string option)
    (grb_method:int)
    (topology_file:string)
    (demand_file:string)
    (predict_file:string)
    (host_file:string) () ->
      let algorithms =
        List.filter_map ~f:(fun x -> x)
         [ if ac || all         then Some Ac          else None
         ; if akecmp            then Some AkEcmp      else None
         ; if akksp             then Some AkKsp       else None
         ; if akmcf             then Some AkMcf       else None
         ; if akraeke           then Some AkRaeke     else None
         ; if akvlb             then Some AkVlb       else None
         ; if ecmp || all       then Some Ecmp        else None
         ; if edksp || all      then Some Edksp       else None
         ; if ffc || all        then Some Ffc         else None
         ; if ffced || all      then Some Ffced       else None
         ; if ksp || all        then Some Ksp         else None
         ; if mcf || all        then Some Mcf         else None
         ; if mwmcf             then Some MwMcf       else None
         ; if optimalmcf || all then Some OptimalMcf  else None
         ; if raeke || all      then Some Raeke       else None
         ; if semimcfac || all        then Some SemiMcfAc     else None
         ; if semimcfecmp || all      then Some SemiMcfEcmp     else None
         ; if semimcfedksp || all     then Some SemiMcfEdksp      else None
         ; if semimcfksp || all       then Some SemiMcfKsp      else None
         ; if semimcfkspft            then Some SemiMcfKspFT    else None
         ; if semimcfmcf              then Some SemiMcfMcf  else None
         ; if semimcfmcfenv || all    then Some SemiMcfMcfEnv   else None
         ; if semimcfmcfftenv || all  then Some SemiMcfMcfFTEnv else None
         ; if semimcfraeke || all     then Some SemiMcfRaeke    else None
         ; if semimcfraekeft || all   then Some SemiMcfRaekeFT  else None
         ; if semimcfvlb || all       then Some SemiMcfVlb      else None
         ; if spf || all        then Some Spf         else None
         ; if vlb || all        then Some Vlb         else None ] in
      let syn_scale =
        if scalesyn then
          calculate_syn_scale topology_file demand_file host_file
        else 1.0 in
      let tot_scale = scale *. syn_scale in
      Printf.printf "Scale factor: %f\n\n" tot_scale;
      ExperimentalData.append_out := appendout;
      Kulfi_Globals.deloop := deloop;
      Kulfi_Globals.tm_sim_iters  := simtime;
      Kulfi_Globals.flash_recover := flash_recover;
      Kulfi_Globals.gurobi_method := grb_method;
      Kulfi_Globals.budget        := budget;
      Kulfi_Globals.failure_time  := fail_time;
      Kulfi_Globals.rand_seed     := rseed;
      Kulfi_Globals.local_recovery_delay  := lr_delay;
      Kulfi_Globals.global_recovery_delay := gr_delay;
      Kulfi_Globals.ffc_max_link_failures :=
        max fail_num !(Kulfi_Globals.ffc_max_link_failures);
      if robust then
        Kulfi_Globals.failure_time  := 0;
      if limittest then
        compare_scaling_limit algorithms num_tms topology_file demand_file
          host_file rtt_file out ()
      else
        simulate algorithms topology_file demand_file predict_file host_file
          num_tms robust vulnerability tot_scale fail_num is_flash flash_ba
          rtt_file out ())

let main = Command.run command

let _ = main
