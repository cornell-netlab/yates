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

type solver_type =
  | Mcf | MwMcf | Vlb | Ecmp | Ksp | Spf | Raeke
  | AkMcf | AkVlb | AkRaeke | AkEcmp | AkKsp
  | SemiMcfMcf | SemiMcfVlb | SemiMcfRaeke | SemiMcfEcmp | SemiMcfKsp

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
  | SemiMcfVlb -> "semimcfvlb"
  | SemiMcfRaeke -> "semimcfraeke"
  | SemiMcfEcmp -> "semimcfecmp"
  | SemiMcfKsp -> "semimcfksp"

let select_algorithm solver = match solver with
  | Mcf -> Kulfi_Routing.Mcf.solve
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
  | SemiMcfVlb
  | SemiMcfRaeke
  | SemiMcfKsp
  | SemiMcfEcmp -> Kulfi_Routing.SemiMcf.solve

let congestion_of_paths (s:scheme) (t:topology) (d:demands) : (float EdgeMap.t) =
  let sent_on_each_edge =
    SrcDstMap.fold
      s
      ~init:EdgeMap.empty
      ~f:(fun ~key:(src,dst) ~data:paths acc ->
          PathMap.fold
	    paths
            ~init:acc
            ~f:(fun ~key:path ~data:prob acc ->
                List.fold_left
		  path
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

let get_path_prob_demand_map (s:scheme) (d:demands) =
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


let simulate_fair_share (s:scheme) (t:topology) (d:demands) =
  (* Printf.printf "%s\n" (dump_scheme t s); *)
  let edge_paths_map = (* Store which paths go through an edge *)
    SrcDstMap.fold
      s
      ~init:EdgeMap.empty
      ~f:(fun ~key:(src,dst) ~data:paths acc ->
          PathMap.fold
	        paths
            ~init:acc
            ~f:(fun ~key:path ~data:prob acc ->
                List.fold_left
		            path
                  ~init:acc
                  ~f:(fun acc e ->
                      match EdgeMap.find acc e with
                      | None -> EdgeMap.add ~key:e ~data:([path]) acc
                      | Some e_paths ->
                          let uniq_paths = if (List.mem e_paths path) then e_paths
                                else (path::e_paths) in
                          EdgeMap.add ~key:e ~data:uniq_paths acc))) in
  let path_prob_map = get_path_prob_demand_map s d in
  (* debug prints *)
  (*
  let _ = EdgeMap.iter edge_paths_map
    ~f:(fun ~key:e ~data:paths->
      Printf.printf "\n%s\n" (dump_edges t [e]);
      List.iter paths
        ~f:(fun path ->
          let prob,demand = PathMap.find_exn path_prob_map path in
          Printf.printf "[%s] %f %f\n" (dump_edges t path) prob demand)) in
  *)
  let expected_flow_per_edge =
    EdgeMap.fold
    ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:paths acc ->
       List.fold_left paths
        ~init:acc
        ~f:(fun acc path ->
          let prob,demand = PathMap.find_exn path_prob_map path in
          let existing_flow = match EdgeMap.find acc e with
                              | None -> 0.0
                              | Some x -> x in
          EdgeMap.add ~key:e ~data:(existing_flow +. (prob *. demand)) acc))
    edge_paths_map in
  let expected_congestion_per_edge = EdgeMap.fold
    ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:amount_sent acc ->
        EdgeMap.add ~key:e ~data:(amount_sent /. (capacity_of_edge t e)) acc)
    expected_flow_per_edge in

  (* Test with previous version for sanity check *)
  (*
	let test_congestions = (congestion_of_paths s t d) in
  EdgeMap.iter test_congestions
    ~f:(fun ~key:e ~data:c ->
      let c' = EdgeMap.find_exn expected_congestion_per_edge e in
      if (c -. c' > 0.01 || c' -. c > 0.01) then failwith "New congestion values differ too much");
  *)
  let rec fair_share_at_edge (e:edge) (spare_cap:float) (sat_paths) (path_share_map) =
    let paths = EdgeMap.find_exn edge_paths_map e in
    let n_unsat_paths = (List.length paths)  - (List.length sat_paths) in
    if n_unsat_paths = 0 then path_share_map else
    let path_fairshare, new_sat_paths = List.fold_left paths
      ~init:(PathMap.empty,sat_paths)
      ~f:(fun acc path -> 
        let acc_path_share_map, acc_sat_paths = acc in
        let prob,sd_demand = PathMap.find_exn path_prob_map path in 
        let req_share = prob *. sd_demand in
        let path_allocated_share = match PathMap.find path_share_map path with
                                    | None -> 0.0
                                    | Some x -> x in
        let new_path_share = if List.mem acc_sat_paths path then path_allocated_share 
                      else min req_share (path_allocated_share +. (spare_cap /. (Float.of_int n_unsat_paths))) in
        let acc_sat_paths = if (new_path_share < req_share -. 0.001) || (List.mem acc_sat_paths path)
                            then acc_sat_paths 
                            else path::acc_sat_paths in
        (PathMap.add ~key:path ~data:new_path_share acc_path_share_map, acc_sat_paths)) in
    let residual_cap = PathMap.fold path_fairshare
      ~init:(capacity_of_edge t e)
      ~f:(fun ~key:path ~data:link_share acc -> acc -. link_share) in
    if residual_cap > 0.0 then fair_share_at_edge e residual_cap new_sat_paths path_fairshare
      else path_fairshare
  in
  expected_congestion_per_edge

let list_last l = match l with
  | hd::tl -> List.fold_left ~f:(fun _ x -> x) ~init:hd tl
  | []    -> failwith "no element"

let rec list_next l elem = match l with
  | hd::tl -> if hd = elem then List.hd tl
              else list_next tl elem
  | [] -> failwith "not found"

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

(* Simulate routing *)
let simulate_routing (s:scheme) (t:topology) (d:demands) =
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
  let net_diameter = 2 in
  let iterations = range 0 (num_iterations+net_diameter) in
  if local_debug then Printf.printf "%s\n" (dump_scheme t s);
  let _,sd_delivered,link_utils =
  List.fold_left
    iterations
    (* ingress link traffic, sd-delivered, traffic on each edge *)
    ~init:(EdgeMap.empty, SrcDstMap.empty, EdgeMap.empty)
    ~f:(fun (in_traffic, delivered, link_utils) iter ->
      (* begin iteration *)
      if local_debug then Printf.printf "--- Iteration : %d ---\n%!" iter;
      let path_prob_map = get_path_prob_demand_map s d in
      (* Add traffic at source of every path,
       * stop some time before end of simulation to allow packets in transit to reach *)
      (* next_iter_traffic : map edge -> in_traffic in next iter *)
      let next_iter_traffic = if num_iterations < iter then EdgeMap.empty
        else PathMap.fold path_prob_map
          ~init:EdgeMap.empty
          ~f:(fun ~key:path ~data:(prob,sd_demand) acc ->
              if path = [] then acc else
              let first_link = match List.hd path with
                                | None -> failwith "Empty path"
                                | Some x -> x in
              let sched_traf_first_link = match EdgeMap.find acc first_link with
                                          | None -> PathMap.empty
                                          | Some v -> v in
              let _ = match PathMap.find sched_traf_first_link path with
                      | None -> true
                      | Some x -> failwith "Scheduling duplicate flow at first link" in
              let traf_first_link = PathMap.add ~key:path ~data:(prob *. sd_demand) sched_traf_first_link in
              EdgeMap.add ~key:first_link ~data:traf_first_link acc) in
      (* Done generating traffic at source *)

      (* For each link, forward fair share of flows to next links or deliver to destination *)
      let next_iter_traffic, new_delivered, new_link_utils = EdgeMap.fold in_traffic
        ~init:(next_iter_traffic, delivered, link_utils)
        ~f:(fun ~key:e ~data:ingress_e acc ->
          let demand_on_link = PathMap.fold ingress_e
            ~init:0.0
            ~f:(fun ~key:_ ~data:flow_dem acc -> acc +. flow_dem) in
          if local_debug then Printf.printf "%s: %f / %f\n" (dump_edges t [e]) (demand_on_link /. 1e9) ((capacity_of_edge t e) /. 1e9);
          let fs_ingress_e = if demand_on_link > (capacity_of_edge t e) then
              (* calculate each flow's fair share *)
              let rec fair_share_at_edge (spare_cap:float) (sat_paths) (path_share_map) =
                if local_debug then Printf.printf "%f\n.%!" spare_cap;
                let n_unsat_paths = (PathMap.length ingress_e) - (List.length sat_paths) in
                if n_unsat_paths = 0 then path_share_map else
                let path_fairshare, new_sat_paths =
                  PathMap.fold
                  ingress_e
                  ~init:(PathMap.empty, sat_paths)
                  ~f:(fun ~key:path ~data:req_share acc ->
                    let (acc_path_share_map, acc_sat_paths) = acc in
                    let path_allocated_share = match PathMap.find path_share_map path with
                                                | None -> 0.0
                                                | Some x -> x in
                    let new_path_share = if List.mem acc_sat_paths path
                                  then path_allocated_share
                                  else min req_share (path_allocated_share +. (spare_cap /. (Float.of_int n_unsat_paths))) in
                    let acc_sat_paths = if (new_path_share < req_share -. 0.01)
                                                || (List.mem acc_sat_paths path)
                                        then acc_sat_paths
                                        else path::acc_sat_paths in
                    (PathMap.add ~key:path ~data:new_path_share acc_path_share_map, acc_sat_paths)) in
                let residual_cap =
                  PathMap.fold
                  path_fairshare
                  ~init:(capacity_of_edge t e)
                  ~f:(fun ~key:path ~data:link_share acc -> acc -. link_share) in
                if residual_cap > 0.01
                  then fair_share_at_edge residual_cap new_sat_paths path_fairshare
                  else path_fairshare in
              fair_share_at_edge (capacity_of_edge t e) [] PathMap.empty
              (* done calculating fair share *)
              else ingress_e in

          PathMap.fold fs_ingress_e
          ~init:acc
          ~f:(fun ~key:path ~data:flow_fair_share acc ->
            let (nit,dlvd,lutil) = acc in
            let next_link_opt = list_next path e in
            match next_link_opt with
            | None -> (* End of path, deliver traffic to dst *)
                let (src,dst) = match get_src_dst_for_path path with
                                | None -> failwith "Empty path"
                                | Some x -> x in
                let prev_delivered = match SrcDstMap.find dlvd (src,dst) with
                                      | None -> 0.0
                                      | Some x -> x in
                let prev_lutil_link = match EdgeMap.find lutil e with
                              | None -> 0.0
                              | Some x -> x in
                let new_dlvd = SrcDstMap.add ~key:(src,dst) ~data:(prev_delivered +. flow_fair_share) dlvd in
                let new_lutil = EdgeMap.add ~key:e ~data:(prev_lutil_link +. flow_fair_share) lutil in
                (nit, new_dlvd, new_lutil)
            | Some next_link -> (* Forward traffic to next link *)
                let sched_traf_next_link = match EdgeMap.find nit next_link with
                    | None -> PathMap.empty
                    | Some v -> v in
                let _ = match PathMap.find sched_traf_next_link path with
                    | None -> true
                    | Some x -> Printf.printf "%s" (dump_edges t path); failwith "Scheduling duplicate flow at next link" in
                let traf_next_link = PathMap.add ~key:path ~data:flow_fair_share sched_traf_next_link in
                let new_nit = EdgeMap.add ~key:next_link ~data:traf_next_link nit in
                let prev_lutil_link = match EdgeMap.find lutil e with
                              | None -> 0.0
                              | Some x -> x in
                let new_lutil = EdgeMap.add ~key:e ~data:(prev_lutil_link +. flow_fair_share) lutil in
                (new_nit, dlvd, new_lutil))) in
      (* Done forwarding *)
      (*Dump state for debugging *)
      if local_debug then
          EdgeMap.iter next_iter_traffic
            ~f:(fun ~key:e ~data:dem ->
              Printf.printf "%s\n" (dump_edges t [e]);
              PathMap.iter dem
              ~f:(fun ~key:path ~data:d -> Printf.printf "%s\t%f\n" (dump_edges t path) d));
      if local_debug then SrcDstMap.iter new_delivered
            ~f:(fun ~key:(src,dst) ~data:delvd ->
              Printf.printf "%s %s\t%f\n" (Node.name (Net.Topology.vertex_to_label t src))
            (Node.name (Net.Topology.vertex_to_label t dst)) delvd);
      (* state carried over to next iter *)
      (next_iter_traffic, new_delivered, new_link_utils))
      (* end iteration *) in
  let tput = SrcDstMap.fold sd_delivered
    ~init:SrcDstMap.empty
    ~f:(fun ~key:sd ~data:dlvd acc ->
      SrcDstMap.add ~key:sd ~data:(dlvd /. (Float.of_int num_iterations)) acc) in
  let congestions = EdgeMap.fold link_utils
    ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:util acc ->
      EdgeMap.add ~key:e ~data:(util /. (Float.of_int (num_iterations)) /. (capacity_of_edge t e)) acc) in
  tput, congestions

let is_int v =
  let p = (Float.modf v) in
  let f = Float.Parts.fractional p in
  let c = Float.classify f in
  c = Float.Class.Zero

(* assumes l is sorted *)
let kth_percentile (l:float list) (k:float) : float =
  let n = List.length l in
  let x = (Float.of_int n) *. k in
  (*Printf.printf "%f / %d\n" x n;*)
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

let init_mcf_demands (topo:topology) : demands =
  let host_set = VertexSet.filter (Topology.vertexes topo)
                                  ~f:(fun v ->
                                      let label = Topology.vertex_to_label topo v in
                                      Node.device label = Node.Host) in
  let hs = Topology.VertexSet.elements host_set in
  List.fold_left
    hs
    ~init:SrcDstMap.empty
    ~f:(fun acc u ->
	List.fold_left
	  hs
	  ~init:acc
	  ~f:(fun acc v ->
	      let r = if u = v then 0.0 else 500000.0 in
	      SrcDstMap.add acc ~key:(u,v) ~data:r))

let get_mean_congestion (l:float list) =
  (List.fold_left ~init:0. ~f:( +. )  l) /. (Float.of_int (List.length l))

(*  assume that flow is fractionally split in the proportions indicated by the probabilities. *)
let get_max_congestion (congestions:float list) : float =
  List.fold_left ~init:Float.nan ~f:(fun a acc -> Float.max_inan a acc) congestions

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

let get_num_paths (s:scheme) : float =
  let count = SrcDstMap.fold
		~init:0
		~f:(fun ~key:_ ~data:d acc ->
		    acc + (PathMap.length d))
		s in
  Float.of_int count

let get_total_tput tput : float =
  SrcDstMap.fold
  tput
  ~init:0.0
  ~f:(fun ~key:_ ~data:delivered acc -> acc +. delivered)

let initial_scheme algorithm topo : scheme =
  match algorithm with
  | SemiMcfMcf
  | AkMcf ->
     let d = init_mcf_demands topo in
     Kulfi_Routing.Mcf.solve topo d SrcDstMap.empty
  | SemiMcfVlb
  | AkVlb ->
     Kulfi_Routing.Vlb.solve topo SrcDstMap.empty SrcDstMap.empty
  | SemiMcfRaeke
  | AkRaeke ->
     Kulfi_Routing.Raeke.solve topo SrcDstMap.empty SrcDstMap.empty
  | SemiMcfEcmp
  | AkEcmp ->
     Kulfi_Routing.Ecmp.solve topo SrcDstMap.empty SrcDstMap.empty
  | SemiMcfKsp
  | AkKsp ->
     Kulfi_Routing.Ksp.solve topo SrcDstMap.empty SrcDstMap.empty
  | _ -> SrcDstMap.empty


let simulate
    (spec_solvers:solver_type list)
	     (topology_file:string)
	     (demand_file:string)
	     (predict_file:string)
	     (host_file:string)
	     (iterations:int)
             (scale:float)
             () : unit =

  (* Do some error checking on input *)

  let demand_lines_length = List.length (In_channel.read_lines demand_file) in
  let predict_lines_length = List.length (In_channel.read_lines predict_file) in

  ignore (if (demand_lines_length < iterations) then failwith "Iterations greater than demand file length" else());
  ignore (if (predict_lines_length < iterations) then failwith "Iterations greater than predict file length" else());


  let topo = Parse.from_dotfile topology_file in
  let host_set = VertexSet.filter (Topology.vertexes topo)
				  ~f:(fun v ->
				      let label = Topology.vertex_to_label topo v in
				      Node.device label = Node.Host) in

  Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
  Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);
  let at = make_auto_timer () in


  let time_data = make_data "Iteratives Vs Time" in
  let churn_data = make_data "Churn Vs Time" in
  let edge_congestion_data = make_data "Edge Congestion Vs Time" in
  let edge_exp_congestion_data = make_data "Edge Expected Congestion Vs Time" in
  let num_paths_data = make_data "Num. Paths Vs Time" in
  let max_congestion_data = make_data "Max Congestion Vs Time" in
  let mean_congestion_data = make_data "Mean Congestion Vs Time" in
  let max_exp_congestion_data = make_data "Max Expected Congestion Vs Time" in
  let mean_exp_congestion_data = make_data "Mean Expected Congestion Vs Time" in
  let total_tput_data = make_data "Total Throughput vs Time" in
  let percentiles = [0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 0.95] in
  let create_percentile_data (metric:string) =
    List.fold_left
      percentiles
      ~init:[]
      ~f:(fun acc p ->
	  let i = Int.of_float (p *. 100.) in
	  let s = Printf.sprintf "%d-th %s Vs Time" i metric in
	  (make_data s)::acc ) in

  let percentile_data = create_percentile_data "Congestion" in
  let exp_percentile_data = create_percentile_data "Expected Congestion" in

  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let is = range 0 iterations in


  List.iter
    spec_solvers
    ~f:(fun algorithm ->

	(* TODO(rjs): Raeke mutates the topology. As a fast fix, I'll just create
	 a new copy of topology for every algorithm. A better fix would be to understand
	 Raeke and ensure that it mutates a copy of the topology, not the actual
	 topology *)
	let topo = Parse.from_dotfile topology_file in

	let solve = select_algorithm algorithm in
	Printf.printf "Iter: %s\n%!" (solver_to_string algorithm);
	let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
	let (predict_host_map, predict_ic) = open_demands predict_file host_file topo in

	(* we may need to initialize the scheme, and advance both traffic files *)
	let start_scheme = initial_scheme algorithm topo in

	ignore (
	    List.fold_left
	      is (* 0..iterations *)
	      ~init:start_scheme
	      ~f:(fun scheme n ->

		  (* get the next demand *)
		  let actual = next_demand ~scale:scale actual_ic actual_host_map in
		  let predict = next_demand ~scale:scale predict_ic predict_host_map in

		  (* solve *)
		  start at;
		  let scheme' = solve topo predict scheme in
		  stop at;

		  let exp_congestions = (congestion_of_paths scheme' topo actual) in
		  let list_of_exp_congestions = List.map ~f:snd (EdgeMap.to_alist exp_congestions) in
		  let sorted_exp_congestions = List.sort ~cmp:(Float.compare) list_of_exp_congestions in

		  let tput,congestions = (simulate_routing scheme' topo actual) in
		  let list_of_congestions = List.map ~f:snd (EdgeMap.to_alist congestions) in
		  let sorted_congestions = List.sort ~cmp:(Float.compare) list_of_congestions in

		  (* record *)
		  let tm = (get_time_in_seconds at) in
		  let ch = (get_churn scheme' scheme) in
		  let np = (get_num_paths scheme') in
		  let cmax = (get_max_congestion list_of_congestions) in
		  let cmean = (get_mean_congestion list_of_congestions) in
		  let expcmax = (get_max_congestion list_of_exp_congestions) in
		  let expcmean = (get_mean_congestion list_of_exp_congestions) in
      let total_tput = (get_total_tput tput) in

		  let percentile_values sort_cong =
		    List.fold_left
		      percentiles
		      ~init:[]
		      ~f:(fun acc p ->
			  (kth_percentile sort_cong p)::acc ) in

		  let sname = (solver_to_string algorithm) in
		  add_record time_data sname {iteration = n; time=tm; time_dev=0.0; };
		  add_record churn_data sname {iteration = n; churn=ch; churn_dev=0.0; };
		  add_record num_paths_data sname {iteration = n; num_paths=np; num_paths_dev=0.0; };
		  add_record max_congestion_data sname {iteration = n; congestion=cmax; congestion_dev=0.0; };
		  add_record mean_congestion_data sname {iteration = n; congestion=cmean; congestion_dev=0.0; };
		  add_record max_exp_congestion_data sname {iteration = n; congestion=expcmax; congestion_dev=0.0; };
		  add_record mean_exp_congestion_data sname {iteration = n; congestion=expcmean; congestion_dev=0.0; };
		  add_record edge_congestion_data sname {iteration = n; edge_congestions=congestions; };
		  add_record edge_exp_congestion_data sname {iteration = n; edge_congestions=exp_congestions; };
		  add_record total_tput_data sname {iteration = n; throughput=total_tput; throughput_dev=0.0; };
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

  (* Store results in a directory name = topology name in expData *)
  let split_dot_file_list = String.split_on_chars topology_file ~on:['/';'.'] in
  let suffix =List.nth split_dot_file_list (List.length split_dot_file_list -2) in
  let realsuffix= match suffix with Some x -> x | _-> "" in
  let dir = "./expData/" ^ realsuffix ^ "/" in

  to_file dir "ChurnVsIterations.dat" churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string;
  to_file dir "NumPathsVsIterations.dat" num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string;
  to_file dir "TimeVsIterations.dat" time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string;
  to_file dir "MaxCongestionVsIterations.dat" max_congestion_data "# solver\titer\tmax-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "MeanCongestionVsIterations.dat" mean_congestion_data "# solver\titer\tmean-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "MaxExpCongestionVsIterations.dat" max_exp_congestion_data "# solver\titer\tmax-exp-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "MeanExpCongestionVsIterations.dat" mean_exp_congestion_data "# solver\titer\tmean-exp-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "TotalThroughputVsIterations.dat" total_tput_data "# solver\titer\ttotal-throughput\tstddev" iter_vs_throughput_to_string;

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

  Printf.printf "%s" (to_string time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string);
  Printf.printf "%s" (to_string churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string);
  Printf.printf "%s" (to_string max_congestion_data "# solver\titer\tmax-congestion\tstddev" iter_vs_congestion_to_string);
  Printf.printf "%s" (to_string total_tput_data "# solver\titer\ttotal-throughput\tstddev" iter_vs_throughput_to_string);
  Printf.printf "%s" (to_string num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string)


(* For synthetic demands based on Abilene, scale them to current topology by multiplying by X/mcf_congestion,
   where X is the max congestion we expect to get when run with the new demands *)
let calculate_syn_scale (deloop:bool) (topology:string) =
  let topo = Parse.from_dotfile topology in
  let host_set = VertexSet.filter (Topology.vertexes topo)
                                  ~f:(fun v ->
                                      let label = Topology.vertex_to_label topo v in
                                      Node.device label = Node.Host) in
  let hs = Topology.VertexSet.elements host_set in
  let demands =
    List.fold_left
      hs
      ~init:SrcDstMap.empty
      ~f:(fun acc u ->
	  List.fold_left
	    hs
	    ~init:acc
	    ~f:(fun acc v ->
                let num_hosts = List.length hs in
		let r = if u = v then 0.0 else 22986934.0 /. Float.of_int(num_hosts * num_hosts) in
		SrcDstMap.add acc ~key:(u,v) ~data:r)) in
  let s=SrcDstMap.empty in
  let s2 =Kulfi_Mcf.solve topo demands s in
  let expected_congestions = congestion_of_paths s2 topo demands in
  let list_of_congestions = List.map ~f:snd (EdgeMap.to_alist expected_congestions) in
  let cmax = (get_max_congestion list_of_congestions) in
  let scale_factor = 0.4/.cmax in
  Printf.printf "%f\n\n" (scale_factor);
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
    +> flag "-semimcfvlb" no_arg ~doc:" run semi mcf+vlb"
    +> flag "-semimcfraeke" no_arg ~doc:" run semi mcf+raeke"
    +> flag "-semimcfecmp" no_arg ~doc:" run semi mcf+ecmp"
    +> flag "-semimcfksp" no_arg ~doc:" run semi mcf+ksp"
    +> flag "-raeke" no_arg ~doc:" run raeke"
    +> flag "-all" no_arg ~doc:" run all schemes"
    +> flag "-scalesyn" no_arg ~doc:" scale synthetic demands to achieve max congestion 1"
    +> flag "-deloop" no_arg ~doc:" remove loops in paths"
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
	 (semimcfvlb:bool)
	 (semimcfraeke:bool)
	 (semimcfecmp:bool)
	 (semimcfksp:bool)
	 (raeke:bool)
	 (all:bool)
         (scalesyn:bool)
         (deloop:bool)
	 (topology_file:string)
	 (demand_file:string)
	 (predict_file:string)
	 (host_file:string)
	 (iterations:int) () ->
     let algorithms =
       List.filter_map
         ~f:(fun x -> x)
         [ if mcf || all then Some Mcf else None
	 ; if mwmcf || all then Some MwMcf else None
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
         ; if semimcfmcf || all then Some SemiMcfMcf else None
	 ; if semimcfecmp || all then Some SemiMcfEcmp else None
	 ; if semimcfksp || all then Some SemiMcfKsp else None
	 ; if semimcfvlb || all then Some SemiMcfVlb else None
	 ; if semimcfraeke || all then Some SemiMcfRaeke else None ] in
     let scale = if scalesyn then calculate_syn_scale deloop topology_file else 1.0 in
     Kulfi_Globals.deloop := deloop;
     simulate algorithms topology_file demand_file predict_file host_file iterations scale ()
  )

let main = Command.run command

let _ = main

