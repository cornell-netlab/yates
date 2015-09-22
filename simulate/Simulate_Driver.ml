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

type solver_type =
  | Mcf | MwMcf | Vlb | Ecmp | Spf | Raeke
  | AkMcf | AkVlb | AkRaeke | AkEcmp
  | SmcfMcf | SmcfVlb | SmcfRaeke | SmcfEcmp
 
let solver_to_string (s:solver_type) : string =
  match s with 
  | Mcf -> "mcf" 
  | MwMcf -> "mwmcf"
  | Vlb -> "vlb" 
  | Ecmp -> "ecmp"
  | Spf -> "spf" 
  | Raeke -> "raeke" 	       
  | AkMcf -> "akmcf"
  | AkVlb -> "akvlb"
  | AkRaeke -> "akraeke"
  | AkEcmp -> "akecmp"
  | SmcfMcf -> "smcfmcf"
  | SmcfVlb -> "smcfvlb"
  | SmcfRaeke -> "smcfraeke"
  | SmcfEcmp -> "smcfecmp"
	       
let select_algorithm solver = match solver with
  | Mcf -> Kulfi_Routing.Mcf.solve
  | MwMcf -> Kulfi_Routing.MwMcf.solve
  | Vlb -> Kulfi_Routing.Vlb.solve
  | Ecmp -> Kulfi_Routing.Ecmp.solve
  | Spf -> Kulfi_Routing.Spf.solve
  | Raeke -> Kulfi_Routing.Raeke.solve
  | AkMcf 
  | AkVlb 
  | AkRaeke 
  | AkEcmp -> Kulfi_Routing.Ak.solve
  | SmcfMcf 
  | SmcfVlb 
  | SmcfRaeke
  | SmcfEcmp -> Kulfi_Routing.SemiMcf.solve 
	       
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

let is_int v =
  let p = (Float.modf v) in
  let f = Float.Parts.fractional p in
  let c = Float.classify f in
  c = Float.Class.Zero

(* assumes l is sorted *)
let kth_percentile (l:float list) (k:float) : float =
  let n = List.length l in
  let x = (Float.of_int n) *. k in
  if is_int x then
    let i = Int.of_float (Float.round_up x) in
    let lhs = match (List.nth l i) with
      | Some f -> f
      | None -> assert false in
    let rhs = match List.nth l (i+1) with
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


let initial_scheme algorithm topo aic ahm pic phm : scheme =
  match algorithm with
  | SmcfMcf 
  | AkMcf ->
     let _ = next_demand aic ahm in 
     let d = next_demand pic phm in 
     Kulfi_Routing.Mcf.solve topo d SrcDstMap.empty
  | SmcfVlb 
  | AkVlb ->
     let _ = next_demand aic ahm in 
     let d = next_demand pic phm in 
     Kulfi_Routing.Vlb.solve topo d SrcDstMap.empty
  | SmcfRaeke 
  | AkRaeke ->
     let _ = next_demand aic ahm in 
     let d = next_demand pic phm in 
     Kulfi_Routing.Raeke.solve topo d SrcDstMap.empty
  | SmcfEcmp 
  | AkEcmp ->
     let _ = next_demand aic ahm in 
     let d = next_demand pic phm in 
     Kulfi_Routing.Ecmp.solve topo d SrcDstMap.empty
  | _ -> SrcDstMap.empty

			
let simulate (spec_solvers:solver_type list)
	     (topology_file:string)
	     (demand_file:string)
	     (predict_file:string)
	     (host_file:string)
	     (iterations:int)
         (scale:float) () : unit =

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

  (* let hosts = Topology.VertexSet.elements host_set in *)

  Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
  Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);
  let at = make_auto_timer () in
  
  let time_data = make_data "Iteratives Vs Time" in
  let churn_data = make_data "Churn Vs Time" in
  let max_congestion_data = make_data "Max Congestion Vs Time" in
  let mean_congestion_data = make_data "Mean Congestion Vs Time" in
  let k10_congestion_data = make_data "10th Congestion Vs Time" in
  let k20_congestion_data = make_data "20th Congestion Vs Time" in
  let k30_congestion_data = make_data "30th Congestion Vs Time" in
  let k40_congestion_data = make_data "40th Congestion Vs Time" in
  let k50_congestion_data = make_data "50th Congestion Vs Time" in
  let k60_congestion_data = make_data "60th Congestion Vs Time" in
  let k70_congestion_data = make_data "70th Congestion Vs Time" in
  let k80_congestion_data = make_data "80th Congestion Vs Time" in
  let k90_congestion_data = make_data "90th Congestion Vs Time" in
  let k95_congestion_data = make_data "95th Congestion Vs Time" in

  let num_paths_data = make_data "Num. Paths Vs Time" in

  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let is = range 0 iterations in 


  List.iter
    spec_solvers
    ~f:(fun algorithm ->
	
	let solve = select_algorithm algorithm in
	
	let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
	let (predict_host_map, predict_ic) = open_demands predict_file host_file topo in

	(* we may need to initialize the scheme, and advance both traffic files *)
	let start_scheme = initial_scheme algorithm topo
					  actual_ic actual_host_map
					  predict_ic predict_host_map in
	
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

		  let congestions = (congestion_of_paths scheme' topo actual) in
		  let list_of_congestions = List.map ~f:snd (EdgeMap.to_alist congestions) in 
		  let sorted_congestions = List.sort ~cmp:(Float.compare) list_of_congestions in
		  
		  (* record *)
		  let tm = (get_time_in_seconds at) in
		  let ch = (get_churn scheme' scheme) in
		  let cmax = (get_max_congestion list_of_congestions) in
		  let cmean = (get_mean_congestion list_of_congestions) in
		  let c10 = (kth_percentile sorted_congestions 0.1) in
		  let c20 = (kth_percentile sorted_congestions 0.2) in
		  let c30 = (kth_percentile sorted_congestions 0.3) in
		  let c40 = (kth_percentile sorted_congestions 0.4) in
		  let c50 = (kth_percentile sorted_congestions 0.5) in
		  let c60 = (kth_percentile sorted_congestions 0.6) in
		  let c70 = (kth_percentile sorted_congestions 0.7) in
		  let c80 = (kth_percentile sorted_congestions 0.8) in
		  let c90 = (kth_percentile sorted_congestions 0.9) in
		  let c95 = (kth_percentile sorted_congestions 0.95) in
		  let np = (get_num_paths scheme') in

		  (* let k95_congestion_data = make_data "95th Congestion Vs Time" in *)
		  
		  add_record time_data (solver_to_string algorithm) {iteration = n; time=tm; time_dev=0.0; };	     
		  add_record churn_data (solver_to_string algorithm) {iteration = n; churn=ch; churn_dev=0.0; };
		  add_record num_paths_data (solver_to_string algorithm) {iteration = n; num_paths=np; num_paths_dev=0.0; };
		  add_record max_congestion_data (solver_to_string algorithm) {iteration = n; congestion=cmax; congestion_dev=0.0; };
		  add_record mean_congestion_data (solver_to_string algorithm) {iteration = n; congestion=cmean; congestion_dev=0.0; };		      
		  add_record k10_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c10; congestion_dev=0.0; };
		  add_record k20_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c20; congestion_dev=0.0; };
		  add_record k30_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c30; congestion_dev=0.0; };
		  add_record k40_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c40; congestion_dev=0.0; };
		  add_record k50_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c50; congestion_dev=0.0; };
		  add_record k60_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c60; congestion_dev=0.0; };
		  add_record k70_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c70; congestion_dev=0.0; };
		  add_record k80_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c80; congestion_dev=0.0; };
		  add_record k90_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c90; congestion_dev=0.0; };
		  add_record k95_congestion_data (solver_to_string algorithm) {iteration = n; congestion=c95; congestion_dev=0.0; };		      
		  scheme') );
	
	(* start at beginning of demands for next algorithm *)
	close_demands actual_ic;
	close_demands predict_ic;

       );
    
  let dir = "./expData/" in
  to_file dir "ChurnVsIterations.dat" churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string;
  to_file dir "NumPathsVsIterations.dat" num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string;
  to_file dir "TimeVsIterations.dat" time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string;  
  to_file dir "MaxCongestionVsIterations.dat" max_congestion_data "# solver\titer\tmax-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "MeanCongestionVsIterations.dat" mean_congestion_data "# solver\titer\tmean-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k10CongestionVsIterations.dat" k10_congestion_data "# solver\titer\t.10-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k20CongestionVsIterations.dat" k20_congestion_data "# solver\titer\t.20-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k30CongestionVsIterations.dat" k30_congestion_data "# solver\titer\t.30-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k40CongestionVsIterations.dat" k40_congestion_data "# solver\titer\t.40-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k50CongestionVsIterations.dat" k50_congestion_data "# solver\titer\t.50-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k60CongestionVsIterations.dat" k60_congestion_data "# solver\titer\t.60-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k70CongestionVsIterations.dat" k70_congestion_data "# solver\titer\t.70-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k80CongestionVsIterations.dat" k80_congestion_data "# solver\titer\t.80-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k90CongestionVsIterations.dat" k90_congestion_data "# solver\titer\t.90-congestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "k95CongestionVsIterations.dat" k95_congestion_data "# solver\titer\t.95-congestion\tstddev" iter_vs_congestion_to_string;
 
  Printf.printf "%s" (to_string time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string);
  Printf.printf "%s" (to_string churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string);
  Printf.printf "%s" (to_string max_congestion_data "# solver\titer\tmax-congestion\tstddev" iter_vs_congestion_to_string);
  Printf.printf "%s" (to_string num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string)




  (*"./data/topologies/zoo/Vinaren.dot" *)
let calculate_syn_scale (topology:string)=
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
		let r = if u = v then 0.0 else 2800000.0 in
		SrcDstMap.add acc ~key:(u,v) ~data:r)) in
  let s=SrcDstMap.empty in 
  let s2 =Kulfi_Mcf.solve topo demands s in 
  let congestions = congestion_of_paths s2 topo demands in 
  let list_of_congestions = List.map ~f:snd (EdgeMap.to_alist congestions) in 
  let cmax = (get_max_congestion list_of_congestions) in
      Printf.printf "%f\n\n" (1.0/.cmax);
      1.0/.cmax
		
let command =
  Command.basic
    ~summary:"Simulate run of routing strategies"
    Command.Spec.(
    empty
    +> flag "-mcf" no_arg ~doc:" run mcf"
    +> flag "-mwmcf" no_arg ~doc:" run mwmcf"
    +> flag "-vlb" no_arg ~doc:" run vlb"
    +> flag "-ecmp" no_arg ~doc:" run ecmp"
    +> flag "-spf" no_arg ~doc:" run spf"
    +> flag "-akmcf" no_arg ~doc:" run ak+mcf"
    +> flag "-akvlb" no_arg ~doc:" run ak+vlb"
    +> flag "-akraeke" no_arg ~doc:" run ak+raeke"
    +> flag "-akecmp" no_arg ~doc:" run ak+ecmp"
    +> flag "-smcfmcf" no_arg ~doc:" run semi mcf+mcf"
    +> flag "-smcfvlb" no_arg ~doc:" run semi mcf+vlb"
    +> flag "-smcfraeke" no_arg ~doc:" run semi mcf+raeke"
    +> flag "-smcfecmp" no_arg ~doc:" run semi mcf+ecmp"
    +> flag "-raeke" no_arg ~doc:" run raeke"
    +> flag "-all" no_arg ~doc:" run all schemes"
    +> flag "-scalesyn" no_arg ~doc:" use it in synthetic data simulation"
    +> anon ("topology-file" %: string)
    +> anon ("demand-file" %: string)
    +> anon ("predict-file" %: string)
    +> anon ("host-file" %: string)
    +> anon ("iterations" %: int)
  ) (fun (mcf:bool)
         (mwmcf:bool)
	 (vlb:bool)
	 (ecmp:bool)
	 (spf:bool)
	 (akmcf:bool)
	 (akvlb:bool)
	 (akraeke:bool)
	 (akecmp:bool)
	 (smcfmcf:bool)
	 (smcfvlb:bool)
	 (smcfraeke:bool)
	 (smcfecmp:bool)
	 (raeke:bool)
	 (all:bool)
     (scalesyn:bool)
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
         ; if spf || all then Some Spf else None
	 ; if akmcf || all then Some AkMcf else None
	 ; if akvlb || all then Some AkVlb else None
	 ; if akecmp || all then Some AkVlb else None
	 ; if akraeke || all then Some AkRaeke else None
         ; if raeke || all then Some Raeke else None 
         ; if smcfmcf || all then Some SmcfMcf else None
	 ; if smcfecmp || all then Some SmcfMcf else None
	 ; if smcfvlb || all then Some SmcfVlb else None
	 ; if smcfraeke || all then Some SmcfRaeke else None ] in 
     let scale = if scalesyn then calculate_syn_scale(topology_file) else 1.0 in
     simulate algorithms topology_file demand_file predict_file host_file iterations scale () 
  )

let main = Command.run command
		       
let _ = main 

