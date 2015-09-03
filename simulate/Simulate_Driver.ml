open Core.Std
open Frenetic_Network
open Net      
open Kulfi_Types
open Kulfi_Routing
open Simulate_Exps
open Simulate_Demands
open RunningStat
open ExperimentalData 
open AutoTimer
       
type solver_type = | Mcf | Vlb | Ecmp | Spf | Ak 

let solver_mode = ref Mcf

let solver_to_string (s:solver_type) : string =
  match s with 
  | Mcf -> "mcf" 
  | Vlb -> "vlb" 
  | Ecmp -> "ecmp"
  | Spf -> "spf" 
  | Ak -> "ak" 
		
let select_algorithm solver = match solver with
  | Mcf -> Kulfi_Routing.Mcf.solve
  | Vlb -> Kulfi_Routing.Vlb.solve
  | Ecmp -> Kulfi_Routing.Ecmp.solve
  | Spf -> Kulfi_Routing.Spf.solve
  | Ak -> Kulfi_Routing.Ak.solve



let congestion_of_paths (s:scheme) (t:topology) (d:demands) : (float EdgeMap.t) =
  let sent_on_each_edge = 
    SrcDstMap.fold
      ~init:EdgeMap.empty
      ~f:(fun ~key:(src,dst) ~data:paths acc ->
          PathMap.fold
            ~init:acc
            ~f:(fun ~key:path ~data:prob acc ->
                List.fold_left
                  ~init:acc
                  ~f:(fun acc e ->
                      let demand =
                        match SrcDstMap.find d (src,dst) with
                        | None -> 0.0
                        | Some x -> x in
                      match EdgeMap.find acc e with
                      | None -> EdgeMap.add ~key:e ~data:(demand *. prob) acc
                      | Some x ->  EdgeMap.add ~key:e ~data:((demand *. prob) +. x) acc) path)
            paths) s      
  in
  EdgeMap.fold
    ~init:EdgeMap.empty
    ~f:(fun ~key:e ~data:amount_sent acc ->
        EdgeMap.add ~key:e ~data:(amount_sent /. (capacity_of_edge t e)) acc) sent_on_each_edge 
    
    
(*  assume that flow is fractionally split in the proportions indicated by the probabilities. *)
let get_congestion (s:scheme) (t:topology) (d:demands) : float =
  let congestions = (congestion_of_paths s t d) in
  EdgeMap.fold ~init:Float.nan ~f:(fun ~key:e ~data:a acc -> Float.max_inan a acc) congestions

               
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
        
let simulate (spec_solvers:solver_type list) (topology_file:string) (iterations:int) () : unit =
  let topo = Parse.from_dotfile topology_file in
  let host_set = VertexSet.filter (Topology.vertexes topo)
				  ~f:(fun v ->
				      let label = Topology.vertex_to_label topo v in
				      Node.device label = Node.Host) in
  let hosts = Topology.VertexSet.elements host_set in
  let demand_matrix = create_sparse hosts 0.1 100 in
  let demands = demand_list_to_map (get_demands demand_matrix) in
  Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
  Printf.printf "# demands = %d\n" (SrcDstMap.length demands);
  Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);
  let at = make_auto_timer () in
  let times = make_running_stat () in
  let churn = make_running_stat () in
  let congestion = make_running_stat () in
  let num_paths = make_running_stat () in

  let time_data = make_data "Iteratives Vs Time" in
  let churn_data = make_data "Churn Vs Time" in
  let congestion_data = make_data "Congestion Vs Time" in
  let num_paths_data = make_data "Num. Paths Vs Time" in
  
  let rec outer algorithms = match algorithms with
    | [] -> ()
    | algorithm::rest ->
       let solve = select_algorithm algorithm in
       let rec inner n scheme = 
	 if n > iterations then
	   ()
	 else
	   begin		
	     start at;
	     let scheme' = solve topo demands scheme in 
	     stop at;
	     push times (get_time_in_seconds at);
	     push churn (get_churn scheme' scheme);	    
	     push congestion (get_congestion scheme' topo demands);
	     push num_paths (get_num_paths scheme');
	     add_record time_data (solver_to_string algorithm)
				     {iteration = n; time=(get_mean times); time_dev=(get_standard_deviation times); };	     
	     add_record churn_data (solver_to_string algorithm)
				     {iteration = n; churn=(get_mean churn); churn_dev=(get_standard_deviation churn); };
	     add_record congestion_data (solver_to_string algorithm)
				     {iteration = n; congestion=(get_mean congestion); congestion_dev=(get_standard_deviation congestion); };
	     add_record num_paths_data (solver_to_string algorithm)
				     {iteration = n; num_paths=(get_mean num_paths); num_paths_dev=(get_standard_deviation num_paths); };
	     inner (n+1) scheme'
	   end
       in
       inner 1 SrcDstMap.empty;
       outer rest
  in
  outer spec_solvers;
  
  let dir = "./expData/" in

  to_file dir "ChurnVsIterations.dat" churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string;
  to_file dir "CongestionVsIterations.dat" congestion_data "# solver\titer\tcongestion\tstddev" iter_vs_congestion_to_string;
  to_file dir "NumPathsVsIterations.dat" num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string;
  to_file dir "TimeVsIterations.dat" time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string;  
  
  Printf.printf "%s" (to_string time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string);
  Printf.printf "%s" (to_string churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string);
  Printf.printf "%s" (to_string congestion_data "# solver\titer\tcongestion\tstddev" iter_vs_congestion_to_string);
  Printf.printf "%s" (to_string num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string)
  
		
let command =
  Command.basic
    ~summary:"Simulate run of routing strategies"
    Command.Spec.(
    empty
    +> flag "-mcf" no_arg ~doc:" run mcf"
    +> flag "-vlb" no_arg ~doc:" run vlb"
    +> flag "-ecmp" no_arg ~doc:" run ecmp"
    +> flag "-spf" no_arg ~doc:" run spf"
    +> flag "-ak" no_arg ~doc:" run ak"
    +> anon ("filename" %: string)
    +> anon ("iterations" %: int)
  ) (fun (mcf:bool) (vlb:bool) (ecmp:bool) (spf:bool) (ak:bool) (topology_file:string) (iterations:int) () ->
     (* TODO(rjs) : how can I make this code uglier? *)
     let algorithms = 
       (if mcf then [Mcf] else [])@
	 (if vlb then [Vlb] else [])@
	   (if ecmp then [Ecmp] else [])@
	     (if spf then [Spf] else [])@
	       (if ak then [Ak] else []) in            
     simulate algorithms topology_file iterations () )

let main = Command.run command
 
let _ = main 

