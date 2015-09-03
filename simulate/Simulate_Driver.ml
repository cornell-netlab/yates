open Core.Std
open Frenetic_Network
open Net      
open Kulfi_Types
open Kulfi_Routing
open Simulate_Exps
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

let get_congestion (s:scheme) (t:topology) : float =
  0.0

let get_churn (old_scheme:scheme) (new_scheme:scheme) : float =
  0.0

let get_num_paths (s:scheme) : float =
  0.0
        
let simulate (spec_solvers:solver_type list) (topology_file:string) (iterations:int) () : unit =
  let topo = Parse.from_dotfile topology_file in
  let host_set = VertexSet.filter (Topology.vertexes topo)
				  ~f:(fun v ->
				      let label = Topology.vertex_to_label topo v in
				      Node.device label = Node.Host) in
  let hosts = Topology.VertexSet.elements host_set in
  let demand_matrix = Simulate_Demands.create_sparse hosts 0.1 100 in
  let pairs = Simulate_Demands.get_demands demand_matrix in
  Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
  Printf.printf "# pairs = %d\n" (List.length pairs);
  Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);
  let at = ref (make_auto_timer ()) in
  let times = ref (make_running_stat ()) in
  let churn = ref (make_running_stat ()) in
  let congestion = ref (make_running_stat ()) in
  let num_paths = ref (make_running_stat ()) in

  let time_data = ref (make_data "Iteratives Vs Time") in
  let churn_data = ref (make_data "Churn Vs Time") in
  let congestion_data = ref (make_data "Congestion Vs Time") in
  let num_paths_data = ref (make_data "Num. Paths Vs Time") in
  
  let rec outer algorithms = match algorithms with
    | [] -> ()
    | algorithm::rest ->
       let solve = select_algorithm algorithm in
       let rec inner n scheme = 
	 if n > iterations then
	   ()
	 else
	   begin
	     at := start !at ;
	     let scheme' = solve topo pairs scheme in 
	     at := stop !at ;
	     times := push !times (get_time_in_seconds !at) ;
	     churn := push !churn (get_churn scheme' scheme) ;	    
	     congestion := push !congestion (get_congestion scheme' topo) ;
	     num_paths := push !num_paths (get_num_paths scheme') ;

	     time_data := add_record !time_data (solver_to_string algorithm)
				     {iteration = n; time=(get_mean !times); time_dev=(get_standard_deviation !times); };
	     
	     churn_data := add_record !churn_data (solver_to_string algorithm)
				     {iteration = n; churn=(get_mean !churn); churn_dev=(get_standard_deviation !churn); };

	     congestion_data := add_record !congestion_data (solver_to_string algorithm)
				     {iteration = n; congestion=(get_mean !congestion); congestion_dev=(get_standard_deviation !congestion); };

	     num_paths_data := add_record !num_paths_data (solver_to_string algorithm)
				     {iteration = n; num_paths=(get_mean !num_paths); num_paths_dev=(get_standard_deviation !num_paths); };


	     inner (n+1) scheme'
	   end
       in
       inner 1 SrcDstMap.empty;
       outer rest
  in
  outer spec_solvers;
  let dir = "./expData/" in
  (* to_file dir "time.dat" !time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string; *)
  (* to_file dir "churn.dat" !churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string; *)
  (* to_file dir "congestion.dat" !congestion_data "# solver\titer\tcongestion\tstddev" iter_vs_congestion_to_string; *)
  (* to_file dir "num_paths.dat" !num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string; *)

  Printf.printf "%s" (to_string !time_data "# solver\titer\ttime\tstddev" iter_vs_time_to_string);
  Printf.printf "%s" (to_string !churn_data "# solver\titer\tchurn\tstddev" iter_vs_churn_to_string);
  Printf.printf "%s" (to_string !congestion_data "# solver\titer\tcongestion\tstddev" iter_vs_congestion_to_string);
  Printf.printf "%s" (to_string !num_paths_data "# solver\titer\tnum_paths\tstddev" iter_vs_num_paths_to_string)

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
     let algorithms =
       List.filter_map
         ~f:(fun x -> x)
         [ if mcf then Some Mcf else None
         ; if vlb then Some Vlb else None
         ; if ecmp then Some Ecmp else None
         ; if spf then Some Spf else None
         ; if ak then Some Ak else None ] in 
     simulate algorithms topology_file iterations () )

let main = Command.run command
 
let _ = main 

