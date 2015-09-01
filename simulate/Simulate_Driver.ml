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

  let data = ref (make_data "Iteratives Vs Time") in

  let rec outer algorithms = match algorithms with
    | [] -> ()
    | algorithm::rest ->
       let solve = select_algorithm algorithm in
       let rec inner n = 
	 if n > iterations then
	   ()
	 else
	   begin		
	     at := start !at ;
	     let _ = solve topo pairs SrcDstMap.empty in 
	     at := stop !at ;
	     times := push !times (get_time_in_seconds !at) ;

	     data := add_record !data (solver_to_string algorithm)
				{iteration = n;
				 time=(get_mean !times);
				 time_dev=(get_standard_deviation !times); };
	     
	     inner (n+1)
	   end
       in
       inner 1;
       outer rest
  in
  outer spec_solvers;
  Printf.printf "%s" (to_string !data "# solver\titer\ttime\tstddev" iter_vs_time_to_string)
		     
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

