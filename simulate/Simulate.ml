open Core.Std
open Frenetic_Network
open Net      
open Kulfi_Types
open Kulfi_Routing
open RunningStat
open ExperimentalData 
open AutoTimer

module Make(Solver:Kulfi_Routing.Algorithm) = struct
  let solve = Solver.solve 
end
       
type solver_type = | Mcf | Vlb | Ecmp | Spf | Ak | NaS (*not a solver*)
let solver_mode = ref NaS
let topo_string = ref ""
let iterations = ref 0
let usage = Printf.sprintf "%s -s [solver] -t [topology] -i [iterations]\n" Sys.argv.(0)
let solver_to_string (s:solver_type) : string =
  match s with 
  | Mcf -> "mcf" 
  | Vlb -> "vlb" 
  | Ecmp -> "ecmp"
  | Spf -> "spf" 
  | Ak -> "ak" 
  | _ -> raise (Arg.Bad("Unknown solver"))

let choose_solver = function
  | "mcf" -> solver_mode := Mcf
  | "vlb" -> solver_mode := Vlb
  | "ecmp" -> solver_mode := Ecmp
  | "spf" -> solver_mode := Spf
  | "ak" -> solver_mode := Ak
  | _ -> raise (Arg.Bad("Unknown solver"))

let speclist = [
  ("-s", Arg.Symbol (["mcf"; "vlb"; "ecmp"; "spf"; "ak"], 
		    choose_solver), ": set solver strategy");
  ("-t", Arg.Set_string topo_string, ": set topology file");
  ("-i", Arg.Set_int iterations, ": set number of iterations");
]

let missing_args () = match (!solver_mode, !topo_string, !iterations) with
  | (NaS,_,_) -> true
  | (_,"",_) -> true
  | (_,_,0) -> true
  | _ -> false
	
let select_algorithm solver = match solver with
  | Mcf -> Kulfi_Routing.Mcf.solve
  | Vlb -> Kulfi_Routing.Mcf.solve
  | Ecmp -> Kulfi_Routing.Mcf.solve
  | Spf -> Kulfi_Routing.Mcf.solve
  | Ak -> Kulfi_Routing.Mcf.solve
  | _ -> assert false

(*		
let make_experiment () : experimental_data =
  let d = make_data "Iteratives Vs Time" in
  let d' = add_field d "solver" in
  let d'' = add_field d' "iteration" in
  let d''' = add_field d'' "time" in
  let d'''' = add_field d''' "time-dev" in
  d''''

let add_record (d:experimental_data) (s:string) (i:float) (t:float) (dev:float) : experimental_data =
  let d' = add_field_value d "solver" s in
  let d'' = add_field_value d' "iteration" i in
  let d''' = add_field_value d'' "time" t in
  let d'''' = add_field_value d''' "time-dev" dev in
  d''''
 *)

type iter_vs_time = {
  iteration : int ;
  time : float;
  time_dev : float;
}
		
let main =
  Arg.parse speclist print_endline usage;
  if (missing_args ()) then Printf.printf "%s" usage
  else
    begin
      Printf.printf "Kulfi Simulate";
      let topo = Parse.from_dotfile !topo_string in
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
	  let solve = select_algorithm !solver_mode in
	  let data = ref (make_data "Iteratives Vs Time") in
	  let rec loop n = 
	    if n > !iterations then
	      ()
	    else
	      begin		
		at := start !at ;
		let _ = solve topo pairs SrcDstMap.empty in 
		at := stop !at ;
		times := push !times (get_time_in_seconds !at) ;
		loop (n+1)
	      end
	  in
	  loop 1;
	  Printf.printf "# solver mean stddev\n";
	  Printf.printf "%s\t%d\t%f\t%f\n"
			(solver_to_string !solver_mode)
			(!iterations)
			(get_mean !times)
			(get_standard_deviation !times);
	  data := add_record !data (solver_to_string !solver_mode)
			     {iteration = !iterations;
			      time=(get_mean !times);
			      time_dev=(get_standard_deviation !times); };
	  Printf.printf "%s" (to_string !data (fun r -> "foo"))
			     
    end
		  
let _ = main 

