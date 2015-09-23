open Core.Std
open Frenetic_Network
open Net      
open ExperimentalData 
open Kulfi_Types
       
let is_int v =
  let p = (Float.modf v) in
  let f = Float.Parts.fractional p in
  let c = Float.classify f in
  c = Float.Class.Zero
	
(* assumes l is sorted *)
let kth_percentile (l:int list) (k:float) : int =
  let n = List.length l in
  let x = (Float.of_int n) *. k in
  if is_int x then
    let i = Int.of_float (Float.round_up x) in
    let lhs = match (List.nth l i) with
      | Some f -> f
      | None -> assert false in
    if (i+1 >= (List.length l -1)) 
    then lhs 
    else
      let rhs = match List.nth l (i+1) with
        | Some f -> f
        | None -> assert false in
      ((lhs + rhs)/2)
  else    
    let i = Int.of_float x in
    match (List.nth l i) with
    | Some f -> f
    | None -> assert false

    
let get_mean (l:int list) =
  (List.fold_left ~init:0 ~f:( + )  l) / (List.length l)
		                    	 
let process (topology_file:string) () : unit =
  
  let topo = Parse.from_dotfile topology_file in
  let host_set = VertexSet.filter (Topology.vertexes topo)
				  ~f:(fun v ->
				      let label = Topology.vertex_to_label topo v in
				      Node.device label = Node.Host) in

  let num_hosts = Topology.VertexSet.length host_set in 
  let num_vertices = (Topology.num_vertexes topo) in
  let degrees =
    VertexSet.fold
      (Topology.vertexes topo)
      ~init:[]
      ~f:(fun acc v ->
	  let n = Topology.VertexSet.length (Topology.neighbors topo v) in
	  n::acc) in
  let sorted_degrees = List.sort ~cmp:(Int.compare) degrees in

  let c10 = (kth_percentile sorted_degrees 0.1) in
  let c20 = (kth_percentile sorted_degrees 0.2) in
  let c30 = (kth_percentile sorted_degrees 0.3) in
  let c40 = (kth_percentile sorted_degrees 0.4) in
  let c50 = (kth_percentile sorted_degrees 0.5) in
  let c60 = (kth_percentile sorted_degrees 0.6) in
  let c70 = (kth_percentile sorted_degrees 0.7) in
  let c80 = (kth_percentile sorted_degrees 0.8) in
  let c90 = (kth_percentile sorted_degrees 0.9) in
  let c95 = (kth_percentile sorted_degrees 0.95) in
  
  Printf.printf "%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n"
		(Filename.basename topology_file) num_vertices num_hosts
		c10 c20 c30 c40 c50 c60 c70 c80 c90 c95

		
let command =
  Command.basic
    ~summary:"Process topology file"
    Command.Spec.(
    empty
    +> anon ("topology-file" %: string)
  ) (fun (topology_file:string)
	 () ->     
     process topology_file () 
    )
    
let main = Command.run command
		       
let _ = main 

