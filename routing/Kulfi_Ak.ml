open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net

(* This implements the Algorithm in Greedy Distributed Optimization 
    of Multi-Commodity Flows by Awerbuch and Khandekar *)
       
let mu = ref Float.nan
let prev_scheme = ref SrcDstMap.empty

(* Make epsilon bigger for faster solving and worse approximation *)             
let epsilon = 0.5
                
let alpha (t:topology) : float = epsilon /. (40.0 *. (log (Float.of_int (Topology.num_edges t))))

let beta (t:topology) : float = (alpha t) *. (epsilon /. (log (Float.of_int (Topology.num_edges t))))

let f_umlaut (e:edge) (muu:float) (t:topology) = 
  let fm = Float.of_int (Topology.num_edges t) in
  (muu /. (log fm)) *. ((capacity_of_edge t e) /. (1.0 +. (beta t))) *.
  ((log (1.0 +. ((alpha t) /. 8.0))) /. (log 2.0))

let find_max_flow f = EdgeMap.fold ~init:0.0 ~f:(fun ~key:e ~data:r acc -> Float.max_inan acc r) f                     

let derivative_phi (e:edge) (muu:float) (x:float) (t:topology) : float =
  let fm = Float.of_int (Topology.num_edges t) in
  ((log fm) /. ((capacity_of_edge t e) *. muu)) *. 
  fm ** (x /. ((capacity_of_edge t e) *. muu))

let path_gradient (p:path) (muu:float) (f:flow) (t:topology) : float =
  List.fold_left ~init:0.0 ~f:(fun acc e ->
                                 acc +. match EdgeMap.find f e with
                                 | None -> 0.0
                                 | Some x -> (derivative_phi e muu x t)
                              ) p
                                   
(* path_update adds a specified value (rate) to the flow on each edge in the path p*)             
let path_update (p:path) (rate:float) (f:flow) : flow =
  List.fold_left ~init:f ~f:(fun acc e -> let old_rate =
                                               match EdgeMap.find f e with
                                               | None -> assert false
                                               | Some demand -> demand in
                                             EdgeMap.add ~key:e ~data:(old_rate +. rate) acc ) p

(*
let apply_to_each_edge (mcf:mc_flow) (fcn:float -> float) : mc_flow =
  SrcDstMap.fold ~init:SrcDstMap.empty 
    ~f:(fun ~key:(u,v) ~data:(edge_map) acc ->
        let new_edge_map = EdgeMap.fold ~init:EdgeMap.empty 
                          ~f:(fun ~key:e ~data:x acc ->
                              EdgeMap.add ~key:e ~data:(fcn x) acc)
	                  edge_map in
	SrcDstMap.add ~key:(u,v) ~data:new_edge_map acc
    ) mcf
*)

let apply_on_each_edge (mcf:mc_flow) (fcn:edge -> float -> float) : mc_flow =
  SrcDstMap.fold ~init:SrcDstMap.empty 
    ~f:(fun ~key:(u,v) ~data:(edge_map) acc ->
        let new_edge_map = EdgeMap.fold ~init:EdgeMap.empty 
                          ~f:(fun ~key:e ~data:x acc ->
                              EdgeMap.add ~key:e ~data:(fcn e x) acc)
	                  edge_map in
	SrcDstMap.add ~key:(u,v) ~data:new_edge_map acc
    ) mcf

let solve (topo:topology) (d:demands) : scheme =
  
  ignore (if (SrcDstMap.is_empty !prev_scheme) then failwith "Kulfi_Ak must be initialized with a non-empty scheme" else ());
  (* First build HashMaps, keyed by edges, containing the
     values f(e), f_i(e), from the pseudocode. *)
  let f' = Topology.fold_edges (fun edge acc -> EdgeMap.add acc ~key:edge ~data:0.0 ) topo EdgeMap.empty in
  let f_i' = SrcDstMap.fold ~init:SrcDstMap.empty ~f:(fun ~key:(u,v) ~data:_ acc -> SrcDstMap.add ~key:(u,v) ~data:f' acc) !prev_scheme in

  (* populate f,f_i according to what we saw in the last scheme *)
  let (f,f_i) =
    SrcDstMap.fold
      !prev_scheme
      ~init:(f',f_i')
      ~f:(fun ~key:(u,v) ~data:path_map (f,f_i) ->
          let r = match SrcDstMap.find d (u,v) with
            | None -> 0.
            | Some r -> r in
          (* for each (k,v) pair: let f' = call path_update k r*v f in *)
          PathMap.fold
	    path_map
            ~init:(f,f_i)
            ~f:(fun ~key:p
                    ~data:x acc ->
                let f'' = path_update p (r*.x) f in
                let f_i'' = let f = match SrcDstMap.find f_i (u,v) with
                              | None -> assert false
                              | Some f -> f in                                                                                                
                            SrcDstMap.add ~key:(u,v) ~data:(path_update p (r*.x) f) f_i in
                (f'', f_i''))) in
  
  (* recompute mu, RouteMetric line 1 *)
  mu := Float.min_inan (!mu) ( 2.0 ** (Float.round_down ((log (epsilon *. (find_max_flow f))) /. (log 2.0))) ) ;

  (* RouteMetric line 2 defines phi, never used. Instead the derivative is used. *)

  (* RouteMetric line 3 *)

  (* Calculate H, the max number of edges of any path in scheme s. *)
  let h = SrcDstMap.fold 
            ~init:0.0
            ~f:(fun ~key:(u,v) ~data:(path_map) acc -> 
	          PathMap.fold ~init:0.0
                    ~f:(fun ~key:p ~data:x acc -> Float.max_inan acc (Float.of_int (List.length p)))
                    path_map
               )
            !prev_scheme in

  (* Step 4 of FlowControl *)
  let scale_factor = (beta topo) /. (4.0 *. h) in
  let delta_minus = apply_on_each_edge f_i (fun e x -> (x *. scale_factor) ) in
  let step_fourbee_rhs e x = ((1.0 +. (beta topo)) *. (Float.max_inan x (f_umlaut e !mu topo ) ) ) -. x in
  let delta_plus = apply_on_each_edge f_i step_fourbee_rhs in

  (* Procedure REROUTE *)
  let new_scheme =
  SrcDstMap.fold (* Iterate over each commodity i *)
    ~init:SrcDstMap.empty (* Initialize new routing scheme to be empty *)
    ~f:(fun ~key:(u,v) ~data:(path_map) new_path_map ->
      (* The main logic of the REROUTE procedure goes here. *)
      (* find_or_die is like SrcDstMap.find, but it bombs with "assert false"
         if the key is not in the SrcDstMap. *)
      let find_or_die mcf key = 
	match SrcDstMap.find mcf key with 
        | None -> assert false 
        | Some f -> f 
      in
      let fi = find_or_die f_i (u,v) in
      let dmi = find_or_die delta_minus (u,v) in
      let dpi = find_or_die delta_plus (u,v) in
      (* Retrieve demand for the commodity with source u, dest v *)
      let d_i = SrcDstMap.fold d ~init:0.0 ~f:(fun ~key:(uu,vv) ~data:r acc3 ->
	   if ( uu = u && vv = v ) then acc3 +. r else acc3) in
      (* Specify initial target amount that we want to reroute.
         This is the combined \Delta^-_i value over all edges 
	 leaving the source node, u. *)
      let initial_target = 
        EdgeMap.fold 
	  ~init:0.0
	  ~f:(fun ~key:e ~data:x acc ->
	      if ( ( fst (Topology.edge_src e) ) = u ) then acc +. x else acc)
          dmi in
      (* Find the path on which we would adjust flow, if we are allowed to. 
         This is the argmin in step 1 of the "do" block of the "while" loop
         of Awerbuch-Khandekar's REROUTE procedure. *)
      let find_path (ff:flow) (fd:flow_decomp) (lb:flow) : path =
	let (pending_path,its_gradient) = 
	  PathMap.fold
	    ~init:([],Float.infinity) 
	    ~f:(fun ~key:p ~data:x acc ->
                 let test1 = (* Test if minimum lb value on path p is > 0 *)
		   List.fold_left p ~init:true ~f:(fun acc2 e -> 
		     let lbe = match EdgeMap.find lb e with
		                | None -> assert false
				| Some x -> x 
		     in
		     acc2 && (lbe >. 0.0) 
                   ) in
		 (* Compute the sum on the RHS of the weird derivative test *)
		 let rhs = EdgeMap.fold 
		             ~init:0.0
			     ~f:(fun ~key:e ~data:x acc ->
			         acc +. (x *. ( derivative_phi e !mu x topo )))
			     ff in
		 let grad = ( path_gradient p !mu f topo ) in
		 let test2 = (* Weird derivative test *) 
		   (d_i *. ( 1. +. (alpha topo) ) *. grad) <. rhs
		 in
		 if (test1 && test2 && ( grad <. (snd acc) )) then
		   (p,grad)
		 else 
		   acc
	      ) 
              fd 
	in 
	(* Return only the path, not its gradient... *)
	pending_path
      in
      (* Awerbuch_Khandekar's REROUTE procedure. *)
      let rec reroute (target:float) (ff:flow) (fd:flow_decomp) (lb:flow) (ub:flow) : flow_decomp = 
	let p = find_path ff fd lb in 
	let bottleneck_rate = 
	  List.fold_left
	    ~init:(Float.infinity)
	    ~f:(fun acc e -> 
	      let ube = match EdgeMap.find ub e with
		         | None -> Float.infinity
			 | Some x -> x
	      in ( Float.min_inan acc ube ) ) 
	    p in
	if (target >. 0. && ( List.length p ) > 0)
	  then
	  let delta = Float.min_inan bottleneck_rate target in
	  let new_target = target -. delta in
	  let scale_factor = 1. -. (delta /. d_i) in
	  let scaled_ff = 
	    EdgeMap.fold 
	      ~init:EdgeMap.empty 
	      ~f:(fun ~key:e ~data:x acc ->
		  EdgeMap.add acc ~key:e ~data:(x *. scale_factor) ) ff in
	  let new_lb =
	    EdgeMap.fold
	      ~init:EdgeMap.empty
	      ~f:(fun ~key:e ~data:x acc ->
		  let fie = match EdgeMap.find ff e with
		            | None -> 0.
			    | Some y -> y
		  in
		  let subtrahand = (delta /. d_i) *. fie in
		  EdgeMap.add acc ~key:e ~data:(x -. subtrahand)
	      ) lb in
	  let new_ub = 
	    path_update p ( Float.neg delta ) ub in
	  let new_ff = 
	    path_update p delta scaled_ff in
	  let new_fd = 
	    PathMap.add ~key:p ~data:delta fd in
	  reroute new_target new_ff new_fd new_lb new_ub
	else (* matches if (target >. 0.) && ( List.length p ) > 0 *)
	  fd
      in
      SrcDstMap.add ~key:(u,v) ~data:(reroute initial_target fi ( find_or_die
      !prev_scheme (u,v) ) dmi dpi ) new_path_map
    ) !prev_scheme
  in
  prev_scheme := new_scheme;
  new_scheme
  

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = Kulfi_Types.normalization_recovery
