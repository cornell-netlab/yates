open Core
open Kulfi_Types
open Frenetic_Network
open Net
open Kulfi_Mw
open Kulfi_Frt
open Kulfi_Rrt

(* multiplicative weights input *)
module MWInput : MW_INPUT with type structure = scheme = struct

  type structure = scheme

  let select_structure (topo : topology) (d: demands) (nodes : Topology.VertexSet.t) =
    (* Call SPF to get a routing scheme from topo *)
    (* The following code is essentially copied from Kulfi_Spf.solve,
       but we have to duplicate here because the set "nodes" is given
       as an argument to select_structure. *)
    let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
      ~f:(fun x y -> (Topology.VertexSet.mem nodes x) && (Topology.VertexSet.mem nodes y))
      in
    (* let inv_bottleneck = List.fold_left apsp ~init:0. ~f:(fun acc (_,v1,v2,p) -> 
      let dmd = match SrcDstMap.find d (u,v) with
	| None -> 0.
	| Some y -> y in
      List.fold_left p ~init:0. ~f:(fun acc e -> 
	let ratio = dmd /. (capacity_of_edge topo e) in
	Float.max_inan acc ratio ) ) in *)
    let spf_scheme = List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) -> 
      SrcDstMap.add acc ~key:(v1,v2) ~data:( PathMap.singleton p 1. ) ) in
    (spf_scheme, 1.)
      

  let usage_of_structure (topo : topology) (d: demands) (s : scheme) =
    let usage_map = SrcDstMap.fold s ~init:EdgeMap.empty 
      ~f:(fun ~key:(u,v) ~data:f_decomp acc ->
	let dmd = match SrcDstMap.find d (u,v) with
	  | None -> 0.
	  | Some x -> x in
	PathMap.fold f_decomp ~init:acc ~f:(fun ~key:p ~data:q acc ->
	  List.fold_left p ~init:acc ~f:(fun acc e ->
	    let cap = capacity_of_edge topo e in
	    let y = (dmd *. q) /. cap in
	    let new_usage = match EdgeMap.find acc e with 
	      | None -> y
	      | Some z -> z +. y in
	    EdgeMap.add acc ~key:e ~data:new_usage) ) ) in
    EdgeMap.to_alist usage_map

  let set_weight topo edge w =
    let label = Topology.edge_to_label topo edge in
    Link.set_weight label w; topo

  let get_weight topo edge =
    let label = Topology.edge_to_label topo edge in
    Link.weight label

end

(* multiplicative weights instantiation *)
module MWAlg : MW_ALG with type structure = scheme = Kulfi_Mw.Make (MWInput)

let prev_scheme = ref SrcDstMap.empty

let solve (t:topology) (d:demands) : scheme =
  let new_scheme =
  if SrcDstMap.is_empty !prev_scheme then
    let epsilon = 0.1 in 
    let end_points = 
      VertexSet.filter (Topology.vertexes t) 
	~f:(fun v -> let label = Topology.vertex_to_label t v in
		     Node.device label = Node.Host) in
    let _,mw_solution,_ = MWAlg.hedge_iterations epsilon t d end_points in   
    let (unnormalized_scheme,flow_sum) = 
      List.fold_left mw_solution
	~init:(SrcDstMap.empty, SrcDstMap.empty)
	~f:(fun (us,fs) (sch,coeff) ->
	  SrcDstMap.fold sch 
	    ~init:(us,fs) 
	    ~f:(fun ~key:(u,v) ~data:f_decomp (us,fs) ->
	      let old_us_data = match SrcDstMap.find us (u,v) with
		| None -> PathMap.empty
		| Some x -> x in
	      let old_fs_data = match SrcDstMap.find fs (u,v) with
		| None -> 0.
		| Some x -> x in
	      let (new_us_data,new_fs_data) = 
		PathMap.fold f_decomp
		~init:(old_us_data,old_fs_data)
		~f:(fun ~key:p ~data:r (oud,ofd) ->
		  let scaled_r = coeff *. r in
		  ( (add_or_increment_path oud p scaled_r) , (ofd +. scaled_r) ) ) in
	      ( SrcDstMap.add ~key:(u,v) ~data:new_us_data us,
	        SrcDstMap.add ~key:(u,v) ~data:new_fs_data fs ) ) ) in
    normalize_scheme unnormalized_scheme flow_sum
  else !prev_scheme in
  prev_scheme := new_scheme;
  new_scheme

let initialize _ = ()

let local_recovery = Kulfi_Types.normalization_recovery
