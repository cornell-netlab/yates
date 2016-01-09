open Kulfi_Routing
open Frenetic_Network
open Net
open Net.Topology
open Kulfi_Ecmp
open Kulfi_Ksp
open Kulfi_Mcf
open Kulfi_Mw
open Kulfi_Raeke
open Kulfi_Spf
open Kulfi_Vlb
open Kulfi_Types
open Kulfi_Util
open Core.Std


module VertexSet = Topology.VertexSet

let tag_cell = ref 100

let create_tag_hash (t:topology) =
  let tag_hash = Hashtbl.Poly.create () in
  iter_edges
    (fun edge ->
      let src, port = edge_src edge in
      let lbl = vertex_to_label t src in
      match Node.device lbl with
        | Node.Switch ->
          begin
            let tag = !tag_cell in
            incr tag_cell;
            Hashtbl.Poly.add_exn tag_hash edge tag;
            Printf.printf "LINK: %s -> %d\n" (dump_edges t [edge]) tag;
          end
        | _ ->
          ()) t;
  tag_hash


let create_topology_and_demands () =
  let topo = Parse.from_dotfile "./data/topologies/3cycle.dot" in
  let host_set = VertexSet.filter (Topology.vertexes topo)
                                  ~f:(fun v ->
                                      let label = Topology.vertex_to_label topo v in
                                      Node.device label = Node.Host) in
  let hs = Topology.VertexSet.elements host_set in
  let hosts = Array.of_list hs in
  let demands =
    List.fold_left
      hs
      ~init:SrcDstMap.empty
      ~f:(fun acc u ->
	  List.fold_left
	    hs
	    ~init:acc
	    ~f:(fun acc v ->
		let r = if u = v then 0.0 else 500000.0 in
		SrcDstMap.add acc ~key:(u,v) ~data:r)) in

  (* Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set); *)
  (* Printf.printf "# demands = %d\n" (SrcDstMap.length demands); *)
  (* Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo); *)

  (hosts,topo,demands)

let all_pairs_connectivity hosts scheme =
  Array.fold
    hosts
    ~init:true
    ~f:(fun acc u ->
	Array.fold
	  hosts
	  ~init:acc
	  ~f:(fun acc v ->
	      if u = v then true && acc
	      else
		match SrcDstMap.find scheme (u,v) with
		| None -> 
		   false
		| Some paths -> not (PathMap.is_empty paths)  && acc))

let paths_are_nonempty (s:scheme) : bool =
    SrcDstMap.fold
      s (* fold over the scheme *)
      ~init:true
      (* for every pair of hosts u,v *)
      ~f:(fun ~key:(u,v) ~data:paths acc ->
	  if u = v then true && acc
    else
	    PathMap.fold
	      paths
	      ~init:acc
	      (* get the possible paths, and for every path *)
	      ~f:(fun ~key:path ~data:_ acc ->
		  acc && (not (List.is_empty path))))

let probabilities_sum_to_one (s:scheme) : bool =
  SrcDstMap.fold
    s
    ~init:true
    ~f:(fun ~key:(u,v) ~data:f_decomp acc ->
      if u = v then acc
      else
	let sum_rate =
	  PathMap.fold f_decomp
	    ~init:0.
	    ~f:(fun ~key:path ~data:r acc -> acc +. r) in
	acc && (sum_rate > 0.9) && (sum_rate < 1.1) )

let test_mw () = false

let test_ecmp () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = Kulfi_Ecmp.solve topo pairs in
  probabilities_sum_to_one scheme

let test_ksp () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = Kulfi_Ksp.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
  probabilities_sum_to_one scheme

let test_mcf () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme =
    Kulfi_Mcf.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_mwmcf () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme =
    Kulfi_Mcf.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_spf () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme =
    Kulfi_Spf.solve topo pairs in
  let h1 = Array.get hosts 0  in
  let h2 = Array.get hosts 1  in
  (* TODO(jnf,rjs): could just call sample_scheme here? *)
  let x = match SrcDstMap.find scheme (h1,h2)  with | None -> assert false | Some x -> x in
  PathMap.fold x
    ~init:true
    ~f:( fun ~key:path ~data:_ acc ->
      acc && ((List.length path) = 3) )

let test_vlb () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme =
    Kulfi_Vlb.solve topo pairs in
  let h1 = Array.get hosts 0  in
  let h2 = Array.get hosts 1  in
  let paths = match SrcDstMap.find scheme (h1,h2) with | None -> assert false | Some x -> x in
  (* Printf.printf "VLB set length =%d\n"  (PathMap.length paths); *)
  (* Printf.printf "%s\n" (dump_scheme topo scheme); *)
  (PathMap.length paths) = 2

let test_apsp () =
    let (hosts,topo,pairs) = create_topology_and_demands () in
    let paths = Frenetic_Network.NetPath.all_pairs_shortest_paths ~topo:topo ~f:(fun _ _ -> true) in
    Array.fold
      hosts
      ~init:true
      ~f:(fun acc u ->
	  Array.fold
	    hosts
	    ~init:acc
	    ~f:(fun acc v ->
		if u = v then acc
		else acc && List.exists paths (fun (_,v1,v2,_) -> v1 = u && v2 = v)))

let test_vlb2 () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = Kulfi_Vlb.solve topo pairs in
  Array.fold
    hosts
    ~init:true
    ~f:(fun acc u ->
	Array.fold
	  hosts
	  ~init:acc
	  ~f:(fun acc v ->
	      if u = v then true && acc
	      else
		match SrcDstMap.find scheme (u,v) with
		| None ->
		   false
		| Some paths -> not (PathMap.is_empty paths)  && acc))

let test_vlb3 () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let scheme = Kulfi_Vlb.solve topo pairs in
  paths_are_nonempty scheme

let test_raeke () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let scheme = Kulfi_Raeke.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_semimcf_mcf () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let start_scheme = Kulfi_Mcf.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_semimcf_ksp () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let start_scheme = Kulfi_Ksp.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_semimcf_vlb () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let start_scheme = Kulfi_Vlb.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_semimcf_raeke () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Routing.Raeke.solve topo pairs in
  Kulfi_Routing.SemiMcf.initialize start_scheme;
  let scheme = Kulfi_Routing.SemiMcf.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_ak_mcf () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let start_scheme = Kulfi_Mcf.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_ak_ksp () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let start_scheme = Kulfi_Ksp.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_ak_vlb () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  let start_scheme = Kulfi_Vlb.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

let test_ak_raeke () =
  let (hosts,topo,pairs) = create_topology_and_demands () in
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Raeke.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity hosts scheme &&
    probabilities_sum_to_one scheme

TEST "spf" = test_spf () = true

TEST "ecmp" = test_ecmp () = true

TEST "ksp" = test_ksp () = true

TEST "mcf" = test_mcf () = true

TEST "apsp" = test_apsp () = true

TEST "vlb" = test_vlb () = true

TEST "vlb2" = test_vlb2 () = true

TEST "vlb3" = test_vlb3 () = true

(* TEST "mw" = test_mw () = true *)

TEST "raeke" = test_raeke () = true

TEST "ak_mcf" = test_ak_mcf () = true

TEST "ak_ksp" = test_ak_ksp () = true

TEST "ak_vlb" = test_ak_vlb () = true

TEST "ak_raeke" = test_ak_raeke () = true

TEST "semimcf_mcf" = test_semimcf_mcf () = true

TEST "semimcf_ksp" = test_semimcf_ksp () = true

TEST "semimcf_vlb" = test_semimcf_vlb () = true

TEST "semimcf_raeke" = test_semimcf_raeke () = true

TEST "mwmcf" = test_mwmcf () = true

