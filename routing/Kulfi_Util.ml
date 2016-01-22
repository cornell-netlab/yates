open Array
open Core.Std
open Frenetic_Network
open Kulfi_Types
open Net
open Net.Topology

let intercalate f s = function
  | [] ->
    ""
  | h::t ->
    List.fold_left t ~f:(fun acc x -> acc ^ s ^ f x) ~init:(f h) 

let string_of_edge (t:topology) (e:edge) : string =
  Printf.sprintf "(%s,%s)"
                    (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_src e))))
                    (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_dst e))))

let edge_to_string_map (t:topology) : string EdgeMap.t =
  let edge_list = EdgeSet.elements (Topology.edges t) in
  List.fold_left edge_list
  ~init:EdgeMap.empty
  ~f:(fun acc e ->
    let edge_str = (string_of_edge t e) in
    EdgeMap.add acc ~key:e ~data:edge_str)


let string_to_edge_map (t:topology) : edge Kulfi_Types.StringMap.t =
  let edge_list = EdgeSet.elements (Topology.edges t) in
  List.fold_left edge_list
  ~init:StringMap.empty
  ~f:(fun acc e -> StringMap.add acc ~key:(string_of_edge t e) ~data:e)

(* dump a list of edges *)
let dump_edges (t:topology) (es:path) : string =
  intercalate 
    (fun e -> 
     Printf.sprintf "(%s,%s)" 
                    (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_src e))))
                    (Node.name (Net.Topology.vertex_to_label t (fst (Net.Topology.edge_dst e))))) ", "  es

let dump_path_prob_set (t:topology) (pps:probability PathMap.t) : string =
  let buf = Buffer.create 101 in
  PathMap.iter 
    pps 
    ~f:(fun ~key:path ~data:prob -> Printf.bprintf buf "[%s] @ %f\n" (dump_edges t path) prob);
  Buffer.contents buf

let dump_scheme (t:topology) (s:scheme) : string = 
  let buf = Buffer.create 101 in
  SrcDstMap.iter s ~f:(fun ~key:(v1,v2) ~data:pps ->
                       Printf.bprintf buf "%s -> %s :\n  %s\n"
                                      (Node.name (Net.Topology.vertex_to_label t v1))
                                      (Node.name (Net.Topology.vertex_to_label t v2))
                                      (dump_path_prob_set t pps));
  Buffer.contents buf

let write_to_file filename text_to_write =
  try
    let cout = Out_channel.create filename in
    let co = Format.formatter_of_out_channel cout in
    Format.fprintf co "%s\n" text_to_write;
    Out_channel.close cout
  with _ as e ->
    Format.printf "Cannot open file \"%s\": %s\n" filename (Exn.to_string e)

let list_last l = match l with
  | hd::tl -> List.fold_left ~f:(fun _ x -> x) ~init:hd tl
  | []    -> failwith "no element"

let rec list_next l elem = match l with
  (* Assumes no duplicate elems in list *)
  | hd::tl -> if hd = elem then List.hd tl
              else list_next tl elem
  | [] -> failwith "not found"

let rec next_hop (t:topology) (p:path) (e:edge) = match p with
  (* Assumes no duplicate elems in list *)
  | hd::tl -> if (string_of_edge t hd) = (string_of_edge t e) then List.hd tl
              else next_hop t tl e
  | [] -> failwith "not found"

let next_hop_arr (p:edge Array.t) (dist:int) : edge option =
  if Array.length p <= dist then None
  else Some (p.(dist))

(* sublist containing first n elements *)
let rec list_first_n l n =
  if n = 0 then []
  else match l with
  | hd::tl -> hd::(list_first_n tl (n-1))
  | [] -> failwith "not enough elements"

let get_hosts (topo:topology) =
  let host_set = VertexSet.filter (Topology.vertexes topo)
  ~f:(fun v ->
    let label = Topology.vertex_to_label topo v in
    Node.device label = Node.Host) in
  Topology.VertexSet.elements host_set

let all_pairs_connectivity topo hosts scheme : bool =
  List.fold_left hosts
    ~init:true
    ~f:(fun acc u ->
      List.fold_left hosts
        ~init:acc
        ~f:(fun acc v ->
	         if u = v then acc
           else
             match SrcDstMap.find scheme (u,v) with
             | None -> Printf.printf "No route for pair (%s, %s)\n%!"
             (Node.name (Net.Topology.vertex_to_label topo u))
             (Node.name (Net.Topology.vertex_to_label topo v)); false
             | Some paths -> not (PathMap.is_empty paths)  && acc))


let paths_are_nonempty (s:scheme) : bool =
    SrcDstMap.fold s
      ~init:true
      (* for every pair of hosts u,v *)
      ~f:(fun ~key:(u,v) ~data:paths acc ->
        if u = v then true && acc
        else
          PathMap.fold paths
          ~init:acc
	        (* get the possible paths, and for every path *)
          ~f:(fun ~key:path ~data:_ acc ->
		         acc && (not (List.is_empty path))))

let probabilities_sum_to_one (s:scheme) : bool =
  SrcDstMap.fold s
    ~init:true
    ~f:(fun ~key:(u,v) ~data:f_decomp acc ->
      if u = v then acc
      else
        let sum_rate =
          PathMap.fold f_decomp
          ~init:0.
	        ~f:(fun ~key:path ~data:r acc -> acc +. r) in
	      acc && (sum_rate > 0.9) && (sum_rate < 1.1) )


(* prune a scheme by limiting number of s-d paths within a given budget *)
let prune_scheme (t:topology) (s:scheme) (budget:int) : scheme =
  let new_scheme = SrcDstMap.fold s
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
      if src = dst then acc
      else
        let pruned_paths = if PathMap.length paths <= budget then paths
              else
                let pplist = PathMap.to_alist paths in
                let sorted_pp = List.sort ~cmp:(fun x y -> Float.compare (snd y) (snd x)) pplist in
                let top_pp = list_first_n sorted_pp budget in
                let total_prob = List.fold_left top_pp ~init:0.0 ~f:(fun acc (_,prob) -> acc +. prob) in
                List.fold_left top_pp
                  ~init:PathMap.empty
                  ~f:(fun acc (path,prob) -> PathMap.add acc ~key:path ~data:(prob /. total_prob))
                in
        SrcDstMap.add acc ~key:(src,dst) ~data:pruned_paths) in
  assert (probabilities_sum_to_one new_scheme);
  assert (all_pairs_connectivity t (get_hosts t) new_scheme);
  new_scheme

let is_nan x =
  (not (x > 0.)) && (not (x <= 0.))

(* Calculate fair share of flows *)
let fair_share_at_edge (capacity:float) (in_flows: float PathMap.t) : (float PathMap.t) =
  let path_dem_list = PathMap.to_alist in_flows in
  let sorted_pdlist = List.sort ~cmp:(fun x y -> Float.compare (snd x) (snd y)) path_dem_list in
  (*ignore (List.iter sorted_pdlist ~f:(fun (_,d) -> Printf.printf "%f\n%!" d;); Printf.printf "\n";);
    assert false;*)
  let (fair_share,_,_) = List.fold_left sorted_pdlist
      ~init:(PathMap.empty, capacity, List.length sorted_pdlist)
      ~f:(fun acc (p,d) ->
        let (curr_share, spare_cap, n_rem_flows) = acc in
        if d *. (Float.of_int n_rem_flows) <= spare_cap then
          let new_share = PathMap.add curr_share ~key:p ~data:d in
          (new_share, spare_cap -. d, n_rem_flows - 1)
        else
          let fs = (spare_cap /. (Float.of_int n_rem_flows)) in
          if is_nan fs then assert false;
          let new_share = PathMap.add curr_share ~key:p ~data:fs in
          (new_share, spare_cap -. fs, n_rem_flows - 1)) in
  fair_share

  
(* Calculate fair share of flows *)
let fair_share_at_edge_arr (capacity:float) (in_flows: (edge Array.t * int * float) List.t) : ((edge Array.t * int * float) List.t) =
  let path_dem_list = Array.of_list in_flows in
  let _ = Array.sort ~cmp:(fun (_,_,x) (_,_,y) -> Float.compare x y) path_dem_list in
  (*ignore (List.iter sorted_pdlist ~f:(fun (_,d) -> Printf.printf "%f\n%!" d;); Printf.printf "\n";);
    assert false;*)
  let (fair_share,_,_) = Array.foldi
      path_dem_list
      ~init:([], capacity, Array.length path_dem_list)
      ~f:(fun _ acc (p,dist,d) ->
        let (curr_share, spare_cap, n_rem_flows) = acc in
        if d *. (Float.of_int n_rem_flows) <= spare_cap then
          let new_share =  (p,dist,d)::curr_share in
          (new_share, spare_cap -. d, n_rem_flows - 1)
        else
          let fs = (spare_cap /. (Float.of_int n_rem_flows)) in
          if is_nan fs then assert false;
          let new_share =  (p,dist,fs)::curr_share in
          (new_share, spare_cap -. fs, n_rem_flows - 1))  in
  fair_share

