open Core.Std
open Frenetic_Network
open Kulfi_Types

let intercalate f s = function
  | [] ->
    ""
  | h::t ->
    List.fold_left t ~f:(fun acc x -> acc ^ s ^ f x) ~init:(f h) 

                          
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

(* sublist containing first n elements *)
let rec list_first_n l n =
  if n = 0 then []
  else match l with
  | hd::tl -> hd::(list_first_n tl (n-1))
  | [] -> failwith "not enough elements"

(* prune a scheme by limiting number of s-d paths within a given budget *)
let prune_scheme (s:scheme) (budget:int) : scheme =
  let new_scheme = SrcDstMap.fold s
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
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
  new_scheme


