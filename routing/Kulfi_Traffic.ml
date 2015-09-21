open Core.Std
open Kulfi_Types
open Frenetic_Network
open Net  
       
let open_demands (demand_file:string) (host_file:string) (topo:topology) : (index_map * in_channel) =

  let name_map =
    VertexSet.fold (Topology.vertexes topo)
		   ~init:StringMap.empty
		   ~f:(fun acc v ->
		       let n = (Net.Topology.vertex_to_label topo v) in
		       (StringMap.add acc ~key:(Node.name n) ~data:v )) in

  let (_,host_map) = 
    In_channel.with_file
      host_file
      ~f:(fun file ->
	  In_channel.fold_lines
	    file ~init:(0,IntMap.empty)
	    ~f:(fun (i,m) line ->

		let n = match (StringMap.find name_map line) with
		  | None -> assert false | Some x -> x in
		
		(i+1, (IntMap.add m ~key:i ~data:n))))      
  in  
  (host_map, (In_channel.create demand_file))

let close_demands (ic:in_channel) : unit =
  In_channel.close ic
	  
let next_demand (ic:in_channel) (host_map:index_map): demands =
  let line =
    try 
      input_line ic 
    with e ->                    
      close_in_noerr ic;         
      raise e
  in
  let entries = Array.of_list (String.split line ~on:' ') in
  let size = Int.of_float (sqrt (Float.of_int (Array.length entries))) in
  let demands = ref SrcDstMap.empty in
  for i = 0 to (size-1) do
    for j = 0 to (size-1) do      
      let s = match IntMap.find host_map i with | None -> assert false | Some x -> x in
      let d = match IntMap.find host_map j with | None -> assert false | Some x -> x in      
      (* Can't demand from yourself *)
      let v = if i = j then 0.0 else Float.of_string (entries.((i*size) + j)) in
      demands := SrcDstMap.add !demands ~key:(s,d) ~data:v
    done
  done;
  !demands
