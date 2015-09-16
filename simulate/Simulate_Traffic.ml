
open Core.Std
open Kulfi_Types

let open_demands (topo_file:string) (host_file:string) : (index_map * in_channel) =
  let (_,host_map) = 
    In_channel.with_file
      host_file
      ~f:(fun file ->
	  In_channel.fold_lines
	    file ~init:(0,IntMap.empty)
	    ~f:(fun (i,m) line ->
		(i+1, (IntMap.add m ~key:i ~data:line))))      
  in  
  (host_map, (In_channel.create topo_file))

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
  for i = 0 to size do
    for j = 0 to size do      
      let s = match IntMap.find host_map i with | None -> assert false | Some x -> x in
      let d = match IntMap.find host_map j with | None -> assert false | Some x -> x in      
      let v = entries.((i*size) + j)  in
      (* TODO(rjs) how do I get a host from an index or string? *)
      assert false
    done
  done;
  !demands


