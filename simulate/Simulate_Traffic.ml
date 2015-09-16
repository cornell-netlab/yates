
open Core.Std
open Kulfi_Types

let open_demands (file:string) : in_channel =
  open_in file

let close_demands (ic:in_channel) : unit =
  close_in ic
	  
let get_demands (ic:in_channel) : (* TODO(rjs) We should return a 
                                     Kulfi.demands, need to consult map *) =
  let line =
    try 
      input_line ic 
    with e ->                    
      close_in_noerr ic;         
      raise e
  in
  let entries = String.split line ~on:' ' in
  let size = Int.of_float (sqrt (Float.of_int (List.length entries))) in
  let demands = Array.make_matrix size size 0. in
  for i = 0 to size do
    for j = 0 to size do
      (* TODO: We don't need to build the matrix.
          we can just populate a Kulfi_Types.demands directly *)      
      let v = match (List.nth entries ((i * size) + j)) with
	| Some x -> x
	| None -> assert false in
      demands.(i).(j) <- Float.of_string v
    done
  done;
  demands
