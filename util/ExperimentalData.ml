open Core.Std

module StringMap = Map.Make(String)
			   
type 'a entries = ('a list)   
			  
type 'a experimental_data = {
  name : string ;
  records: ('a entries) StringMap.t ;
}
			      
let make_data (experiment_name:string) : 'a experimental_data =
  { name = experiment_name; records = StringMap.empty; }

let add_record (d:'a experimental_data) (id:string) (r:'a) : 'a experimental_data =  
  let entries = match (StringMap.find d.records id) with
    | None -> []
    | Some x -> x in
  let entries' = List.append entries [r] in  
  let records' = StringMap.add ~key:id ~data:entries' d.records in  
  { d with records = records' }

let to_string (d:'a experimental_data) (header:string) (fn:'a -> string) : string =      
  StringMap.fold
    ~init:(header ^ "\n")
    ~f:(fun ~key:name ~data:recs acc ->
	List.fold
	  ~init:acc
	  ~f:(fun acc r -> acc ^ name ^ "\t" ^ (fn r) ^ "\n") recs) d.records

 
