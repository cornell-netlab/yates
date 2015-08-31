open Core.Std

module StringMap = Map.Make(String)

type entry_map = (float list) StringMap.t  
			  
type experimental_data = {
  name : string ;
  records: entry_map ;
}
       
let make_data (experiment_name:string) : experimental_data =
  { name = experiment_name; records = StringMap.empty ; }
    
let add_field_value (d:experimental_data) (field_name:string) (value:float) : experimental_data =  
  let entries = match (StringMap.find d.records field_name) with
  | None -> assert false
  | Some x -> x in
  let entries' = List.append entries [value] in
  let records' = StringMap.add ~key:field_name ~data:entries' d.records in  
  { d with records = records' }
              
let add_field (d:experimental_data) (field_name:string) : experimental_data =  
  let records' = StringMap.add ~key:field_name ~data:[] d.records in
  { d with records = records' }
  
let print (d:experimental_data) : unit =
  assert false
