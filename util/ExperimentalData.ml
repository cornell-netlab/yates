open Core

module StringMap = Map.Make(String)

let append_out = ref false

type 'a entries = ('a list)

type 'a experimental_data = {
  mutable name : string ;
  mutable records: ('a entries) StringMap.t ;
}

let make_data (experiment_name:string) : 'a experimental_data =
  { name = experiment_name; records = StringMap.empty; }

let add_record (d:'a experimental_data) (id:string) (r:'a) : unit =
  let entries = match (StringMap.find d.records id) with
    | None -> []
    | Some x -> x in
  let entries' = List.append entries [r] in
  d.records <- StringMap.set ~key:id ~data:entries' d.records

let to_string (d:'a experimental_data) (header:string) (fn:'a -> string) : string =
  StringMap.fold
    ~init:(header ^ "\n")
    ~f:(fun ~key:name ~data:recs acc ->
      List.fold ~init:acc ~f:(fun acc r ->
        acc ^ name ^ "\t" ^ (fn r) ^ "\n") recs) d.records

let to_file (dir:string) (file:string) (d:'a experimental_data) (header:string) (fn:'a -> string) : unit =
  let _ = match (Sys.file_exists dir) with | `No -> Unix.mkdir dir | _ -> () in
  let oc = Out_channel.create (dir ^ file) ~append:(!append_out) in
  fprintf oc "%s\n"  (to_string d header fn);
  Out_channel.close oc
