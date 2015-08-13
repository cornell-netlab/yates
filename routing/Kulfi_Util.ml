open Core.Std
open Frenetic_Network
open Kulfi_Types

let dump_path_lists (t:topology) (l : (Net.Topology.vertex * Net.Topology.vertex * (Net.Topology.edge list * float) list) list list) : string = 
  let buf = Buffer.create 101 in
  List.iter l
  ~f:(fun l' ->
    Buffer.add_string buf "[\n";
    List.iter l'
      ~f:(fun (v1,v2,cs) ->
      Printf.bprintf buf "%s -> %s :\n  %s\n"
        (Node.name (Net.Topology.vertex_to_label t v1))
        (Node.name (Net.Topology.vertex_to_label t v2))
        (Merlin_Util.intercalate (fun (es,f) -> Printf.sprintf "[%s] @ %f" (dump_edges t es) f) "\n  " cs));
    Buffer.add_string buf "]\n\n";
  );
  Buffer.contents buf
