open Core.Std

include Kulfi_LP_Lang

(* Generate variables for naive ingress traffic shift *)
let var_name_sin topo x v =
  Printf.sprintf "sin_%s"
    (string_of_pair topo (x, v))

(* Generate variables for naive egress traffic shift *)
let var_name_seg topo x u =
  Printf.sprintf "seg_%s"
    (string_of_pair topo (x, u))

(* Generate variables for SD ingress traffic shift *)
let var_name_sdsin topo u x v =
  Printf.sprintf "sin_%s--%s--%s"
    (name_of_vertex topo u)
    (name_of_vertex topo x)
    (name_of_vertex topo v)

(* Generate variables for SD egress traffic shift *)
let var_name_sdseg topo x u v =
  Printf.sprintf "seg_%s--%s--%s"
    (name_of_vertex topo x)
    (name_of_vertex topo u)
    (name_of_vertex topo v)
