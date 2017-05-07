open Core.Std

include Kulfi_LP_Lang

(* Generate variables for ingress traffic shift *)
let var_name_sin topo host dst =
  Printf.sprintf "sin_%s"
    (string_of_pair topo (host, dst))

(* Generate variables for egress traffic shift *)
let var_name_seg topo src host =
  Printf.sprintf "seg_%s"
    (string_of_pair topo (src, host))
