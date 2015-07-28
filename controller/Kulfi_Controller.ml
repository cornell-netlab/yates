open Kulfi_Routing
open Kulfi_MCF

                
module Solver =  Kulfi_Routing.Make(Kulfi_MCF)

let () =
  print_endline "Kulfi Controller"
