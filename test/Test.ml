(* Write tests in independent modules, then include them here to run *)

open Test_Routing
open Test_Stats
open Ppx_inline_test_lib.Runtime       

let _ = add_evaluator ~f:(fun _ -> Test_result.Success)

