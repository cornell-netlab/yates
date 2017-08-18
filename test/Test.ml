(* Write tests in independent modules, then include them here to run *)

open Test_Routing
open Test_Stats

let _ =
  Ppx_inline_test_lib.Runtime.exit ()
