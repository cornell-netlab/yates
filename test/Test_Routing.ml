open Kulfi_Routing

module Solver =  Kulfi_Routing.Make(Kulfi_MCF)
                                          
       
TEST "routing1" =
  let _ = Solver.solve in 
  true
    

