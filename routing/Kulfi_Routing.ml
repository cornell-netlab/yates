
open Kulfi_Types
                 
module type ALGORITHM = sig
    val solve : topology -> demands -> scheme -> scheme
end
                
module Make (Algorithm:ALGORITHM) = struct
  let solve = Algorithm.solve
end
