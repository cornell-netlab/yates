open Core.Std
open Kulfi_Routing
open Frenetic_NetKAT

module Make(SOLVER:Kulfi_Routing.Algorithm) = struct
  (* let implement_scheme (scheme:scheme) : unit =  *)
  (*   SrcDstMap.fold scheme  *)
  (*     ~init:drop *)
  (*     ~f:(fun ~key:(src,dst) ~data:path_dist ~acc:prog ->  *)
  (*         PathProbabilitySet.fold *)
            


  let start () = ()
  
end

module Controller = Make(Kulfi_Routing.Mcf)

let () =
  print_endline "Kulfi Controller";
  Controller.start()
