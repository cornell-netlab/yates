open Core
open Async
open Command

open Yates_controller
open Yates_solvers.Solvers
open Yates_types.Types
open Yates_routing

let main algo topo_fn actual_fn predicted_fn hosts_fn src_routing () =
  match algo with
  | Ac -> let module C = Controller.Make(AC) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | AkEcmp
  | AkKsp
  | AkMcf
  | AkRaeke
  | AkVlb -> let module C = Controller.Make(Ak) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Cspf -> let module C = Controller.Make(Cspf) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Ecmp -> let module C = Controller.Make(Ecmp) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Edksp -> let module C = Controller.Make(Edksp) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Ffc
  | Ffced -> let module C = Controller.Make(Ffc) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Ksp -> let module C = Controller.Make(Ksp) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Mcf -> let module C = Controller.Make(Mcf) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | MwMcf -> let module C = Controller.Make(MwMcf) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | OptimalMcf -> let module C = Controller.Make(Mcf) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Raeke -> let module C = Controller.Make(Raeke) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | SemiMcfAc
  | SemiMcfEcmp
  | SemiMcfEdksp
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfVlb -> let module C = Controller.Make(SemiMcf) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Spf -> let module C = Controller.Make(Spf) in C.start topo_fn actual_fn hosts_fn algo src_routing ()
  | Vlb -> let module C = Controller.Make(Vlb) in C.start topo_fn actual_fn hosts_fn algo src_routing ()

let yates_main_cmd =
  Command.basic_spec
    ~summary:"Run the Yates SDN controller"
    Command.Spec.(
      empty
      +> flag "-src" no_arg ~doc:" use source routing"
      +> flag "-alg" (required string) ~doc:(
        Printf.sprintf " TE algorithm to use. Options: %s"
          (List.fold_left all_solver_string_descripton ~init:"\n"
             ~f:(fun acc (key, desc) -> acc ^ key ^ ":\t" ^ desc ^ ".\n")))
      +> flag "-budget" (optional int) ~doc:" max paths between each pair of hosts"
      +> anon ("topology-file" %: string)
      +> anon ("actual-file" %: string)
      +> anon ("predicted-file" %: string)
      +> anon ("host-file" %: string)
      )
    (fun
      (src:bool)
      (alg:string)
      (budget:int option)
      (topo_fn:string)
      (actual_fn:string)
      (predicted_fn:string)
      (host_fn:string) () ->
     ignore(Globals.budget := match budget with | None -> Int.max_value | Some x -> x);
     let algorithm = string_to_solver alg in
     main algorithm topo_fn actual_fn predicted_fn host_fn src () )

let () =
  Command.run yates_main_cmd;
  never_returns(Scheduler.go ())
