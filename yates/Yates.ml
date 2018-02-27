open Core
open Async
open Command
open Yates_Types

type solver_type = | Mcf | Vlb | Ecmp | Ksp | Spf | Raeke  | Ak | Smcf

let main algo topo_fn actual_fn predicted_fn hosts_fn init_str src_routing () =
  match algo with
  | Mcf -> let module C = Yates_Controller.Make(Yates_Mcf) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()
  | Vlb -> let module C = Yates_Controller.Make(Yates_Vlb) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()
  | Ecmp -> let module C = Yates_Controller.Make(Yates_Ecmp) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()
  | Ksp -> let module C = Yates_Controller.Make(Yates_Ksp) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()
  | Spf -> let module C = Yates_Controller.Make(Yates_Spf) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()
  | Ak -> let module C = Yates_Controller.Make(Yates_Ak) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()
  | Smcf -> let module C = Yates_Controller.Make(Yates_SemiMcf) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()
  | Raeke -> let module C = Yates_Controller.Make(Yates_Raeke) in C.start topo_fn actual_fn hosts_fn init_str src_routing ()

let kulfi_main_cmd =
  Command.basic_spec
    ~summary:"Run the Yates SDN controller"
    Command.Spec.(
      empty
      +> flag "-src" no_arg ~doc:" use source routing"
      +> flag "-ak" no_arg ~doc:" run ak"
      +> flag "-ecmp" no_arg ~doc:" run ecmp"
      +> flag "-ksp" no_arg ~doc:" run ksp"
      +> flag "-mcf" no_arg ~doc:" run mcf"
      +> flag "-spf" no_arg ~doc:" run spf"
      +> flag "-vlb" no_arg ~doc:" run vlb"
      +> flag "-smcf" no_arg ~doc:" run semi mcf"
      +> flag "-raeke" no_arg ~doc:" run raeke"
      +> flag "-init" (optional string) ~doc:" solver to inititialize input
      scheme [ecmp|ksp|mcf|raeke|vlb]"
      +> flag "-budget" (optional int) ~doc:" max paths between each pair of hosts"
      +> anon ("topology-file" %: string)
      +> anon ("actual-file" %: string)
      +> anon ("predicted-file" %: string)
      +> anon ("host-file" %: string)
      )
    (fun
      (src:bool)
      (ak:bool)
      (ecmp:bool)
      (ksp:bool)
      (mcf:bool)
      (spf:bool)
      (vlb:bool)
      (smcf:bool)
      (raeke:bool)
      (init_str:string option)
      (budget:int option)
      (topo_fn:string)
      (actual_fn:string)
      (predicted_fn:string)
      (host_fn:string) () ->
     ignore(Yates_Globals.budget := match budget with | None -> Int.max_value | Some x -> x);
     let algorithm =
       if ak then Spf
       else if ecmp then Ecmp
       else if ksp then Ksp
       else if mcf then Mcf
       else if spf then Spf
       else if vlb then Vlb
       else if smcf then Smcf
       else if raeke then Raeke
       else assert false in
     main algorithm topo_fn actual_fn predicted_fn host_fn init_str src () )

let () =
  Command.run kulfi_main_cmd;
  never_returns(Scheduler.go ())
