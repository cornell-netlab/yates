open Core.Std
open Async.Std
open Command
open Kulfi_Types

type solver_type = Mcf | Vlb | Ecmp | Spf | Ak | Smcf

let main algo topo_fn actual_fn predicted_fn hosts_fn init_str () =
  match algo with 
  | Mcf -> let module C = Kulfi_Controller.Make(Kulfi_Mcf) in C.start topo_fn actual_fn hosts_fn init_str ()
  | Vlb -> let module C = Kulfi_Controller.Make(Kulfi_Vlb) in C.start topo_fn actual_fn hosts_fn init_str ()
  | Ecmp -> let module C = Kulfi_Controller.Make(Kulfi_Ecmp) in C.start topo_fn actual_fn hosts_fn init_str ()  
  | Spf -> let module C = Kulfi_Controller.Make(Kulfi_Spf) in C.start topo_fn actual_fn hosts_fn init_str ()
  | Ak -> let module C = Kulfi_Controller.Make(Kulfi_Ak) in C.start topo_fn actual_fn hosts_fn init_str ()
  | Smcf -> let module C = Kulfi_Controller.Make(Kulfi_SemiMcf) in C.start topo_fn actual_fn hosts_fn init_str ()
								    
let kulfi_main_cmd =
  Command.basic
    ~summary:"Run the Kulfi SDN controller"
    Command.Spec.(
      empty
      +> flag "-ak" no_arg ~doc:" run ak"
      +> flag "-ecmp" no_arg ~doc:" run ecmp"
      +> flag "-mcf" no_arg ~doc:" run mcf"
      +> flag "-spf" no_arg ~doc:" run spf"
      +> flag "-vlb" no_arg ~doc:" run vlb"
      +> flag "-smcf" no_arg ~doc:" run semi mcf"
      +> flag "-init" (optional string) ~doc:" solver to inititialize input scheme"
      +> anon ("topology-file" %: string)
      +> anon ("actual-file" %: string)
      +> anon ("predicted-file" %: string)
      +> anon ("host-file" %: string)
      ) 
    (fun (ak:bool)
	 (ecmp:bool)
	 (mcf:bool)
	 (spf:bool)
	 (vlb:bool)
	 (smcf:bool)
	 (init_str:string option)
	 (topo_fn:string)
	 (actual_fn:string)
	 (predicted_fn:string)
	 (host_fn:string) () ->
     let algorithm =
       if ak then Spf 
       else if ecmp then Ecmp
       else if mcf then Mcf
       else if spf then Spf
       else if vlb then Vlb
       else if smcf then Smcf
       else assert false in
     main algorithm topo_fn actual_fn predicted_fn host_fn init_str () )

let () = 
  Command.run kulfi_main_cmd;
  never_returns(Scheduler.go ())
