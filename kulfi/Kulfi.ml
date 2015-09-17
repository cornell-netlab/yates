open Core.Std
open Async.Std
open Command
open Kulfi_Types

type solver_type = | Mcf | Vlb | Ecmp | Spf | Ak

let main algorithm topo_fn actual_demand predicted_demand host_file () =
  let topo = Frenetic_Network.Net.Parse.from_dotfile topo_fn in
  (* TODO: Use correct routing algorithm based on CLI option *)
  let module Controller =  Kulfi_Controller.Make(Kulfi_Mcf) in
  Controller.start topo actual_demand predicted_demand host_file ()

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
      +> anon ("topology-file" %: string)
      +> anon ("actual-demand" %: string)
      +> anon ("predicted-demand" %: string)
      +> anon ("host-file" %: string)
      ) (fun (ak:bool)
      (ecmp:bool)
      (mcf:bool)
      (spf:bool)
      (vlb:bool)
      (topology_file:string)
      (actual_demand:string)
      (predicted_demand:string)
      (host_file:string) () ->
        let algorithm =
          if ak then Spf else
            if ecmp then Ecmp else
              if mcf then Mcf else
                if spf then Spf else
                  if vlb then Vlb
        else assert false
        in
      main algorithm topology_file actual_demand predicted_demand host_file () )

let () = 
  Command.run kulfi_main_cmd;
  never_returns(Scheduler.go ())
