open Core.Std
open Async.Std
open Command
open Kulfi_Types

type solver_type = | Mcf | Vlb | Ecmp | Spf | Ak

let main algorithm topo_fn () =
  let topo = Frenetic_Network.Net.Parse.from_dotfile topo_fn in
  let predict = SrcDstMap.empty (* TODO(jnf) *) in
  let actual = SrcDstMap.empty (* TODO(jnf) *) in
  let module Controller =  Kulfi_Controller.Make(Kulfi_Spf) in
  Controller.start topo predict actual ()

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
      +> anon ("topology" %: string)
      ) (fun (ak:bool)
      (ecmp:bool)
      (mcf:bool)
      (spf:bool)
      (vlb:bool)
      (topology:string) () ->
        let algorithm = 
          if ak then Spf else
            if ecmp then Ecmp else
              if mcf then Mcf else
                if spf then Spf else
                  if vlb then Vlb
        else assert false
        in
      main algorithm topology () )

let () = 
  Command.run kulfi_main_cmd;
  never_returns(Scheduler.go ())
