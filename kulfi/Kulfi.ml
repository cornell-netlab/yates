open Core.Std
open Async.Std
open Command

let main topo_fn () = 
  let topo = Frenetic_Network.Net.Parse.from_dotfile topo_fn in 
  Kulfi_Controller.Controller.start topo

let kulfi_main_cmd = 
  Command.basic
    ~summary:"Run the Kulfi SDN controller"
    Command.Spec.(empty +> anon ("topology" %: string))
    main 

let () = 
  Command.run kulfi_main_cmd;
  never_returns(Scheduler.go ())
