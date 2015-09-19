open Core.Std
open Async.Std
open Kulfi_Routing
open Kulfi_Types
open Kulfi_Traffic
open Frenetic_OpenFlow
open Frenetic_OpenFlow0x01
open Message
open Frenetic_Network
open Net

module Controller = Frenetic_OpenFlow0x01_Controller

(* Global Variables *)
let port_stats_delay = 1.0

let verbose s = 
  if false then 
    Printf.eprintf "[kulfi]: %s\n%!" s
  else
    ()

let string_of_stats (sw:switchId) ((time:Int64.t), (ps:portStats)) : string = 
  Printf.sprintf 
    "time=%Ld, switch=%Ld, port_no=%d, \
     rx_packets=%Ld, tx_packets=%Ld, \
     rx_bytes=%Ld, tx_bytes=%Ld, \
     rx_dropped=%Ld, tx_dropped=%Ld, \
     rx_errors=%Ld, tx_errors=%Ld, \
     rx_frame_err=%Ld, rx_over_err=%Ld, rx_crc_err=%Ld, \
     collisions=%Ld"
    time
    sw 
    ps.port_no
    ps.rx_packets ps.tx_packets
    ps.rx_bytes ps.tx_bytes
    ps.rx_dropped ps.tx_dropped
    ps.rx_errors ps.tx_errors
    ps.rx_frame_err ps.rx_over_err ps.rx_crc_err
    ps.collisions  

module Make(Solver:Kulfi_Routing.Algorithm) = struct

  type switch_state = 
    { ports : portId list;
      flows : flowMod list }

  type state = 
    { network : (switchId, switch_state) Hashtbl.t;
      stats : (switchId * portId, (int64 * portStats) list) Hashtbl.t;
      topo : topology;
      mutable congestion : demand list;
      mutable churn : int list }

  let global_state : state = 
    { network = Hashtbl.Poly.create ();
      stats = Hashtbl.Poly.create ();
      topo = Topology.empty ();
      congestion = [];
      churn = [] }
                          
  (* Command-line Interface *)
  let rec cli () = 
    let help () = 
      Printf.printf "Kulfi commands:\n\
                     \thelp : print this message\n\
                     \tswitches : print connected switches\n\
                     \tports [switch] : print ports for [switch]\n\
                     \tflows : print installed rules\n\
                     \tstats [switch] [port]: print port stats for [switch] and [port]\n\
                     \tdump [file]: dump all port stats to [file]\n\
                     %!";
      return () in 
    let switches () = 
      Printf.printf
        "[%s]\n%!" 
        (Kulfi_Util.intercalate 
           (Printf.sprintf "%Ld") 
           ", "
           (Hashtbl.Poly.keys global_state.network));
      return () in 
    let ports sws = 
      let sw = try Int64.of_string sws with _ -> -1L in 
      begin match Hashtbl.Poly.find global_state.network sw with 
            | None -> 
               Printf.printf "No ports for switch %s\n%!" sws
            | Some sw_state -> 
               Printf.printf
                 "[%s]\n%!"
                 (Kulfi_Util.intercalate
                    (Printf.sprintf "%d")
                    ", "
                    sw_state.ports)
      end;
      return () in 
    let flows () = 
      Printf.printf
        "%s\n%!"
        (Hashtbl.Poly.fold
           global_state.network
           ~init:""
           ~f:(fun ~key:sw ~data:st acc -> 
               Printf.sprintf
                 "%sswitch %Ld:\n%s" 
                 (if acc = "" then "" else acc ^ "\n\n") 
                 sw
                 (Kulfi_Util.intercalate Frenetic_OpenFlow0x01.FlowMod.to_string "\n" st.flows)));
      return () in 

    let stats sws pts = 
      let sw = try Int64.of_string sws with _ -> -1L in 
      let pt = try Int.of_string pts with _ -> -1 in 
      begin match Hashtbl.Poly.find global_state.stats (sw,pt) with
            | None 
            | Some [] -> 
               Printf.printf "No stats for switch %s port %s\n" sws pts
            | Some ((time,ps)::_) -> 
               Printf.printf "%s\n" (string_of_stats sw (time,ps))
      end;
      return () in
    let dump fn =
      let buf = Buffer.create 101 in 
      Hashtbl.iter 
	global_state.stats 
	~f:(fun ~key:(sw,pt) ~data:stats -> 
	    List.iter 
	      stats 
	      ~f:(fun (time,ps) -> 
		  Printf.bprintf buf "%s\n" (string_of_stats sw (time,ps))));
      Kulfi_Util.write_to_file fn (Buffer.contents buf);
      return () in
    let eof () = 
      dump (Printf.sprintf "kulfi-controller-%f.txt" (Unix.time ())) >>= 
      fun () -> Pervasives.exit 0 in 
    let split s = 
      List.filter (String.split s ' ') ((<>) "") in 
    begin 
      Printf.printf "kulfi> %!";
      Reader.read_line (force Reader.stdin) >>= 
        (function
          | `Eof -> eof ()
          | `Ok s -> match split s with 
                     | ["switches"] -> switches ()
                     | ["ports"; sws] -> ports sws
                     | ["flows"] -> flows ()              
                     | ["stats"; sws; pts] -> stats sws pts
                     | ["dump"; fn] -> dump fn
                     | ["exit"] -> eof ()
                     | _ -> help ()) >>= fun _ -> 
      cli ()
    end

  (* Helper to create fresh xids *)
  let xid = 
    let r = ref 0l in 
    (fun () -> r := Int32.(!r + 1l); !r)

  (* Send messages to the controller, return xid *)
  let msgs,send_msg = 
    let r,w = Pipe.create () in 
    (r, fun (sw,m) ->
        let x = xid () in Pipe.write_without_pushback w (sw,x,m); x)

  (* Controller helper functions *)
  let rec port_stats_loop () : unit Deferred.t =   
    Hashtbl.Poly.iter
      global_state.network 
      ~f:(fun ~key:sw ~data:sw_state -> 
          List.iter
            sw_state.ports
            ~f:(fun p -> 
                let m = Message.StatsRequestMsg (PortRequest (Some (PhysicalPort p))) in 
                let _ = send_msg (sw, m) in
                ()));
    after (Time.Span.of_sec port_stats_delay) >>= fun () -> 
    port_stats_loop ()

  let send (sw, x, msg) =
    Controller.send sw x msg >>= function 
    | `Eof -> return ()
    | `Ok -> return ()

  let safe_add hash k v f = 
    match Hashtbl.Poly.find hash k with 
    | None -> Hashtbl.Poly.add_exn hash k v 
    | Some v' -> Hashtbl.Poly.set hash k (f v v')

  let handler flow_hash evt =
    match evt with
    | `Connect (sw, feats) -> 
       begin 
         Printf.printf "switch %Ld connected" sw;
         (* Save global state *)
         let sw_state = 
           { ports = List.map feats.SwitchFeatures.ports ~f:(fun pd -> pd.port_no);
             flows = 
               (match Hashtbl.Poly.find flow_hash sw with 
                | None -> []
                | Some flows -> flows) } in 
         safe_add global_state.network sw sw_state (fun x _ -> x);
         (* Propagate state to network *)  
         send (sw, xid (), FlowModMsg delete_all_flows) >>= fun () ->
         send (sw, xid (), BarrierRequest) >>= fun () ->
         Deferred.all_unit
           (List.map sw_state.flows
                     (fun flow -> send (sw, xid (), FlowModMsg flow)))
       end
    | `Disconnect sw -> 
       verbose (Printf.sprintf "switch %Ld disconnected" sw);
       Hashtbl.Poly.remove global_state.network sw;
       return ()
    | `Message(sw,_,StatsReplyMsg (PortRep psl as rep)) ->
       verbose (Printf.sprintf "stats from %Ld: %s" sw (reply_to_string rep));      
       let time = Kulfi_Time.time () in 
       List.iter
         psl
         ~f:(fun ps -> safe_add global_state.stats (sw,ps.port_no) [(time, ps)] (@));
       return ()
    | `Message(_,_,msg) -> 
       return ()
      
  let start topo_fn predict_fn host_fn () =
    (* Parse topology *)
    let topo = Frenetic_Network.Net.Parse.from_dotfile topo_fn in
    (* Create fabric *)
    let flow_hash,tag_hash = Kulfi_Fabric.create topo in
    (* Open predicted demands *)
    let (predict_host_map, predict_traffic_ic) = open_demands predict_fn host_fn topo in
    (* Helper to generate host configurations *)
    let rec simulate i = 
      try 
	let predict = next_demand predict_traffic_ic predict_host_map in
	let scheme = Solver.solve topo predict SrcDstMap.empty in
	print_configuration topo (configuration_of_scheme topo scheme tag_hash) i;
	simulate (i+1)
      with _ -> 
	() in 
    let open Deferred in
    (* Main code *)
    Printf.eprintf "[Kulfi: generating configurations]\n%!";
    simulate 0;
    Printf.eprintf "[Kulfi: starting controller]\n%!";
    Controller.init 6633;
    Printf.eprintf "[Kulfi: running...]\n%!";
    don't_wait_for (cli ());
    don't_wait_for (port_stats_loop ());
    don't_wait_for (Pipe.iter Controller.events (handler flow_hash));
    don't_wait_for (Pipe.iter msgs send);
    ()    
end
