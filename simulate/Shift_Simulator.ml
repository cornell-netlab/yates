open Core.Std
open Frenetic_Network
open Net

open Kulfi_Globals
open Kulfi_Routing
open Kulfi_Routing_Util
open Kulfi_Shift
open Kulfi_Traffic
open Kulfi_Types
open Kulfi_Util
open Simulate_Demands
open Simulate_TM
open Simulation_Types
open Simulation_Util

let simulate
    (topology_file:string)
    (demand_file:string)
    (host_file:string)
    (num_tms_opt:int option)
    (scale:float) () : unit =

  let topo = Parse.from_dotfile topology_file in
  let demand_lines_length = List.length (In_channel.read_lines demand_file) in
  let num_tms = demand_lines_length in

  (* TODO: get this as input *)
  let crashed_dc = VertexSet.choose_exn (get_hosts_set topo) in
  let crashed_router = ingress_switch topo crashed_dc in
  Printf.printf "(%s,%s)"
    (Node.name (Net.Topology.vertex_to_label topo crashed_dc))
    (Node.name (Net.Topology.vertex_to_label topo crashed_router));

  let topo' = Topology.remove_vertex
      (Topology.remove_vertex topo crashed_dc) crashed_router in

  let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in
  let shifted_tms =
    List.fold_left (range 0 num_tms)
      ~init:[]
      ~f:(fun acc _ ->
          let tm = next_demand ~scale:scale actual_ic actual_host_map in
          let shifted_tm = NS.model topo' crashed_dc tm in
          shifted_tm::acc)
    |> List.rev in
  close_demands actual_ic;

  let algorithm = Mcf in
  let _ =
    List.fold_left shifted_tms
      ~init:SrcDstMap.empty
      ~f:(fun prev_scheme tm ->
          if (SrcDstMap.is_empty prev_scheme) then initialize_scheme algorithm topo' tm;
          let scheme,_ = solve_within_budget algorithm topo' tm tm in
          let cmax = congestion_of_paths topo' tm scheme
                |> EdgeMap.to_alist
                |> List.map ~f:snd
                |> get_max_congestion in
          Printf.printf "Max exp congestion: %f\n" cmax;
          scheme) in

  Printf.printf "\nComplete.\n"

let command =
  Command.basic
    ~summary:"Simulate traffic shift models"
    Command.Spec.(
    empty
    +> flag "-budget" (optional_with_default (Int.max_value/100) int)
      ~doc:" max paths between each pair of hosts"
    +> flag "-scale" (optional_with_default 1. float)
      ~doc:" scale demands by this factor"
    +> flag "-num-tms" (optional int)
      ~doc:" number of TMs (-robust overrides this)"
    +> anon ("topology-file" %: string)
    +> anon ("demand-file" %: string)
    +> anon ("host-file" %: string)
  ) (fun
    (budget:int)
    (scale:float)
    (num_tms:int option)
    (topology_file:string)
    (demand_file:string)
    (host_file:string) () ->
      Kulfi_Globals.deloop := true;
      Kulfi_Globals.budget := budget;
      simulate topology_file demand_file host_file num_tms scale ())

let main = Command.run command

let _ = main
