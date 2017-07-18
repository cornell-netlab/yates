open Core
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

type shiftmodel_type = | NS | SD

let shiftmodel_to_string (s:shiftmodel_type) : string =
  match s with
  | NS -> "Naive shift"
  | SD -> "Source-Destination based"

let select_model (s:shiftmodel_type) = match s with
  | NS -> Kulfi_Shift.NS.model
  | SD -> Kulfi_Shift.SD.model

let simulate
    (topology_file:string)
    (demand_file:string)
    (host_file:string)
    (crashed_dc_name:string)
    (num_tms_opt:int option)
    (scale:float) () : unit =

  let topo = Parse.from_dotfile topology_file in
  let demand_lines_length = List.length (In_channel.read_lines demand_file) in
  let num_tms = demand_lines_length in

  let crashed_dc_opt = Topology.fold_vertexes (fun v acc ->
      match acc with
      | None ->
        let label = Topology.vertex_to_label topo v in
        if Node.device label = Node.Host &&
           Node.name label = crashed_dc_name then
          Some v
        else
          None
      | _ -> acc) topo None in
  let crashed_dc = match crashed_dc_opt with
    | None -> failwith "Could not find specified host!"
    | Some x -> x in
  let crashed_router = ingress_switch topo crashed_dc in
  Printf.printf "(%s,%s)\n"
    (Node.name (Net.Topology.vertex_to_label topo crashed_dc))
    (Node.name (Net.Topology.vertex_to_label topo crashed_router));

  let topo' = Topology.remove_vertex
      (Topology.remove_vertex topo crashed_dc) crashed_router in

  List.iter [NS; SD] ~f:(fun shiftmodel ->
      (* Read real traffic matrices *)
      let (actual_host_map, actual_ic) = open_demands demand_file host_file topo in

      (* Create traffic matrices based on selected shofting algorithm *)
      let shifted_tms =
        List.fold_left (range 0 num_tms)
          ~init:[]
          ~f:(fun acc _ ->
              let tm = next_demand ~scale:scale actual_ic actual_host_map in
              let model = select_model shiftmodel in
              let shifted_tm = model topo' crashed_dc tm in
              shifted_tm::acc)
        |> List.rev in
      close_demands actual_ic;

      (* Evaluate perf on shifted traffic matrices with some TE approach *)
      let algorithm = Mcf in
      let _ =
        List.fold_left shifted_tms
          ~init:SrcDstMap.empty
          ~f:(fun prev_scheme tm ->
              if (SrcDstMap.is_empty prev_scheme) then
                initialize_scheme algorithm topo' tm;
              let scheme,_ = solve_within_budget algorithm topo' tm tm in
              let cmax = congestion_of_paths topo' tm scheme
                         |> EdgeMap.to_alist
                         |> List.map ~f:snd
                         |> get_max_congestion in
              Printf.printf "Max exp congestion: %f\n" cmax;
              scheme) in
      Printf.printf "\n");
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
    +> anon ("dc" %: string)
  ) (fun
    (budget:int)
    (scale:float)
    (num_tms:int option)
    (topology_file:string)
    (demand_file:string)
    (host_file:string)
    (dc:string)
    () ->
      Kulfi_Globals.deloop := true;
      Kulfi_Globals.budget := budget;
      simulate topology_file demand_file host_file dc num_tms scale ())

let main = Command.run command

let _ = main
