(* Experiments that we want to run with the Simulator *)
open Kulfi_Types
open Frenetic_Network

type iter_vs_time = { iteration : int ; time : float; time_dev : float; }

let iter_vs_time_to_string (r:iter_vs_time) : string =		      
  Printf.sprintf "%d\t%f\t%f" r.iteration r.time r.time_dev

type iter_vs_congestion = { iteration : int ; congestion : float; congestion_dev : float; }		 

let iter_vs_congestion_to_string (r:iter_vs_congestion) : string =		      
  Printf.sprintf "%d\t%f\t%f" r.iteration r.congestion r.congestion_dev

type iter_vs_churn = { iteration : int ; churn : float; churn_dev : float; }		 

let iter_vs_churn_to_string (r:iter_vs_churn) : string =		      
  Printf.sprintf "%d\t%f\t%f" r.iteration r.churn r.churn_dev

type iter_vs_num_paths = { iteration : int ; num_paths : float; num_paths_dev : float; }		 

let iter_vs_num_paths_to_string (r:iter_vs_num_paths) : string =		      
  Printf.sprintf "%d\t%f\t%f" r.iteration r.num_paths r.num_paths_dev

type iter_vs_edge_congestions = { iteration : int ; edge_congestions : float EdgeMap.t; }		 

let iter_vs_edge_congestions_to_string (topo:topology) (r:iter_vs_edge_congestions) : string =
  Printf.sprintf "%d\t" r.iteration ^ 
  EdgeMap.fold ~init:"" ~f:(fun ~key:e ~data:c acc -> acc ^ "\t" ^ "(" ^
    (Node.name (Net.Topology.vertex_to_label topo (fst (Net.Topology.edge_src e)))) ^ "," ^
    (Node.name (Net.Topology.vertex_to_label topo (fst (Net.Topology.edge_dst e)))) ^ ") : " ^
    string_of_float c) r.edge_congestions
