(* Experiments that we want to run with the Simulator *)

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
