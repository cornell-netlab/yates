(* Experiments that we want to run with the Simulator *)

type iter_vs_time = { iteration : int ; time : float; time_dev : float; }

let iter_vs_time_to_string (r:iter_vs_time) : string =		      
  Printf.sprintf "%d\t%f\t%f" r.iteration r.time r.time_dev
