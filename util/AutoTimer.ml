open Core.Std
       
type time = {
  time : float;
  stop : float;
}

let make_auto_timer () : time =
  { time = 0.0 ; stop = 0.0 ; }

let start (at:time) : time =
  { at with time = Unix.gettimeofday() ; }

let stop (at:time) : time =
  { at with stop = Unix.gettimeofday () ; }

let get_time_in_seconds (at:time) : float =
  at.stop -. at.time
