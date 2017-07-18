open Core
       
type time = {
  mutable time : float;
  mutable stop : float;
}

let make_auto_timer () : time =
  { time = 0.0 ; stop = 0.0 ; }

let start (at:time) : unit =
  at.time <-  Unix.gettimeofday ()
                               
let stop (at:time) : unit =
  at.stop <-  Unix.gettimeofday()

let get_time_in_seconds (at:time) : float =
  at.stop -. at.time
