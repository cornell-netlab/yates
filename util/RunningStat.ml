open Core

type stat = {
  mutable n : int;
  mutable old_m : float;
  mutable new_m : float;
  mutable old_s : float;
  mutable new_s : float;
}

let make_running_stat () : stat =
  {  n = 0; old_m = 0.0; new_m = 0.0; old_s = 0.0; new_s = 0.0; }

let clear (rs:stat) : unit =
  rs.n <- 0
    
(* See Knuth TAOCP vol 2, 3rd edition, page 232 *)
let push (rs:stat) (x:float) : unit =
  rs.n <- rs.n + 1;
  if (rs.n = 1) then
    begin
      rs.old_m <- x;
      rs.new_m <- x;
      rs.old_s <- 0.0;
    end
  else
    begin      
      rs.new_m <- rs.old_m +. ((x -. rs.old_m) /. Float.of_int rs.n) ;
      rs.new_s <- rs.old_s +. ((x -. rs.old_m) *. (x -. rs.new_m)) ;
      (* set up for next iterations *)
      rs.old_m <- rs.new_m;
      rs.old_s <- rs.new_s;
    end      

let get_num_data_values (rs:stat) : int =
  rs.n

let get_mean (rs:stat) : float =
  if (rs.n > 0) then rs.new_m else 0.0

let get_variance (rs:stat) : float =
  if (rs.n > 1) then  (rs.new_s /. (Float.of_int (rs.n - 1)))
  else 0.0

let get_standard_deviation (rs:stat) : float =
  sqrt (get_variance rs)

	 
