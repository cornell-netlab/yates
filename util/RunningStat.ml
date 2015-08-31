open Core.Std

type stat = {
  n : int;
  old_m : float;
  new_m : float;
  old_s : float;
  new_s : float;
}

let make_running_stat () : stat =
  {  n = 0; old_m = 0.0; new_m = 0.0; old_s = 0.0; new_s = 0.0; }

let clear (rs:stat) : stat =
  {rs with n = 0; }
    
(* See Knuth TAOCP vol 2, 3rd edition, page 232 *)
let push (rs:stat) (x:float) : stat =
  let n' = rs.n + 1 in 
  if (n' = 1) then
    { n = n';
      old_m = x;
      new_m = x;
      old_s = 0.0;
      new_s = 0.0;
    }
  else
    let m = rs.old_m +. ((x -. rs.old_m) /. Float.of_int n') in
    let s = rs.old_s +. ((x -. rs.old_m) *. (x -. m)) in
    { n = n';
      new_m = m;
      new_s = s;
      (* set up for next iterations *)
      old_m = m;
      old_s = s;
    }

let get_num_data_values (rs:stat) : int =
  rs.n

let get_mean (rs:stat) : float =
  if (rs.n > 0) then rs.new_m else 0.0

let get_variance (rs:stat) : float =
  if (rs.n > 1) then  (rs.new_s /. (Float.of_int (rs.n - 1)))
  else 0.0

let get_standard_deviation (rs:stat) : float =
  sqrt (get_variance rs)

	 
