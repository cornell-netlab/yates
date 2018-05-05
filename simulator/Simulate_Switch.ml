open Core
open Yates_types.Types

let is_nan x =
  Float.classify x = Float.Class.Nan

(* Calculate fair share of flows *)
let fair_share_at_edge (capacity:float) (in_flows: float PathMap.t) : (float PathMap.t) =
  let path_dem_list = PathMap.to_alist in_flows in
  let sorted_pdlist = List.sort ~compare:(fun x y -> Float.compare (snd x) (snd y)) path_dem_list in
  (*ignore (List.iter sorted_pdlist ~f:(fun (_,d) -> Printf.printf "%f\n%!" d;); Printf.printf "\n";);
    assert false;*)
  let (fair_share,_,_) = List.fold_left sorted_pdlist
      ~init:(PathMap.empty, capacity, List.length sorted_pdlist)
      ~f:(fun acc (p,d) ->
        let (curr_share, spare_cap, n_rem_flows) = acc in
        if d *. (Float.of_int n_rem_flows) <= spare_cap then
          let new_share = PathMap.set curr_share ~key:p ~data:d in
          (new_share, spare_cap -. d, n_rem_flows - 1)
        else
          let fs = (spare_cap /. (Float.of_int n_rem_flows)) in
          if is_nan fs then assert false;
          let new_share = PathMap.set curr_share ~key:p ~data:fs in
          (new_share, spare_cap -. fs, n_rem_flows - 1)) in
  fair_share


(* Calculate fair share of flows *)
let fair_share_at_edge_arr (capacity:float) (in_flows: (edge Array.t * int * float) List.t) : ((edge Array.t * int * float) List.t) =
  let path_dem_list = Array.of_list in_flows in
  let _ = Array.sort ~compare:(fun (_,_,x) (_,_,y) -> Float.compare x y) path_dem_list in
  (*ignore (List.iter sorted_pdlist ~f:(fun (_,d) -> Printf.printf "%f\n%!" d;); Printf.printf "\n";);
    assert false;*)
  let (fair_share,_,_) = Array.foldi
      path_dem_list
      ~init:([], capacity, Array.length path_dem_list)
      ~f:(fun _ acc (p,dist,d) ->
        let (curr_share, spare_cap, n_rem_flows) = acc in
        if d *. (Float.of_int n_rem_flows) <= spare_cap then
          let new_share =  (p,dist,d)::curr_share in
          (new_share, spare_cap -. d, n_rem_flows - 1)
        else
          let fs = (spare_cap /. (Float.of_int n_rem_flows)) in
          if is_nan fs then assert false;
          let new_share =  (p,dist,fs)::curr_share in
          (new_share, spare_cap -. fs, n_rem_flows - 1))  in
  fair_share

