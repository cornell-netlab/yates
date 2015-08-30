
type solver_type = | Mcf | Vlb | Ecmp | Spf | Ak
let solver_mode = ref Mcf
let topo_string = ref ""
let usage = Printf.sprintf "%s [OPTIONS]" Sys.argv.(0)
let choose_solver = function
  | "mcf" -> solver_mode := Mcf
  | "vlb" -> solver_mode := Vlb
  | "ecmp" -> solver_mode := Ecmp
  | "spf" -> solver_mode := Spf
  | "ak" -> solver_mode := Ak
  | _ -> raise (Arg.Bad("Unknown solver"))

let speclist = [
  ("-s", Arg.Symbol (["mcf"; "vlb"; "ecmp"; "spf"; "ak"], 
		    choose_solver), " Choose solver");
  ("-t", Arg.Set_string topo_string, " Choose topology file")
]

let has_files () = match !topo_string with | "" -> false | _ -> true

let main =
  Arg.parse speclist print_endline usage;
  if not (has_files ()) then Printf.printf "%s" usage
  else
    Printf.printf "Kulfi Simulate"

let _ = main 

