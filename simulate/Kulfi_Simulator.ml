open Core.Std
open Frenetic_Network
open Net
       
open Kulfi_Routing
open Kulfi_Types
open Kulfi_Frt
open Kulfi_Mw
open Kulfi_Rrt
open Simulate_LP
open Simulate_Demands

       
let () = Random.self_init ()

let _ = Simulate_Demands.create_random

let solve_n_iterations total_n topo hosts demand_model =
  let before_hedge = Unix.gettimeofday () in
  let (num_trees, oblivious_sol, _) = RRTs.hedge_iterations 0.1
      topo hosts in
  let after_hedge = Unix.gettimeofday () in
  let hedge_time = after_hedge -. before_hedge in
  Printf.printf "Oblivious solution computed in %f seconds\n" hedge_time;
  Printf.printf "# trees = %d\n" num_trees;
  print_newline ();
  let rec loop n model prev_paths diffs path_set =
    if n > total_n then (diffs, path_set)
    else
      begin
        Printf.printf "Running iteration %d...\n" n;
        flush stdout;
        let demands = Simulate_Demands.get_demands demand_model in
        let before_lp = Unix.gettimeofday () in
        let (lp_ratio,paths,num_paths) = Simulate_LP.solver_paths topo demands false in
        let after_lp = Unix.gettimeofday () in
        let lp_time = after_lp -. before_lp in

        Printf.printf "LP solved in %f seconds\n" lp_time;
        Printf.printf "LP # paths = %d\n" num_paths;
        Printf.printf "LP ratio = %f\n" lp_ratio;

        let before_obliv = Unix.gettimeofday () in
        let (obliv_ratio, obliv_num_paths) = load_from_demands topo hosts
            oblivious_sol demands in
        let after_obliv = Unix.gettimeofday () in
        let obliv_time = after_obliv -. before_obliv in

        Printf.printf "Obliv demands projected in %f seconds\n" obliv_time;
        Printf.printf "Obliv # paths = %d\n" obliv_num_paths;
        Printf.printf "Obliv ratio = %f\n" obliv_ratio;

        Printf.printf "Approximation ratio = %f\n" (obliv_ratio /. lp_ratio);

        let new_paths_set = List.fold_left (fun acc (_,_,st_paths) ->
            List.fold_left (fun acc2 (path, weight) ->
                let name = Simulate_LP.names_of_path topo path in
                StringListSet.add name acc2) acc st_paths)
            StringListSet.empty paths in
        let combined_set = StringListSet.union new_paths_set path_set in

        Printf.printf "Cumulative LP paths used = %d\n"
          (StringListSet.cardinal combined_set);

        let new_diff = if StringListSet.is_empty prev_paths then 0
          else sym_diff new_paths_set prev_paths in
        Printf.printf "LP paths diff (churn) = %d\n" new_diff;
        print_newline ();
        let next_demands = Demands.update model in
        loop (n+1) next_demands new_paths_set (new_diff::diffs) combined_set
      end in
  let diffs, path_set = loop 1 demand_model StringListSet.empty []
      StringListSet.empty in
  let sum_diffs = List.fold_left (+) 0 diffs in
  let average_diff = (float sum_diffs) /. (float (List.length diffs)) in
  let num_paths = StringListSet.cardinal path_set in
  (average_diff, num_paths)

                    
let () =
  print_endline "Kulfi Simulator";
  if (Array.length Sys.argv <> 3) then
    (Printf.printf "usage: %s [dot-file] [num-iterations]\n" Sys.argv.(0))
  else
    let graph_file = Sys.argv.(1) in
    let n = int_of_string Sys.argv.(2) in
    let topo = Parse.from_dotfile graph_file in
    let host_set = VertexSet.filter (Topology.vertexes topo)
        ~f:(fun v ->
            let label = Topology.vertex_to_label topo v in
            Node.device label = Node.Host) in
    let hosts = Topology.VertexSet.elements host_set in
    let demand_matrix = Simulate_Demands.create_sparse hosts 0.1 100 in
    let pairs = Simulate_Demands.get_demands demand_matrix in
    Printf.printf "# hosts = %d\n" (Topology.VertexSet.length host_set);
    Printf.printf "# pairs = %d\n" (List.length pairs);
    Printf.printf "# total vertices = %d\n" (Topology.num_vertexes topo);
    (* let _ = Solver.solve topo pairs SrcDstSet.empty in *)

    let avg_diff, num_paths = solve_n_iterations n topo host_set demand_matrix in
    Printf.printf "Average LP diff = %f\n" avg_diff;
    Printf.printf "Cumulative num LP paths = %d\n" num_paths;
    flush stdout;
