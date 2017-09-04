open Core
open Frenetic_Network
open Net
open Kulfi_Types
open Kulfi_Util
open Simulate_Switch
open Simulate_TM
open Simulation_Types

let create_3cycle_input () =
  Kulfi_Globals.deloop := true;
  let topo = Parse.from_dotfile "./data/topologies/3cycle.dot" in
  let hosts = get_hosts topo in
  let demands =
    List.fold_left
      hosts
      ~init:SrcDstMap.empty
      ~f:(fun acc u ->
          List.fold_left
            hosts
            ~init:acc
            ~f:(fun acc v ->
                let r = if u = v then 0.0 else 536870912. in
                SrcDstMap.add acc ~key:(u,v) ~data:r)) in
  (hosts,topo,demands)

let create_6s4h_input () =
  Kulfi_Globals.deloop := true;
  let topo = Parse.from_dotfile "./data/topologies/6s4hMultiSwitch.dot" in
  let hosts = get_hosts topo in
  let demands =
    List.fold_left
      hosts
      ~init:SrcDstMap.empty
      ~f:(fun acc u ->
          List.fold_left
            hosts
            ~init:acc
            ~f:(fun acc v ->
                let r = if u = v then 0.0 else 536870912. in
                SrcDstMap.add acc ~key:(u,v) ~data:r)) in
  (hosts,topo,demands)

let test_max_congestion (sch : scheme) (topo : topology) (dem : demands)
    (algorithm : solver_type) (exp_cmax : float) : bool =
  let sim_stats = simulate_tm sch topo dem EdgeSet.empty dem algorithm false 0.
      (List.hd_exn (get_hosts topo)) in
  let _, list_of_max_congestions =
    List.map ~f:snd (EdgeMap.to_alist sim_stats.congestion)
    |> split_alist in
  let cmax = get_max_congestion list_of_max_congestions in
  cmax =. exp_cmax

(********** Budget tests ***********)
let check_budget (s:scheme) (n:int) : bool =
  SrcDstMap.fold
    s
    ~init:true
    ~f:(fun ~key:(u,v) ~data:f_decomp acc ->
      if u = v then acc
      else let num_paths = PathMap.length f_decomp in
      acc && (num_paths <= n))

let test_budget_raeke () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let scheme = prune_scheme topo (Kulfi_Raeke.solve topo pairs) 1 in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  check_budget scheme 1

let test_budget_mcf () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let scheme = prune_scheme topo (Kulfi_Mcf.solve topo pairs) 1 in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  check_budget scheme 1

let test_budget_semimcf_vlb () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Vlb.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Vlb.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = prune_scheme topo (Kulfi_SemiMcf.solve topo pairs) 1 in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  check_budget scheme 1

let test_capped_mcf () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let scheme = prune_scheme topo (Kulfi_Mcf_Capped.solve topo pairs) 1 in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  check_budget scheme 1

(********** Switch simulator tests ***********)
let test_fair_share () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let scheme = Kulfi_Spf.solve topo pairs in
  let v = ref 0.0 in
  let paths = SrcDstMap.fold scheme
      ~init:PathMap.empty
      ~f:(fun ~key:_ ~data:ppmap acc ->
          PathMap.fold ppmap
            ~init:acc
            ~f:(fun ~key:p ~data:_ acc ->
                match PathMap.find acc p with
                | Some x -> acc
                | None ->
                  v := !v +. 1.0;
                  PathMap.add acc ~key:p ~data:!v)) in
  assert (PathMap.length paths = 7);
  (*PathMap.iter paths ~f:(fun ~key:p ~data:v -> Printf.printf "%f " v);*)
  let fs_paths = fair_share_at_edge 20.0 paths in
  (*PathMap.iter fs_paths ~f:(fun ~key:p ~data:v -> Printf.printf "%f " v);*)
  let shares = List.sort ~cmp:Float.compare (List.map ~f:snd (PathMap.to_alist fs_paths)) in
  (*List.iter shares ~f:(fun x -> Printf.printf "%f " x);*)
  List.nth shares 0 = Some 1.0 &&
  List.nth shares 1 = Some 2.0 &&
  List.nth shares 2 = Some 3.0 &&
  List.nth shares 3 = Some 3.5 &&
  List.nth shares 4 = Some 3.5 &&
  List.nth shares 5 = Some 3.5 &&
  List.nth shares 6 = Some 3.5

(********** Routing algorithm tests ***********)
let test_ac () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_AC.initialize SrcDstMap.empty;
  let scheme = Kulfi_AC.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs Ac (2./.3.)

let test_ak_ksp () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Ksp.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Ksp.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_ak_mcf () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let start_scheme = Kulfi_Mcf.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_ak_raeke () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Raeke.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_ak_vlb () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Vlb.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Vlb.solve topo pairs in
  Kulfi_Ak.initialize start_scheme;
  let scheme = Kulfi_Ak.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_apsp () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let paths = Frenetic_Network.NetPath.all_pairs_shortest_paths
      ~topo:topo ~f:(fun _ _ -> true) in
  List.fold_left hosts ~init:true
    ~f:(fun acc u ->
        List.fold_left hosts
          ~init:acc
          ~f:(fun acc v ->
              if u = v then acc
              else
                acc &&
                List.exists paths (fun (_,v1,v2,_) -> v1 = u && v2 = v)))

let test_ecmp () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Ecmp.initialize SrcDstMap.empty;
  let scheme = Kulfi_Ecmp.solve topo pairs in
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs Ecmp 0.5


let test_edksp () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Edksp.initialize SrcDstMap.empty;
  let scheme = Kulfi_Edksp.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs Edksp 0.75

let test_ffc () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Ksp.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Ksp.solve topo pairs in
  Kulfi_Ffc.initialize start_scheme;
  let scheme = Kulfi_Ffc.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs Ffc 0.75

let test_ffced () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Edksp.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Edksp.solve topo pairs in
  Kulfi_Ffc.initialize start_scheme;
  let scheme = Kulfi_Ffc.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs Ffced 0.75

let test_ksp () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Ksp.initialize SrcDstMap.empty;
  let scheme = Kulfi_Ksp.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs Ksp 0.75

let test_mcf () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let scheme = Kulfi_Mcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs Mcf 0.5

let test_mwmcf () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let scheme = Kulfi_Mcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_mw () = false

let test_raeke () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let scheme = Kulfi_Raeke.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_semimcf_ac () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_AC.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_AC.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_semimcf_edksp () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Edksp.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Edksp.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs SemiMcfEdksp 0.50

let test_semimcf_ksp () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Ksp.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Ksp.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme &&
  test_max_congestion scheme topo pairs SemiMcfKsp 0.50

let test_semimcf_mcf () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  let start_scheme = Kulfi_Mcf.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_semimcf_raeke () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Routing.Raeke.solve topo pairs in
  Kulfi_Routing.SemiMcf.initialize start_scheme;
  let scheme = Kulfi_Routing.SemiMcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_semimcf_vlb () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Vlb.initialize SrcDstMap.empty;
  let start_scheme = Kulfi_Vlb.solve topo pairs in
  Kulfi_SemiMcf.initialize start_scheme;
  let scheme = Kulfi_SemiMcf.solve topo pairs in
  all_pairs_connectivity topo hosts scheme &&
  probabilities_sum_to_one scheme

let test_spf () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Spf.initialize SrcDstMap.empty;
  let scheme = Kulfi_Spf.solve topo pairs in
  match hosts with
  | h1::h2::tail ->
    (* TODO(jnf,rjs): could just call sample_scheme here? *)
    let x = match SrcDstMap.find scheme (h1,h2)  with
      | None -> assert false
      | Some x -> x in
    PathMap.fold x
      ~init:true
      ~f:(fun ~key:path ~data:_ acc ->
          acc && ((List.length path) = 3) )
  | _ -> assert false

let test_vlb () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Vlb.initialize SrcDstMap.empty;
  let scheme = Kulfi_Vlb.solve topo pairs in
  match hosts with
  | h1::h2::tail ->
    let paths =
      match SrcDstMap.find scheme (h1,h2) with
      | None -> assert false
      | Some x -> x in
    (* Printf.printf "VLB set length =%d\n"  (PathMap.length paths); *)
    (* Printf.printf "%s\n" (dump_scheme topo scheme); *)
    (PathMap.length paths) = 2
  | _ -> assert false

let test_vlb2 () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Vlb.initialize SrcDstMap.empty;
  let scheme = Kulfi_Vlb.solve topo pairs in
  List.fold_left hosts ~init:true
    ~f:(fun acc u ->
        List.fold_left hosts ~init:acc
          ~f:(fun acc v ->
              if u = v then true && acc
              else
                match SrcDstMap.find scheme (u,v) with
                | None -> false
                | Some paths -> not (PathMap.is_empty paths) && acc))

let test_vlb3 () =
  let (hosts,topo,pairs) = create_3cycle_input () in
  Kulfi_Vlb.initialize SrcDstMap.empty;
  let scheme = Kulfi_Vlb.solve topo pairs in
  paths_are_nonempty scheme

let test_host_not_in_path () =
  let (hosts,topo,pairs) = create_6s4h_input () in
  let check_hosts s =
    SrcDstMap.fold s ~init:true
      ~f:(fun ~key:_ ~data:paths acc ->
          PathMap.fold paths ~init:acc
            ~f:(fun ~key:path ~data:_ acc ->
                match path with
                | [] -> acc
                | hd::edges ->
                  List.fold_left ~init:acc ~f:(&&)
                    (List.map edges
                       (fun e ->
                         Node.device (Topology.vertex_to_label topo (fst (Topology.edge_src e))) = Node.Switch)
                      ))) in

  Kulfi_Globals.er_mode := true;
  let spf_scheme = Kulfi_Spf.solve topo pairs in
  Kulfi_Ecmp.initialize SrcDstMap.empty;
  let ecmp_scheme = Kulfi_Ecmp.solve topo pairs in
  Kulfi_Ksp.initialize SrcDstMap.empty;
  let ksp_scheme = Kulfi_Ksp.solve topo pairs in
  Kulfi_Mcf.initialize SrcDstMap.empty;
  let mcf_scheme = Kulfi_Mcf.solve topo pairs in
  Kulfi_Vlb.initialize SrcDstMap.empty;
  let vlb_scheme = Kulfi_Vlb.solve topo pairs in
  (* Keep Raecke at the end, or reset the topology *)
  Kulfi_Raeke.initialize SrcDstMap.empty;
  let raeke_scheme = Kulfi_Raeke.solve topo pairs in
  Kulfi_Globals.er_mode := false;
  check_hosts spf_scheme && check_hosts ecmp_scheme && check_hosts ksp_scheme
  && check_hosts mcf_scheme && check_hosts raeke_scheme && check_hosts vlb_scheme

(******* Declare all tests to be performed ***********)

let%test "ac" = test_ac ()
let%test "ak_ksp" = test_ak_ksp ()
let%test "ak_mcf" = test_ak_mcf ()
let%test "ak_raeke" = test_ak_raeke ()
let%test "ak_vlb" = test_ak_vlb ()
let%test "apsp" = test_apsp ()
let%test "ecmp" = test_ecmp ()
let%test "edksp" = test_edksp ()
let%test "ffc" = test_ffc ()
let%test "ffced" = test_ffced ()
let%test "ksp" = test_ksp ()
let%test "mcf" = test_mcf ()
let%test "mwmcf" = test_mwmcf ()
(* let%test "mw" = test_mw () *)
let%test "raeke" = test_raeke ()
let%test "semimcf_ac" = test_semimcf_ac ()
let%test "semimcf_edksp" = test_semimcf_edksp ()
let%test "semimcf_ksp" = test_semimcf_ksp ()
let%test "semimcf_mcf" = test_semimcf_mcf ()
let%test "semimcf_raeke" = test_semimcf_raeke ()
let%test "semimcf_vlb" = test_semimcf_vlb ()
let%test "spf" = test_spf ()
let%test "vlb" = test_vlb ()
let%test "vlb2" = test_vlb2 ()
let%test "vlb3" = test_vlb3 ()

let%test "budget_raeke" = test_budget_raeke ()
let%test "budget_mcf" = test_budget_mcf ()
let%test "budget_semimcf_vlb" = test_budget_semimcf_vlb ()

let%test "fair_share" = test_fair_share ()
let%test "host_path" = test_host_not_in_path ()
(* let%test "capped_mcf" = test_capped_mcf () *)
