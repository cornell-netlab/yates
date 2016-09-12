open Kulfi_Types

open Frenetic_Network
open Net
open Core.Std

let reverse_edge_exn topo e = match Topology.inverse_edge topo e with
          | Some x -> x
          | None -> assert false

(**************** Failures ************)
let bidir_failure topo e =
  let e' = reverse_edge_exn topo e in
  EdgeSet.add (EdgeSet.singleton e) e'

let update_topo_with_failure (t:topology) (f:failure) : topology =
  EdgeSet.fold f ~init:t
    ~f:(fun acc link -> Topology.remove_edge acc link)

(* Solve SPF *)
let solve_spf (topo:topology) (d:demands) : scheme =
  let device v = let lbl = Topology.vertex_to_label topo v in (Node.device lbl) in
  let apsp = NetPath.all_pairs_shortest_paths ~topo:topo
    ~f:(fun x y ->
          (match (device x, device y) with | (Node.Host,Node.Host) -> true | _ -> false)
    ) in
  List.fold_left apsp ~init:SrcDstMap.empty ~f:(fun acc (c,v1,v2,p) ->
    SrcDstMap.add acc ~key:(v1,v2) ~data:( PathMap.singleton p 1.0) )


(* check all-pairs connectivity after failure *)
let check_connectivity_after_failure (topo:topology) (fail:failure) : bool =
  let topo' = update_topo_with_failure topo fail in
  let hosts = get_hosts topo' in
  solve_spf topo' SrcDstMap.empty
    |> all_pairs_connectivity topo' hosts


(* Compute all possible failure scenarios with num_failures link failing, while not partitioning the network *)
let get_all_possible_failures (topo:topology) (num_failures:int) : (failure List.t) =
  (* List of single link failures *)
  let all_single_failures =
    EdgeSet.fold (Topology.edges topo) ~init:[]
      ~f:(fun acc e ->
        if not (edge_connects_switches e topo) then acc
        else
          let fl = bidir_failure topo e in
          if List.mem ~equal:EdgeSet.equal acc fl then acc
          else fl::acc)
    |> List.filter ~f:(fun fl -> check_connectivity_after_failure topo fl) in
  List.fold_left (range 1 num_failures) ~init:all_single_failures
    ~f:(fun partial_acc i ->
      List.fold_left partial_acc ~init:[]
        ~f:(fun acc partial_fl ->
          List.fold_left all_single_failures ~init:acc
            ~f:(fun acc single_fl ->
              if EdgeSet.subset single_fl partial_fl then acc
              else
                let new_failure = EdgeSet.union partial_fl single_fl in
                if check_connectivity_after_failure topo new_failure then new_failure::acc
                else acc)))
