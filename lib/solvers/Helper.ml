open Core

open Yates_routing.Util
open Yates_types.Types

(* compute routing schemes for each link failure and merge the schemes *)
let all_failures_envelope solver (topo:topology) (envelope:demands) : scheme =
  let hosts = get_hosts topo in
  let failure_scheme_map,_ = EdgeSet.fold (Topology.edges topo)
    ~init:(EdgeMap.empty, EdgeSet.empty)
    ~f:(fun (acc, handled_edges) e ->
      if (EdgeSet.mem handled_edges e) then (acc, handled_edges)
      else
        let e' = match Topology.inverse_edge topo e with
          | None ->
              Printf.printf "%s\n%!" (string_of_edge topo e);
              failwith "No reverse edge found"
          | Some x -> x in
        let handled_edges = EdgeSet.add (EdgeSet.add handled_edges e) e' in
        let failure_scen = EdgeSet.add (EdgeSet.singleton e) e' in
        if edge_connects_switches e topo then
          let topo' = EdgeSet.fold failure_scen
            ~init:topo
            ~f:(fun acc link -> Topology.remove_edge acc link) in
          (* consider only the failures which do not partition the network *)
          let spf_scheme = Yates_routing.Spf.solve topo' SrcDstMap.empty in
          if all_pairs_connectivity topo' hosts spf_scheme then
            begin
            let sch = solver topo' envelope in
            assert (not (SrcDstMap.is_empty sch));
            (EdgeMap.set ~key:e ~data:sch acc, handled_edges)
            end
          else (acc, handled_edges)
        else (acc, handled_edges)) in
  (* merge all the schemes *)
  assert (not (EdgeMap.is_empty failure_scheme_map));
  let agg_scheme = EdgeMap.fold failure_scheme_map
    ~init:SrcDstMap.empty
    ~f:(fun ~key:e ~data:edge_scheme agg ->
      (* merge edge_schme and agg *)
      assert (not (SrcDstMap.is_empty edge_scheme));
      SrcDstMap.fold edge_scheme
        ~init:agg
        ~f:(fun ~key:(s,d) ~data:f_pp_map res ->
          if s = d then res else
          let acc_pp_map = match SrcDstMap.find res (s,d) with
            | None -> PathMap.empty
            | Some x -> x in
          if (PathMap.is_empty f_pp_map) then
            begin
              Printf.printf "sd: (%s, %s) Edge: %s: Scheme %s"
                (string_of_vertex topo s) (string_of_vertex topo d)
                (string_of_edge topo e) (dump_scheme topo edge_scheme);
              assert false
            end
          else
          let n_pp_map = PathMap.fold f_pp_map
            ~init:acc_pp_map
            ~f:(fun ~key:path ~data:f_prob acc_paths ->
              let acc_prob = match PathMap.find acc_paths path with
                | None -> 0.
                | Some x -> x in
              PathMap.set ~key:path ~data:(acc_prob +. f_prob) acc_paths) in
          SrcDstMap.set ~key:(s,d) ~data:n_pp_map res)) in
  (* normalize scheme *)
  assert (not (SrcDstMap.is_empty agg_scheme));
  normalize_scheme_opt  agg_scheme


