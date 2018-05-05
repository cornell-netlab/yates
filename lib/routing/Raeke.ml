open Core

open Util
open Yates_Frt
open Yates_Mw
open Yates_Rrt
open Yates_types.Types

(* multiplicative weights input *)
module MWInput : MW_INPUT with type structure = FRT.routing_tree = struct

  type structure = FRT.routing_tree

  let select_structure (topo : topology) (_:demands) (nodes : Topology.VertexSet.t) =
    (* First, make an FRT tree decomposition from the topology. *)
    let tree = FRT.make_frt_tree topo in
    let node_list = Topology.VertexSet.elements nodes in
    (FRT.generate_rt topo tree node_list, 1.)

  let usage_of_structure (_ : topology) (_:demands) (st : FRT.routing_tree) =
    FRT.usage_of_tree st

  let set_weight topo edge w =
    let label = Topology.edge_to_label topo edge in
    Link.set_weight label w; topo

  let get_weight topo edge =
    let label = Topology.edge_to_label topo edge in
    Link.weight label

end

(* multiplicative weights instantiation *)
module RRTs : MW_ALG with type structure = FRT.routing_tree = Yates_Mw.Make (MWInput)

let prev_scheme = ref SrcDstMap.empty

let solve (topo:topology) (d:demands) : scheme =
  let new_scheme =
    if SrcDstMap.is_empty !prev_scheme then
      let hosts = get_hosts_set topo in
      let t =
        if !Globals.er_mode then
          (* Avoid traffic to go through other edge routers or hosts *)
          (* Remove all hosts from topology *)
          VertexSet.fold hosts ~init:topo
            ~f:(fun acc h -> Topology.remove_vertex acc h)
        else topo in

      let end_points =
        if !Globals.er_mode then
          (* Make switches as end-points *)
          Topology.vertexes t
        else hosts in

      let epsilon = 0.1 in
      let _,mw_solution,_ = RRTs.hedge_iterations epsilon t d end_points in
      let paths src dst : probability PathMap.t =
        List.fold_left mw_solution
          ~init:PathMap.empty
          ~f:(fun acc (rt,p) ->
            (* TODO(jnf,soule): should FRT export a function to directly
               compute the physical path? Seems like it... *)
            let routing_path = FRT.get_path rt src dst in
            let physical_path = FRT.path_to_physical rt routing_path in
            let physical_path' =
              if !Globals.deloop then
                Yates_Frt.FRT.remove_cycles physical_path
              else
                physical_path in
            add_or_increment_path acc physical_path' p) in
      let endpt_scheme = Topology.VertexSet.fold
        end_points
        ~init:SrcDstMap.empty
        ~f:(fun acc src ->
          Topology.VertexSet.fold
            end_points
            ~init:acc
            ~f:(fun acc dst ->
              if src <> dst then
                SrcDstMap.set acc ~key:(src, dst) ~data:(paths src dst)
              else
                SrcDstMap.set acc ~key:(src, dst) ~data:(PathMap.singleton [] 1.0))) in
      let sch =
        if !Globals.er_mode then
          (* Convert switch-switch scheme to host-host *)
          VertexSet.fold hosts
            ~init:SrcDstMap.empty
            ~f:(fun acc src ->
                VertexSet.fold hosts
                  ~init:acc
                  ~f:(fun acc dst ->
                      if src <> dst then
                        let ingress_links = outgoing_edges topo src in
                        let egress_links = incoming_edges topo dst in
                        let pps = List.fold_left ingress_links
                            ~init:PathMap.empty
                            ~f:(fun pps_acc in_link ->
                                List.fold_left egress_links ~init:pps_acc
                                  ~f:(fun pps_acc eg_link ->
                                      let in_sw = fst (Topology.edge_dst in_link) in
                                      let eg_sw = fst (Topology.edge_src eg_link) in
                                      let tmp_pps = SrcDstMap.find endpt_scheme
                                          (in_sw, eg_sw) in
                                      match tmp_pps with
                                      | None -> pps_acc
                                      | Some tmp_pps ->
                                        PathMap.fold tmp_pps ~init:pps_acc
                                          ~f:(fun ~key:sw_path ~data:prob acc ->
                                              let path = in_link::(sw_path@[eg_link]) in
                                              add_or_increment_path acc path prob))) in
                        SrcDstMap.set acc ~key:(src, dst) ~data:pps
                      else
                        acc))
        else
          endpt_scheme in
      normalize_scheme sch
    else !prev_scheme in
  prev_scheme := new_scheme;
  new_scheme

let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

let local_recovery = normalization_recovery
