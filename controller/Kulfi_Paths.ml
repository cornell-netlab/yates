open Core
open Async

open Kulfi_Types
open Kulfi_Util

let drop : Frenetic_kernel.OpenFlow0x01.flowMod =
  let open Frenetic_kernel.OpenFlow0x01 in
  let pattern =
    { dlSrc = None;
      dlDst = None;
      dlTyp = None;
      dlVlan = None;
      dlVlanPcp = None;
      nwSrc = None;
      nwDst = None;
      nwProto = None;
      nwTos = None;
      tpSrc = None;
      tpDst = None;
      inPort = None } in
  let actions = [ ] in
  let prio = 1 in
  { command = AddFlow
  ; pattern = pattern
  ; actions = actions
  ; priority = prio
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  ; notify_when_removed = true
  ; apply_to_packet = None
  ; out_port = None
  ; check_overlap = false
  }

(* match on tag and forward on out port *)
let mk_flow_mod_fw (tag:int) (out:int) : Frenetic_kernel.OpenFlow0x01.flowMod =
  let open Frenetic_kernel.OpenFlow0x01 in
  let pattern =
    { dlSrc = None;
      dlDst = None;
      dlTyp = None;
      dlVlan = Some (Some tag);
      dlVlanPcp = None;
      nwSrc = None;
      nwDst = None;
      nwProto = None;
      nwTos = None;
      tpSrc = None;
      tpDst = None;
      inPort = None } in
  let actions = [ Output (PhysicalPort out) ] in
  let prio = 65535 in
  { command = AddFlow
  ; pattern = pattern
  ; actions = actions
  ; priority = prio
  ; cookie = 0L
  ; idle_timeout = Permanent
  ; hard_timeout = Permanent
  ; notify_when_removed = true
  ; apply_to_packet = None
  ; out_port = None
  ; check_overlap = false
  }

let tag_cell = ref 100

(* assign a tag to a path if not seen earlier *)
let add_paths_from_scheme (scm:scheme) (path_tag_map:Tag.t PathMap.t) : Tag.t PathMap.t =
  SrcDstMap.fold scm ~init:path_tag_map ~f:(fun ~key:_ ~data:path_prob_map acc ->
    PathMap.fold path_prob_map ~init:acc ~f:(fun ~key:path ~data:_ acc ->
      match PathMap.find acc path with
      | None ->
        begin
          let tag = !tag_cell in
          incr tag_cell;
          PathMap.add acc ~key:path ~data:tag
        end
      | Some _ -> acc))

(* Create flow mods based on path-tag map. Ignore first host-switch edge.
 * For each switch, match on tag and forward *)
let create_sw_flows_map topo (path_tag_map:Tag.t PathMap.t) :
  (Frenetic_kernel.OpenFlow0x01.switchId, Frenetic_kernel.OpenFlow0x01.flowMod list) Hashtbl.t =
  let sw_flow_map = Hashtbl.Poly.create () in
  PathMap.iteri path_tag_map ~f:(fun ~key:path ~data:tag ->
    Core.printf "%d\t%s\n" tag (dump_edges topo path));
  PathMap.iteri path_tag_map ~f:(fun ~key:path ~data:tag ->
    match path with
    | _::sw_path ->
        begin
          (* ignore first edge host->switch *)
          List.iter sw_path ~f:(fun e ->
            (* TODO: remove tag at last hop? *)
            let sw,port = Topology.edge_src e in
            let sw_id =  Node.id (Topology.vertex_to_label topo sw) in
            let flow_mod = mk_flow_mod_fw tag (Int32.to_int_exn port) in
            match Hashtbl.Poly.find sw_flow_map sw_id with
              | None ->
                  Hashtbl.Poly.add_exn sw_flow_map sw_id [flow_mod; drop]
              | Some flow_mods ->
                Hashtbl.Poly.set sw_flow_map sw_id (flow_mod::flow_mods))
        end
    | _ -> ());
    sw_flow_map
