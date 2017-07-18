open Core
open Async
open Frenetic_Network
open Net
open Net.Topology
open Kulfi_Types
open Kulfi_Util

let drop : Frenetic_OpenFlow0x01.flowMod =
  let open Frenetic_OpenFlow0x01 in
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

let mk_flow_mod (tag:int) (out:int) : Frenetic_OpenFlow0x01.flowMod =
  let open Frenetic_OpenFlow0x01 in
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
  let actions = [ SetDlVlan None; Output (PhysicalPort out) ] in
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

let create (t:topology) =
  let tag_hash = Hashtbl.Poly.create () in
  let flow_hash = Hashtbl.Poly.create () in
  iter_edges
    (fun edge ->
      let src, port = edge_src edge in
      let lbl = vertex_to_label t src in
      match Node.device lbl with
        | Node.Switch ->
          begin
            let sw = Node.id lbl in
            let tag = !tag_cell in
            incr tag_cell;
            Hashtbl.Poly.add_exn tag_hash edge tag;
            Core.printf "LINK: %s -> %d\n" (dump_edges t [edge]) tag;
            let flow_mod = mk_flow_mod tag (Int32.to_int_exn port) in
            match Hashtbl.Poly.find flow_hash sw with
              | None ->
                Hashtbl.Poly.add_exn flow_hash sw [flow_mod; drop]
              | Some flow_mods ->
                Hashtbl.Poly.set flow_hash sw (flow_mod::flow_mods)
          end
        | _ ->
          ()) t;
  (flow_hash, tag_hash)
