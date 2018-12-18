open Core

open Yates_routing.Util
open Yates_types.Types

let source_routing_configuration_of_scheme (topo:topology) (scm:scheme)
    (tag_hash: (edge,int) Hashtbl.t) : configuration =
  SrcDstMap.fold scm
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:paths acc ->
        if src = dst then acc
        else
          let tags = PathMap.fold paths ~init:TagsMap.empty
              ~f:(fun ~key:path ~data:prob acc ->
                  match path with
                  | [] -> assert false
                  | _::path' ->
                    let tags = List.filter ~f:(fun x -> x <> 99)
                        (List.map path' ~f:(fun edge ->
                             match Hashtbl.find tag_hash edge with
                             | None ->
                               Printf.printf "Couldn't find %s\n" (dump_edges topo [edge]);
                               99
                             | Some t -> t)) in
                    TagsMap.set acc ~key:tags ~data:prob) in
          SrcDstMap.set acc ~key:(src,dst) ~data:tags)

let path_routing_configuration_of_scheme (topo:topology) (scm:scheme)
    (path_tag_map: int PathMap.t) : configuration =
  SrcDstMap.fold scm
    ~init:SrcDstMap.empty
    ~f:(fun ~key:(src,dst) ~data:path_prob_map acc ->
        if src = dst then acc
        else
          let tag_prob_map = PathMap.fold path_prob_map ~init:TagsMap.empty
              ~f:(fun ~key:path ~data:prob acc ->
                  let tag = PathMap.find_exn path_tag_map path in
                  TagsMap.set acc ~key:([tag]) ~data:prob) in
          SrcDstMap.set acc ~key:(src,dst) ~data:tag_prob_map)


let bprint_tags (buf:Buffer.t) (tag_dist:probability TagsMap.t) : unit =
  TagsMap.iteri
    tag_dist
    ~f:(fun ~key:tags ~data:prob ->
        Printf.bprintf buf "%d " (Float.to_int (1000.0 *. prob));
        Printf.bprintf buf "%d " (List.length tags);
        List.iter tags (Printf.bprintf buf "%d "))

let bprint_configuration (topo:topology) (bufs:(Topology.vertex,Buffer.t) Hashtbl.t)
    (conf:configuration) : unit =
  let dstCount =
    SrcDstMap.fold
      conf
      ~init:VertexMap.empty
      ~f:(fun ~key:(src, dst) ~data:tag_dist acc ->
          let count =
            match VertexMap.find acc src with
            | None -> 0
            | Some x -> x
          in
          VertexMap.set acc ~key:src ~data:(count+1);
        ) in
  SrcDstMap.iteri
    conf
    ~f:(fun ~key:(src,dst) ~data:tag_dist ->
        let buf =
          match Hashtbl.find bufs src with
          | Some buf -> buf
          | None ->
            let buf = Buffer.create 101 in
            Hashtbl.add_exn bufs src buf;
            let count =
              match VertexMap.find dstCount src with
              | None -> 0
              | Some x -> x
            in
            Printf.bprintf buf "%d " count;
            buf in
        Printf.bprintf buf "%lu " (Node.ip (Topology.vertex_to_label topo dst));
        Printf.bprintf buf "%d " (TagsMap.length tag_dist);
        bprint_tags buf tag_dist)

let print_configuration (topo:topology) (conf:configuration) (time:int) : unit =
  let bufs = Hashtbl.Poly.create () in
  bprint_configuration topo bufs conf;
  let routes_dir = "data/routes" in
  let _ = match (Sys.file_exists routes_dir) with | `No -> Unix.mkdir routes_dir | _ -> () in
  Hashtbl.Poly.iteri
    bufs
    ~f:(fun ~key:src ~data:buf ->
        let route_filename = Printf.sprintf "%s/%s_%d"
            routes_dir
            (Frenetic_kernel.Packet.string_of_ip
               (Node.ip (Topology.vertex_to_label topo src))) time in
      let route_file = Out_channel.create route_filename in
      Out_channel.output_string route_file (Buffer.contents buf);
      Out_channel.close route_file;
    )
