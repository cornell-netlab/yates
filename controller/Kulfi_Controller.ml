open Core.Std
open Kulfi_Routing
open Kulfi_Types
open Frenetic_NetKAT
open Frenetic_NetKAT_Optimize

module Make(SOLVER:Kulfi_Routing.Algorithm) = struct
    (* GenSym for tags *)
    let tag_cell = ref 0 
    let fresh_tag () = 
      incr tag_cell;
      !tag_cell
    let reset () = 
      tag_cell := 0

    let netkat_of_path (path:path) : policy * tag = 
      let tag = fresh_tag () in 
      (* NB: Path H1 - S1 - S2 - H2 is represented as [(H1,S1), (S1,S2), (S2,H2)] *)
      let pol,_ = 
	List.fold_left
	  path
	  ~init:(drop,true)
	  ~f:(fun (acc,first) e -> 
	      if first then 
		(acc,false)
	      else
		let _,out_port = Frenetic_Network.Net.Topology.edge_src e in 
		let acc' = 
		  mk_seq (mk_filter(Test(Vlan(tag))))
			 (Mod(Location(Physical(out_port)))) in 
		(acc',false)) in
      (* TODO: pop Vlan at last hop *)
      (pol, tag)

  let netkat_of_scheme (scheme:scheme) : policy * configuration =
    SrcDstMap.fold 
      scheme
      ~init:(drop, SrcDstMap.empty)
      ~f:(fun ~key:(src,dst) ~data:path_dist (pol, config) ->
          let pol', tags = 
	    PathMap.fold
	      path_dist
	      ~init:(pol, TagMap.empty)
	      ~f:(fun ~key:path ~data:prob (pol, tags) -> 
		  let path_pol,tag = netkat_of_path path in 		
		  let pol' = mk_union path_pol pol in
		  let tags' = TagMap.add tags tag prob in
		  (pol',tags')) in 
	  let config' = SrcDstMap.add config (src,dst) tags in
	  (pol',config'))

  let start () = ()
  
end

module Controller = Make(Kulfi_Routing.Mcf)

let () =
  print_endline "Kulfi Controller";
  Controller.start()
