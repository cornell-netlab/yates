open Core
open Yates_routing.Util
open Yates_types.Types
open Helper


(* List of supported TE systems *)
type solver_type =
  | Ac | Cspf | Ecmp | Edksp | Ksp | Mcf | MwMcf | Raeke | Spf | Vlb
  | AkMcf | AkRaeke | AkEcmp | AkKsp | AkVlb
  | Ffc | Ffced
  | SemiMcfAc | SemiMcfEcmp | SemiMcfEdksp | SemiMcfKsp | SemiMcfKspFT
  | SemiMcfMcf | SemiMcfMcfEnv | SemiMcfMcfFTEnv | SemiMcfRaeke
  | SemiMcfRaekeFT | SemiMcfVlb
  | OptimalMcf

(* TE system identifier *)
let solver_to_string (s:solver_type) : string =
  match s with
  | Ac -> "ac"
  | AkEcmp -> "akecmp"
  | AkKsp -> "akksp"
  | AkMcf -> "akmcf"
  | AkRaeke -> "akraeke"
  | AkVlb -> "akvlb"
  | Cspf -> "cspf"
  | Ecmp -> "ecmp"
  | Edksp -> "edksp"
  | Ffc -> "ffc"
  | Ffced -> "ffced"
  | Ksp -> "ksp"
  | Mcf -> "mcf"
  | MwMcf -> "mwmcf"
  | Raeke -> "raeke"
  | SemiMcfAc -> "semimcfac"
  | SemiMcfEcmp -> "semimcfecmp"
  | SemiMcfEdksp -> "semimcfedksp"
  | SemiMcfKsp -> "semimcfksp"
  | SemiMcfKspFT -> "semimcfkspft"
  | SemiMcfMcf -> "semimcfmcf"
  | SemiMcfMcfEnv -> "semimcfmcfenv"
  | SemiMcfMcfFTEnv -> "semimcfmcfftenv"
  | SemiMcfRaeke -> "semimcfraeke"
  | SemiMcfRaekeFT -> "semimcfraekeft"
  | SemiMcfVlb -> "semimcfvlb"
  | Spf -> "spf"
  | Vlb -> "vlb"
  | OptimalMcf -> "optimalmcf"

(* TE system identifier reverse map *)
let string_to_solver (s:string) : solver_type =
  match s with
  | "ac" -> Ac
  | "akecmp" -> AkEcmp
  | "akksp" -> AkKsp
  | "akmcf" -> AkMcf
  | "akraeke" -> AkRaeke
  | "akvlb" -> AkVlb
  | "cspf" -> Cspf
  | "ecmp" -> Ecmp
  | "edksp" -> Edksp
  | "ffc" -> Ffc
  | "ffced" -> Ffced
  | "ksp" -> Ksp
  | "mcf" -> Mcf
  | "mwmcf" -> MwMcf
  | "raeke" -> Raeke
  | "semimcfac" -> SemiMcfAc
  | "semimcfecmp" -> SemiMcfEcmp
  | "semimcfedksp" -> SemiMcfEdksp
  | "semimcfksp" -> SemiMcfKsp
  | "semimcfkspft" -> SemiMcfKspFT
  | "semimcfmcf" -> SemiMcfMcf
  | "semimcfmcfenv" -> SemiMcfMcfEnv
  | "semimcfmcfftenv" -> SemiMcfMcfFTEnv
  | "semimcfraeke" -> SemiMcfRaeke
  | "semimcfraekeft" -> SemiMcfRaekeFT
  | "semimcfvlb" -> SemiMcfVlb
  | "spf" -> Spf
  | "vlb" -> Vlb
  | "optimalmcf" -> OptimalMcf
  | _ -> failwith (Printf.sprintf "Invalid solver type specified: %s" s)

(* Return a brief summary of specified TE system *)
let solver_to_description (s:solver_type) : string =
  match s with
  | Ac -> "Applegate-Cohen's optimal oblivious TE"
  | AkEcmp -> "ECMP for path selection + multiplicative weights for rate adaptation"
  | AkKsp -> "k-shortest paths for path selection + multiplicative weights for rate adaptation"
  | AkMcf -> "multi-commodity flow for path selection + multiplicative weights for rate adaptation"
  | AkRaeke -> "Raecke's oblivious routing for path selection + multiplicative weights for rate adaptation"
  | AkVlb -> "Valiant load balancing for path selection + multiplicative weights for rate adaptation"
  | Cspf -> "Constrained Shortest Paths First"
  | Ecmp -> "Equal-Cost Multi Path"
  | Edksp -> "Edge-disjoint k-shortest paths"
  | Ffc -> "Forward Fault Correction (FFC) with k-shortest paths"
  | Ffced -> "Forward Fault Correction (FFC) with edge-disjoing k-shortest paths"
  | Ksp -> "k-shortest paths"
  | Mcf -> "multi-commodity flow that minimizes max. link utilization"
  | MwMcf -> "multi-commodity flow using multiplicative weights"
  | Raeke -> "Raecke's oblivious routing"
  | SemiMcfAc -> "Applegate-Cohen's oblivious routing for path selection + restricted MCF for rate adaptation"
  | SemiMcfEcmp -> "ECMP for path selection + restricted MCF for rate adaptation"
  | SemiMcfEdksp -> "Edge-disjoint k-shortest paths for path selection + restricted MCF for rate adaptation"
  | SemiMcfKsp -> "k-shortest paths for path selection + restricted MCF for rate adaptation"
  | SemiMcfKspFT -> "k-shortest paths with fault tolerance for path selection + restricted MCF for rate adaptation"
  | SemiMcfMcf -> "MCF for path selection + restricted MCF for rate adaptation"
  | SemiMcfMcfEnv -> "MCF with demand envelope for path selection + restricted MCF for rate adaptation"
  | SemiMcfMcfFTEnv -> "MCF with demand envelope and fault tolerance for path selection + restricted MCF for rate adaptation"
  | SemiMcfRaeke -> "Raecke's oblivious routing for path selection + restricted MCF for rate adaptation"
  | SemiMcfRaekeFT -> "Raecke's oblivious routing with fault tolerance for path selection + restricted MCF for rate adaptation"
  | SemiMcfVlb -> "Valiant load balancing for path selection + restricted MCF for rate adaptation"
  | Spf -> "Shortest paths first"
  | Vlb -> "Valiant Load Balancing"
  | OptimalMcf -> "Optimal multi-commodity flow based TE which doesn't have any operational constraints"

(* Select the rate adaptation method for a TE system *)
let select_algorithm solver = match solver with
  | Ac -> Yates_routing.AC.solve
  | AkEcmp
  | AkKsp
  | AkMcf
  | AkRaeke
  | AkVlb -> Yates_routing.Ak.solve
  | Cspf -> Yates_routing.Cspf.solve
  | Ecmp -> Yates_routing.Ecmp.solve
  | Edksp -> Yates_routing.Edksp.solve
  | Ffc
  | Ffced -> Yates_routing.Ffc.solve
  | Ksp -> Yates_routing.Ksp.solve
  | Mcf -> Yates_routing.Mcf.solve
  | MwMcf -> Yates_routing.MwMcf.solve
  | OptimalMcf -> Yates_routing.Mcf.solve
  | Raeke -> Yates_routing.Raeke.solve
  | SemiMcfAc
  | SemiMcfEcmp
  | SemiMcfEdksp
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfVlb -> Yates_routing.SemiMcf.solve
  | Spf -> Yates_routing.Spf.solve
  | Vlb -> Yates_routing.Vlb.solve

(* Select the local recovery method for a TE system *)
let select_local_recovery solver = match solver with
  | Ac -> Yates_routing.AC.local_recovery
  | AkEcmp
  | AkKsp
  | AkMcf
  | AkRaeke
  | AkVlb -> Yates_routing.Ak.local_recovery
  | Cspf -> Yates_routing.Cspf.local_recovery
  | Ecmp -> Yates_routing.Ecmp.local_recovery
  | Edksp -> Yates_routing.Edksp.local_recovery
  | Ffc
  | Ffced -> Yates_routing.Ffc.local_recovery
  | Ksp -> Yates_routing.Ksp.local_recovery
  | Mcf -> Yates_routing.Mcf.local_recovery
  | MwMcf -> Yates_routing.MwMcf.local_recovery
  | OptimalMcf -> failwith "No local recovery for optimal mcf"
  | Raeke -> Yates_routing.Raeke.local_recovery
  | SemiMcfAc
  | SemiMcfEcmp
  | SemiMcfEdksp
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfVlb -> Yates_routing.SemiMcf.local_recovery
  | Spf -> Yates_routing.Spf.local_recovery
  | Vlb -> Yates_routing.Vlb.local_recovery

let demand_envelope = ref SrcDstMap.empty
(* Compute the initial scheme for a TE algorithm *)
let initial_scheme algorithm topo predict : scheme =
  match algorithm with
  | SemiMcfAc ->
    let _ = Yates_routing.AC.initialize SrcDstMap.empty in
    Yates_routing.AC.solve topo SrcDstMap.empty
  | AkEcmp
  | SemiMcfEcmp ->
    let _ = Yates_routing.Ecmp.initialize SrcDstMap.empty in
    Yates_routing.Ecmp.solve topo SrcDstMap.empty
  | Ffced
  | SemiMcfEdksp ->
    let _ = Yates_routing.Edksp.initialize SrcDstMap.empty in
    Yates_routing.Edksp.solve topo SrcDstMap.empty
  | AkKsp
  | Ffc
  | SemiMcfKsp ->
    let _ = Yates_routing.Ksp.initialize SrcDstMap.empty in
    Yates_routing.Ksp.solve topo SrcDstMap.empty
  | SemiMcfKspFT ->
    let _ = Yates_routing.Ksp.initialize SrcDstMap.empty in
    all_failures_envelope Yates_routing.Ksp.solve topo SrcDstMap.empty
  | AkMcf
  | SemiMcfMcf ->
    Yates_routing.Mcf.solve topo predict
  | SemiMcfMcfEnv ->
    Yates_routing.Mcf.solve topo !demand_envelope
  | SemiMcfMcfFTEnv ->
    all_failures_envelope Yates_routing.Mcf.solve topo !demand_envelope
  | AkRaeke
  | SemiMcfRaeke ->
    let _ = Yates_routing.Raeke.initialize SrcDstMap.empty in
    Yates_routing.Raeke.solve topo SrcDstMap.empty
  | SemiMcfRaekeFT ->
    let _ = Yates_routing.Raeke.initialize SrcDstMap.empty in
    all_failures_envelope Yates_routing.Raeke.solve topo SrcDstMap.empty
  | AkVlb
  | SemiMcfVlb ->
    let _ = Yates_routing.Vlb.initialize SrcDstMap.empty in
    Yates_routing.Vlb.solve topo SrcDstMap.empty
  | _ -> SrcDstMap.empty


(* Initialize a TE algorithm *)
let initialize_scheme algorithm topo predict : unit =
  let start_scheme = initial_scheme algorithm topo predict in
  let pruned_scheme =
    if SrcDstMap.is_empty start_scheme then start_scheme
    else prune_scheme topo start_scheme !Yates_routing.Globals.budget in
  match algorithm with
  | Ac -> Yates_routing.AC.initialize SrcDstMap.empty
  | AkEcmp
  | AkKsp
  | AkMcf
  | AkRaeke
  | AkVlb -> Yates_routing.Ak.initialize pruned_scheme
  | Cspf -> Yates_routing.Cspf.initialize SrcDstMap.empty
  | Ecmp -> Yates_routing.Ecmp.initialize SrcDstMap.empty
  | Edksp -> Yates_routing.Edksp.initialize SrcDstMap.empty
  | Ffc
  | Ffced -> Yates_routing.Ffc.initialize pruned_scheme
  | Ksp -> Yates_routing.Ksp.initialize SrcDstMap.empty
  | Raeke -> Yates_routing.Raeke.initialize SrcDstMap.empty
  | SemiMcfAc
  | SemiMcfEcmp
  | SemiMcfEdksp
  | SemiMcfKsp
  | SemiMcfKspFT
  | SemiMcfMcf
  | SemiMcfMcfEnv
  | SemiMcfMcfFTEnv
  | SemiMcfRaeke
  | SemiMcfRaekeFT
  | SemiMcfVlb -> Yates_routing.SemiMcf.initialize pruned_scheme
  | Vlb -> Yates_routing.Vlb.initialize SrcDstMap.empty
  | _ -> ()

(* Name and description of all supported TE systems *)
let all_solver_string_descripton : (string * string) list =
  [ (solver_to_string Ac, solver_to_description Ac) ;
    (solver_to_string AkEcmp, solver_to_description AkEcmp) ;
    (solver_to_string AkKsp, solver_to_description AkKsp) ;
    (solver_to_string AkMcf, solver_to_description AkMcf) ;
    (solver_to_string AkRaeke, solver_to_description AkRaeke) ;
    (solver_to_string AkVlb, solver_to_description AkVlb) ;
    (solver_to_string Cspf, solver_to_description Cspf) ;
    (solver_to_string Ecmp, solver_to_description Ecmp) ;
    (solver_to_string Edksp, solver_to_description Edksp) ;
    (solver_to_string Ffc, solver_to_description Ffc) ;
    (solver_to_string Ffced, solver_to_description Ffced) ;
    (solver_to_string Ksp, solver_to_description Ksp) ;
    (solver_to_string Mcf, solver_to_description Mcf) ;
    (solver_to_string MwMcf, solver_to_description MwMcf) ;
    (solver_to_string Raeke, solver_to_description Raeke) ;
    (solver_to_string SemiMcfAc, solver_to_description SemiMcfAc) ;
    (solver_to_string SemiMcfEcmp, solver_to_description SemiMcfEcmp) ;
    (solver_to_string SemiMcfEdksp, solver_to_description SemiMcfEdksp) ;
    (solver_to_string SemiMcfKsp, solver_to_description SemiMcfKsp) ;
    (solver_to_string SemiMcfKspFT, solver_to_description SemiMcfKspFT) ;
    (solver_to_string SemiMcfMcf, solver_to_description SemiMcfMcf) ;
    (solver_to_string SemiMcfMcfEnv, solver_to_description SemiMcfMcfEnv) ;
    (solver_to_string SemiMcfMcfFTEnv, solver_to_description SemiMcfMcfFTEnv) ;
    (solver_to_string SemiMcfRaeke, solver_to_description SemiMcfRaeke) ;
    (solver_to_string SemiMcfRaekeFT, solver_to_description SemiMcfRaekeFT) ;
    (solver_to_string SemiMcfVlb, solver_to_description SemiMcfVlb) ;
    (solver_to_string Spf, solver_to_description Spf) ;
    (solver_to_string Vlb, solver_to_description Vlb) ;
    (solver_to_string OptimalMcf, solver_to_description OptimalMcf) ]
