open Core
(**************************************************************************)
(* Global variables to store configuration *)
(**************************************************************************)

(* Network *)
let budget = ref Int.max_value
let deloop = ref false
let er_mode = ref false
let nbins : int option ref = ref None

(* Simulator tweaks *)
let tm_sim_iters = ref 1000
let rand_seed : int option ref = ref None
let gurobi_method = ref Int.minus_one

(* Failures and recovery *)
let failure_time = ref Int.max_value
let local_recovery_delay  = ref Int.max_value
let global_recovery_delay = ref Int.max_value
let flash_recover = ref false

(* Routing algorithm specific *)
let ffc_max_link_failures = ref 1
