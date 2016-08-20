open Core.Std

let budget          = ref Int.max_value
let deloop          = ref false
let failure_time    = ref Int.max_value
let flash_recover   = ref false
let gurobi_method   = ref Int.minus_one
let tm_sim_iters    = ref 1000
let rand_seed : int option ref = ref None
let local_recovery_delay  = ref Int.max_value
let global_recovery_delay = ref Int.max_value
