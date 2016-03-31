open Core.Std

let deloop = ref false
let budget = ref Int.max_value
let failure_time = ref Int.max_value 
let local_recovery_delay = ref Int.max_value 
let global_recovery_delay = ref Int.max_value 
let flash_recover = ref false
let gurobi_method = ref Int.minus_one
