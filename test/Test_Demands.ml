open Simulate_Traffic

let test_demand =
  let _ = get_demands () in false
    
TEST "test_demand" = test_demand = true
