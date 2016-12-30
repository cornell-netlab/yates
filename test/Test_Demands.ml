open Simulate_Traffic

let test_demand =
  let _ = get_demands () in false

let%test "test_demand" = test_demand = true
