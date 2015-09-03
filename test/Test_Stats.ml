open RunningStat

let test_mean1 =
  let stat = make_running_stat () in
  let stat' = push stat 1.0 in
  let stat'' = push stat' 1.0 in
  let stat''' = push stat'' 1.0 in
  (get_mean stat''' = 1.0)

let test_mean2 =
  let stat = make_running_stat () in
  let stat' = push stat 1.0 in
  let stat'' = push stat' 2.0 in
  let stat''' = push stat'' 3.0 in
  (get_mean stat''' = 2.0)

let test_dev1 =
  let stat = make_running_stat () in
  let stat' = push stat 1.0 in
  let stat'' = push stat' 1.0 in
  let stat''' = push stat'' 1.0 in
  (get_standard_deviation stat''' = 0.0)

let test_dev2 =
  let stat = make_running_stat () in
  let stat' = push stat 1.0 in
  let stat'' = push stat' 2.0 in
  let stat''' = push stat'' 3.0 in
  (get_standard_deviation stat''' = 1.0)

    
TEST "running_mean1" = test_mean1 = true
TEST "running_mean2" = test_mean2 = true
TEST "running_dev1" = test_dev1 = true
TEST "running_dev2" = test_dev2 = true
