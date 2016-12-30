open RunningStat

let test_mean1 =
  let stat = make_running_stat () in
  push stat 1.0;
  push stat 1.0;
  push stat 1.0;
  (get_mean stat = 1.0)

let test_mean2 =
  let stat = make_running_stat () in
  push stat 1.0;
  push stat 2.0;
  push stat 3.0;
  (get_mean stat = 2.0)

let test_dev1 =
  let stat = make_running_stat () in
  push stat 1.0;
  push stat 1.0;
  push stat 1.0;
  (get_standard_deviation stat = 0.0)

let test_dev2 =
  let stat = make_running_stat () in
  push stat 1.0;
  push stat 2.0;
  push stat 3.0;
  (get_standard_deviation stat = 1.0)


let%test "running_mean1" = test_mean1 = true
let%test "running_mean2" = test_mean2 = true
let%test "running_dev1" = test_dev1 = true
let%test "running_dev2" = test_dev2 = true
