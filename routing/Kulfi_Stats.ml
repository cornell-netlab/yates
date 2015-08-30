
open Kulfi_Types

type stats = {
  congestions : congestion list;
  num_paths : int list;
  churn : int list
}
               
let make_stats congs paths churn = {
  congestions = congs;
  num_paths = paths;
  churn = churn;
}
