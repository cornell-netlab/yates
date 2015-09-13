open Core.Std

external kulfi_gettime : unit -> Int64.t = "kulfi_gettime"

let time () = kulfi_gettime ()

let from (start:Int64.t) =
  Int64.(time () - start)

let to_secs nsecs = Int64.(nsecs / 1000000000L)

let to_fsecs nsecs = (Int64.to_float nsecs) /. (Int64.to_float 1000000000L)
