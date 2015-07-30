module type OrderedType = Set.OrderedType

module Mapplus =
struct
  module type S = sig
    include Map.S
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
  end

  module type MAKE = functor (O:OrderedType) -> (S with type key = O.t)

  module Make (Ord:OrderedType) =
  struct
    include Map.Make(Ord)
    let keys m = let ks,_ = List.split (bindings m) in ks
    let values m = let _,vs = List.split (bindings m) in vs
  end
end

module Setplus =
struct
  module type S = sig
    include Set.S
    val map : (elt -> elt) -> t -> t
    val intercalate : (elt -> string) -> string -> t -> string
    val of_list : elt list -> t
  end

  module type MAKE = functor (O:OrderedType) -> (S with type elt = O.t)

  module Make (Ord:OrderedType) =
  struct
    include Set.Make(Ord)
    let map f s = fold (fun v acc -> add (f v) acc) s empty
    let intercalate f sep s =
      fold
        (fun si acc -> Printf.sprintf "%s%s%s" acc (if acc = "" then "" else sep) (f si))
        s ""

    let of_list ls =
      List.fold_left (fun acc e -> add e acc) empty ls
  end
end
