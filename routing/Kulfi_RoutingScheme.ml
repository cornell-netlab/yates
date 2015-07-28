
type edge = unit 

type path = edge list

(* A Routing Scheme is an object that describes a prob distribution over paths. 
   It supports an interface to lets one draw a random sample, and a way to compare
   to other routing schemes, for example, if we want to minimize differences  *)

let draw_random () : path = []
