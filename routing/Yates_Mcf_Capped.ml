open Core

open Yates_LP_Lang
open Yates_Types

(*
LP that uses polynomially many binary-valued variables, which solves MCF with a capped number of paths.
 It uses variables x_{e,i,j} where
  * e ranges over edges
  * i ranges from 1 to k (total number of paths allowed in the MCF solution)
  * j ranges from 1 to L (maximum allowable path length)
An upper bound on the maximum allowable path length is n-1 (if a path has no repeated vertices,
it cannot have more than n-1 edges)

The interpretation of x_{e,i,j}=1 is that edge e belongs to path i, and it constitutes the j-th hop of that path.
There are some constraints that ensure that the MIP solution adheres to this interpretation.

(c1) for all i,j:
  the sum of x_{e,i,j} over all edges e is at most 1.
  (A path can have at most one j-th hop.)
(c2) for all edges e, all i, and all j>1:
  x_{e,i,j} is less or equal to the sum of x_{e',i,j-1} over all edges e' coming into v.
  (If a path contains an edge leaving v, and it is not the first edge on the path, then the path must contain an edge coming into v.)
(c3) for all vertices v, and all i:
  the sum of x_{e,i,j} over all j and all edges e leaving v is at most 1.
  Similarly, the sum of x_{e,i,j} over all j and all edges e coming into v is at most 1.
  (A path cannot enter or leave a vertex twice.)

Now we'll have binary-valued variables y_{i,s,t} where the interpretation of y_{i,s,t}=1 is that path i has source s and destination t. There are some constraints that ensure that the MIP solution adheres to this interpretation.

(c4) for all i,s,t:
  y_{i,s,t} is less than or equal to the sum of x_{e,i,1} over all edges e leaving s.
  (The first hop of a path from s to t must be one of the edges leaving s.)
(c5) for all i,s,t:
  y_{i,s,t} is less than or equal to the sum of x_{e,i,j} over all j and all edges e coming into t.
  (A path from s to t must contain an edge entering t.)
(c6) for all i,s,t:
  1-y_{i,s,t} is greater than or equal to the sum of x_{e,i,j} over all j and all edges e leaving t.
  (If a path ends at t, then it cannot contain an edge leaving t.)

Finally, we have non-negative real-valued variables a_{e,i} and b_{i,s,t} denoting the amount of flow that traverses edge e as part of path i, and the amount of flow that path i sends from s to t. These must satisfy.

(c7) for all e,i:
  a_{e,i} <= K*(sum of x_{e,i,j} over all j).
  (If path i sends a positive amount of flow on e, then e must be the j-th hop of the path for some value of j.)
(c8) for all i,s,t:
  b_{i,s,t} <= K*y_{i,s,t}
  (If path i sends a positive amount of flow from s to t, then s and t must be the source and destination of the path.)
(c9) for all vertices v, and all i:
  (sum of a_{e,i} over all edges leaving v) - (sum of a_{e,i} over all edges entering v) <= K*(sum of y_{i,v,t} over all t)
  (If the amount of flow leaving v on path i is greater than the amount of flow coming into v on that path, then v must be the source of the path.)
(c10) for all vertices v, and all i:
  (sum of a_{e,i} over all edges entering v) - (sum of a_{e,i} over all edges leaving v) <= K*(sum of y_{i,s,v} over all s)
  (If the amount of flow entering v on path i is greater than the amount of flow leaving v on that path, then v must be the sink of the path.)
(c11) for all edges e:
  (sum of a_{e,i} over all i) <= capacity(e)
  (The total amount of flow on e doesn't violate its capacity.)

 *)

let mk_objective () : arith_exp =
  Var "Z"


let range (i:int) (j:int) : (int list) =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []


(*
 (c1) for all i,j:
  the sum of x_{e,i,j} over all edges e is at most 1.
  (A path can have at most one j-th hop.)
(c2) for all edges e, all i, and all j>1:
  x_{e,i,j} is less or equal to the sum of x_{e',i,j-1} over all edges e' coming into v.
  (If a path contains an edge leaving v, and it is not the first edge on the path, then the path must contain an edge coming into v.)
(c3) for all vertices v, and all i:
  the sum of x_{e,i,j} over all j and all edges e leaving v is at most 1.
  Similarly, the sum of x_{e,i,j} over all j and all edges e coming into v is at most 1.
  (A path cannot enter or leave a vertex twice.)
 *)


let x_var topo e i j =
  let src,_ = Topology.edge_src e in
  let dst,_ = Topology.edge_dst e in
  Printf.sprintf "x_%s--%s_%d_%d"
		 (name_of_vertex topo dst) (name_of_vertex topo src) i j

let mk_constraints (topo : Topology.t) (demand_pairs : demands) (k:int) (l:int) : (constrain list) =

  let is = (range 1 k) in (* i from 1 to k *)
  let js = (range 1 l) in (* j from 1 to l *)
  let cs1 =
       List.fold_left
	 is
	 ~init:[]
	 ~f:(fun acc i ->
	     List.fold_left
	       js
	       ~init:acc
	       ~f:(fun acc j ->
		   let xs =
		     Topology.fold_edges
		       (fun e acc2 -> let x = x_var topo e i j in Var(x)::acc2) topo [] in
		   let sum = Sum(xs) in
		   let (c:constrain) = Leq("c1",sum,1.0) in
		   c::acc )) in

  cs1


let mk_lp (topo : Topology.t) (demand_pairs : demands) (k:int) (l:int) : lp =
  let o = mk_objective () in
  let c = mk_constraints topo demand_pairs k l in
  (o, c)

let solve (topo:topology) (pairs:demands) : scheme =
  let k = 10 in (* total number of paths allowed in the MCF solution *)
  let l = 16 in (* maximum allowable path length *)
  let lp = mk_lp topo pairs k l in
  Printf.printf "%s" (string_of_lp lp);
  assert false





