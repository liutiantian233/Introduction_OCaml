(* CS421 - Fall 2016 concat
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem.  You will want to change how a number of these start.
 *)

(*************************
* Patterns of Recursion *
*************************)

(*********************
 * Tail Recursion *
 *********************)

(* Problems *)
let rec concat s l =
  let rec concat_tail l m =
    match l with [] -> m | x::y -> concat_tail y (m ^ s ^ x) in
    match l with [] -> "" | a::b -> a ^ concat_tail b "";;
