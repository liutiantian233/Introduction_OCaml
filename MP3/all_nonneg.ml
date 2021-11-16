(* CS421 - Fall 2016 all_nonneg
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem.  You will want to change how a number of these start.
 *)

(*************************
* Patterns of Recursion *
*************************)

(*********************
 * Forward Recursion *
 *********************)

(* Problems *)
let rec all_nonneg l =
  let rec all_nonneg_tail l m =
    match l with [] -> m | x::y -> all_nonneg_tail y (m && x > 0) in all_nonneg_tail l true;;
let all_nonneg_start = true;;
let all_nonneg_step r x = r && x >0;;
