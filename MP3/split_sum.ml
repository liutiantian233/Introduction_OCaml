(* CS421 - Fall 2017
 * split_sum
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

(*************************
 * Patterns of Recursion *
 *************************)

(******************
 * Tail Recursion *
 ******************)


(* Problem 10 *)
let split_sum l f =
  let rec split_sum_tail (index_a, index_b) x =
    match x with [] -> index_a, index_b | x::y ->
    split_sum_tail (if f x then (index_a + x, index_b) else (index_a, index_b + x)) y in split_sum_tail (0, 0) l;;

(* Problem 20 *)
let split_sum_start = (0, 0);;
let split_sum_step f (index_a, index_b) x = if f x then (index_a + x, index_b) else (index_a, index_b + x);;
