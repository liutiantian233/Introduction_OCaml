(* CS421 even_count_tr
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
let rec even_count_tr l = 
  let rec even_count_tr_tail l m = 
    match l with [] -> m | x::y -> even_count_tr_tail y (if x mod 2 = 0 then m + 1 else m) in even_count_tr_tail l 0;;
let even_count_tr_start = 0;;
let even_count_tr_step x rec_val = if rec_val mod 2 = 0 then x + 1 else x;;
