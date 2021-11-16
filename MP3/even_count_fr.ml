(* CS421 - Fall 2016 even_count_fr
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
let rec even_count_fr l = match l with [] -> 0 | x::y ->
let ln = even_count_fr y in ln + if (x mod 2) = 0 then 1 else 0;;

let even_count_fr_base = 0;; (* You may need to change this *)
let even_count_fr_rec r x = x + if (r mod 2) = 0 then 1 else 0;;
