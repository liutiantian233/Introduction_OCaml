(* CS421 sift
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
let rec sift p l = match l with [] -> ([], []) | x::y ->
                   match sift p y with a, b -> if p x then x::a, b else a, x::b;;
