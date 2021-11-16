open Common

(* CS421 - Fall 2018 fold_left
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let rec fold_left (f,e,l) = match l with [] -> e | (x::y) -> fold_left(f, (f (e, x)), y);;

let rec fold_leftk (fk,e,l) k = match l with [] -> k e | (x::y) -> fk(e, x) (fun m -> fold_leftk(fk, m, y) k);;
