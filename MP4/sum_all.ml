open Common

(* CS421 - Fall 2017 sum_all
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let rec sum_all (p,l) = match l with [] -> 0.0 | (x::y) -> let m = sum_all(p, y) in if p x then x +. m else m;;

let rec sum_allk (p,l) k = match l with [] -> k 0.0 | (x::y) -> sum_allk(p, y) (fun m -> p x (fun n -> if n then float_addk(x, m) k else k m));;
