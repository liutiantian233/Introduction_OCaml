open Common

(* CS421 - Fall 2017 list_prod
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(***** Problem 6a: Recursion & CPS ******)
let rec list_prod l = match l with [] -> 1 | (x::y) -> let m = list_prod y in x * m;;

(***** Problem 6b: Recursion & CPS ******)
let rec list_prodk l k = match l with [] -> k 1 | (x::y) -> list_prodk y (fun m -> mulk(x, m) k);;
