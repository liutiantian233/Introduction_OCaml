open Common

(* CS421 - Fall 2016 even_count
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let rec even_count  l   = match l with [] -> 0 | (x::y) -> let m =  even_count y in (if x mod 2 = 0 then 1 else 0) + m;;
let rec even_countk l k = match l with [] -> k 0 | (x::y) -> even_countk y (fun m -> modk(x, 2) (fun n -> eqk(n, 0) (fun o -> if o then addk(1, m) k else addk(0, m) k)));;
