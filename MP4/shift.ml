open Common

(* CS421 - Fall 2016 shift
 * 
 * Please keep in mind that there may be more than one way to solve a
 * problem. You will want to change how a number of these start.
 *)

(*****************************
* Continuation Passing Style *
*****************************)

(* Problems *)
let shiftk (s, q) k = float_addk(q, 1.57) (fun r1 -> float_mulk(r1, r1) (fun m -> truncatek m (fun i -> string_of_intk i (fun r4 -> concatk(s, r4) (fun r5 -> concatk(r5, s) k)))));;
