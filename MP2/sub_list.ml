(* CS421 - Fall 2017
 * sub_list
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)


(*Problem*)
let rec sub_list l1 l2 = match l2 with [] -> true | (a::b) -> (match l1 with [] -> false | (x::y) -> if a = x then sub_list(y)(b) else sub_list(y)(l2));;
