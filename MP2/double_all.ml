(* CS421
 * double_all
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)


(*Problem*)
let rec double_all l = match l with [] -> [] | (a::b) -> (2. *. a)::double_all(b);;
