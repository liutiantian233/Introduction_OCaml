(* CS421
 * ackermann
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)


(*Problem*)
let rec ackermann m n = if m = 0 then n + 1 else if n = 0 && m > 0 then ackermann(m - 1)(1) else ackermann(m - 1)(ackermann(m)(n - 1))
