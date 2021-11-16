(* CS421 - Fall 2017
 * closer_to_origin
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)


(*Problem*)
let square (x, y) = sqrt(x *. x +. y *. y);;
let closer_to_origin p1 p2 = let dis_p1 = square(p1) and dis_p2 = square(p2) in compare(dis_p1)(dis_p2);;
