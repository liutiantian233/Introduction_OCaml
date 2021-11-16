(* CS421 - Fall 2017
 * rectangle_area.ml
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common

(*Problem*)
let rectangle_area l w = if l >= 0.0 && w >= 0.0 then l *. w else -1.0;;
