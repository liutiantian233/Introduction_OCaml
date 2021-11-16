(* CS421 - Fall 2017
 * var
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common

let rec count_const_in_exp exp =
  match exp with VarExp x -> 0 | ConstExp y -> 1 | MonOpAppExp (a, b) -> count_const_in_exp b
  | BinOpAppExp (c, d, e) -> (count_const_in_exp d) + (count_const_in_exp e)
  | IfExp (d, e, f) -> (count_const_in_exp d) + (count_const_in_exp e) + (count_const_in_exp f)
  | AppExp (d, e) -> (count_const_in_exp d) + (count_const_in_exp e)
  | FunExp (g, b) -> count_const_in_exp b
  | LetInExp (x, d, e) -> (count_const_in_exp d) + (count_const_in_exp e)
  | LetRecInExp (g, x, d, e) -> (count_const_in_exp d) + (count_const_in_exp e);;
