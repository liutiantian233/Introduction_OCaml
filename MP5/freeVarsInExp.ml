(* CS421
 * freeVarsInExp
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common

(* Problem 4 *)
let rec freeVarsInExp exp =
  match exp with VarExp x -> [x] | ConstExp y -> []
  | MonOpAppExp (a, b) -> freeVarsInExp b
  | BinOpAppExp (c, d, e) -> freeVarsInExp d @ freeVarsInExp e
  | IfExp (d, e, f) -> freeVarsInExp d @ (freeVarsInExp e @ freeVarsInExp f)
  | AppExp (d, e) -> freeVarsInExp d @ freeVarsInExp e
  | FunExp (g, b) -> List.filter (fun z -> not(z = g)) (freeVarsInExp b)
  | LetInExp (x, d, e) -> freeVarsInExp d @ (List.filter (fun z -> not(z = x)) (freeVarsInExp e))
  | LetRecInExp (g, x, d, e) -> (List.filter (fun z -> not((z = g) || (z = x))) (freeVarsInExp d)) @ (List.filter (fun z -> not(z = g)) (freeVarsInExp e));;
