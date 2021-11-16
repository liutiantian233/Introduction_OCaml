(* CS421 - Fall 2017
 * var
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common

let rec import_list lst = match lst with [] -> ConstExp NilConst | (m, n)::rest -> BinOpAppExp(ConsOp, BinOpAppExp(CommaOp, ConstExp (IntConst m), ConstExp (IntConst n)), import_list rest);;
