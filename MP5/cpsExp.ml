(* CS421
 * cpsExp
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You may want to change how this starts.
 *)

open Common
open Plsolution  (* Gives compiled solutions to earlier problems *)
(* Leave this line here! *)

let rec cps_exp e k = match e with VarExp x -> VarCPS (k, x) | ConstExp n -> ConstCPS (k, n) | MonOpAppExp (a, e) ->
let x1 = freshFor (freeVarsInContCPS k) in cps_exp e (FnContCPS (x1, MonOpAppCPS (k, a, x1))) | BinOpAppExp (b, c, d) ->
let x2 = freshFor (freeVarsInContCPS k @ freeVarsInExp c) in
let x3 = freshFor (x2::(freeVarsInContCPS k)) in
let x4 = cps_exp c (FnContCPS (x3, BinOpAppCPS (k, b, x3, x2))) in cps_exp d (FnContCPS (x2, x4)) | IfExp (c, d, e) ->
let x1 = freshFor (freeVarsInContCPS k @ freeVarsInExp d @ freeVarsInExp e) in
let x4 = cps_exp d k in
let x5 = cps_exp e k in cps_exp c (FnContCPS (x1, IfCPS (x1, x4, x5))) | AppExp (c, d) ->
let x2 = freshFor (freeVarsInContCPS k @ freeVarsInExp c) in
let x3 = freshFor (x2::freeVarsInContCPS k) in
let x6 = cps_exp c (FnContCPS (x3, AppCPS (k, x3, x2))) in cps_exp d (FnContCPS (x2, x6)) | FunExp (x, e) ->
let x7 = cps_exp e (ContVarCPS Kvar) in FunCPS (k, x, Kvar, x7) | LetInExp (x, c, d) ->
let x4 = cps_exp d k in
let x8 = FnContCPS (x, x4) in cps_exp c x8 | LetRecInExp(f, x, c, d) ->
let x6 = cps_exp c (ContVarCPS Kvar) in
let x4 = cps_exp d k in FixCPS (FnContCPS (f, x4), f, x, Kvar, x6);;
