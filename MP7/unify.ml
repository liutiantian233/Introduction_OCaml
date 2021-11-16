(* File: unify.ml *)

open Common
open Plsolution

(* PUT ANY HELPER FUNCTIONS YOU WANT UP HERE! *)

let rec unify constraints =
  let rec new_constraints a b c =
    match a, b with [], [] -> Some c | w::x, y::z -> new_constraints x z ((w, y)::c) | _ -> None in
    match constraints with [] -> Some([]) | (v, w)::d -> if v = w then unify d else
    (match (v, w) with (TyConst (e, x), TyConst (f, z)) -> if e = f then
    (match (new_constraints x z d) with None -> None | Some g -> unify g) else None
    | (TyConst (e, x), TyVar (h)) -> unify ((TyVar (h), TyConst (e, x))::d)
    | (TyVar (h), w) -> if (occurs h w) then None else let i = List.map (fun (m, n) ->
    (monoTy_lift_subst [(h, w)] m, monoTy_lift_subst [(h, w)] n)) d in (match unify i with None -> None
    | Some (j) -> Some ((h, monoTy_lift_subst j w)::j)));;

    