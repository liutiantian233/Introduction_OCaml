open Common
open Plsolution
(* Leave these lines here! *)
(* Put any helper functions you want to write and use in this space. *)


let rec first p l = match l with [] -> None | (x::xs) -> if p x then Some x else first p xs


let rec canon_type subst m ty = match ty with TyVar n ->
(match first (fun p -> fst p = n) subst
with Some (j, k) -> (subst, m, TyVar k) | None -> ((n, m)::subst), m + 1, TyVar m) | TyConst (c, tys) ->
(match List.fold_left (fun (subst, n, tyl) -> fun ty ->
(match canon_type subst n ty with (subst', n', ty') -> (subst', n', ty'::tyl))) (subst, m, []) tys
with (new_subst, new_m, new_tys) -> (new_subst, new_m, TyConst(c, List.rev new_tys)))


let canonicalize ty = let (_, _, c_ty) = canon_type [] 0 ty in c_ty


let rec equiv_types ty1 ty2 =
  let new_ty1 = canonicalize ty1 in
  let new_ty2 = canonicalize ty2 in new_ty1 = new_ty2;;
