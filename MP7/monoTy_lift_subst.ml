open Common
open Plsolution

let rec monoTy_lift_subst sigma ty = match ty with TyVar x -> subst_fun sigma x | TyConst (y, z) -> TyConst (y, List.map (monoTy_lift_subst sigma) z);;
