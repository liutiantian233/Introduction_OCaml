open Common

let rec occurs v ty = match ty with TyVar x -> x = v | TyConst (y, z) -> List.exists (occurs v) z;;
