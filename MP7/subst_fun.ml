open Common

let rec subst_fun sigma ty = match sigma with [] -> TyVar ty | (x, y)::z -> if x = ty then y else subst_fun z ty;;
