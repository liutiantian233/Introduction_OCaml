open Common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None -> None
             | Some sigma -> Some(Proof([], judgment), sigma))
    | VarExp x ->
        (match lookup_env gamma x with None -> None | Some gamma_x ->
            (match unify [(tau, freshInstance gamma_x)]
            with None -> None | Some sigma -> Some(Proof([], judgment), sigma)))
    | BinOpAppExp (x, a, b) ->
        let tau' = binop_signature x in
        let y = fresh() in
        let z = fresh() in
        (match gather_exp_ty_substitution gamma a y
        with None -> None
            | Some(sm1, sigma1) -> (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) b z
            with None -> None
                | Some(sm2, sigma2) ->
                    let sigma = subst_compose sigma2 sigma1 in
                    (match unify [(monoTy_lift_subst sigma (mk_fun_ty y (mk_fun_ty z tau)), freshInstance tau')]
            with None -> None
                | Some sigma3 -> Some(Proof([sm1;sm2], judgment),subst_compose sigma3 sigma))))
    | MonOpAppExp (x, a) ->
        let tau' = monop_signature x in
        let y = fresh() in
        (match gather_exp_ty_substitution gamma a y
        with None -> None
            | Some(sm, sigma) -> (match unify [(monoTy_lift_subst sigma (mk_fun_ty y tau), freshInstance tau')]
            with None -> None
                | Some sigma1 -> Some(Proof([sm], judgment), subst_compose sigma1 sigma)))
    | IfExp (a, b, c) ->
        (match gather_exp_ty_substitution gamma a bool_ty with None -> None
            | Some(sm1, sigma1) ->
                (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) b (monoTy_lift_subst sigma1 tau)
                with None -> None
                    | Some(sm2, sigma2) ->
                        let sigma = subst_compose sigma2 sigma1 in
                        (match gather_exp_ty_substitution (env_lift_subst sigma gamma) c (monoTy_lift_subst sigma tau)
                        with None -> None
                            | Some(sm3, sigma3) -> Some(Proof([sm1;sm2;sm3], judgment), subst_compose sigma3 sigma))))
    | FunExp (x, y) ->
        let a = fresh() in
        let b = fresh() in
        (match gather_exp_ty_substitution (ins_env gamma x (polyTy_of_monoTy a)) y b with None -> None
            | Some(sm, sigma1) ->
                (match unify [(monoTy_lift_subst sigma1 tau, monoTy_lift_subst sigma1 (mk_fun_ty a b))]
                with None -> None
                    | Some sigma -> Some(Proof([sm], judgment), subst_compose sigma sigma1)))
    | AppExp (x, y) ->
        let a = fresh() in
        (match gather_exp_ty_substitution gamma x (mk_fun_ty a tau) with None -> None
            | Some(sm1, sigma1) ->
                (match gather_exp_ty_substitution (env_lift_subst sigma1 gamma) y (monoTy_lift_subst sigma1 a)
                with None -> None
                    | Some(sm2, sigma2) -> Some(Proof([sm1;sm2], judgment), subst_compose sigma2 sigma1)))
    | RaiseExp x ->
        (match gather_exp_ty_substitution gamma x int_ty with None -> None
            | Some(sm, sigma) -> Some(Proof([sm], judgment), sigma))
    | LetInExp (a, b, c) ->
        let x = fresh() in
        (match gather_exp_ty_substitution gamma b x with None -> None
            | Some(sm1, sigma1) ->
                let y = make_env a (gen (env_lift_subst sigma1 gamma) (monoTy_lift_subst sigma1 x)) in
                (match gather_exp_ty_substitution (sum_env y (env_lift_subst sigma1 gamma)) c (monoTy_lift_subst sigma1 tau)
                with None -> None
                    | Some(sm2, sigma2) ->
                        let sigma = subst_compose sigma2 sigma1 in Some(Proof([sm1;sm2], judgment), sigma)))
    | LetRecInExp (a, b, c, d) ->
        let x = fresh() in
        let y = fresh() in
        let z = mk_fun_ty x y in
        (match gather_exp_ty_substitution (ins_env (ins_env gamma a (polyTy_of_monoTy z)) b (polyTy_of_monoTy x)) c y
        with None -> None
            | Some(sm1, sigma1) ->
                let e = env_lift_subst sigma1 gamma in
                let f = monoTy_lift_subst sigma1 z in
                (match gather_exp_ty_substitution (ins_env e a (gen e f)) d (monoTy_lift_subst sigma1 tau)
                with None -> None
                    | Some(sm2, sigma2) ->
                        let sigma = subst_compose sigma2 sigma1 in Some(Proof([sm1;sm2], judgment), sigma)))
    | TryWithExp (a, b, c, d) ->
        (match (gather_exp_ty_substitution gamma a tau) with None -> None
            | Some(sm, sigma) ->
                (match List.fold_left (fun result -> fun (out, value) ->
                (match result with None -> None
                    | Some(rev_list, sigmas) ->
                        (match gather_exp_ty_substitution (env_lift_subst sigmas gamma) value (monoTy_lift_subst sigmas tau)
                        with None -> None
                            | Some(sm_n, sigma_n) -> Some(sm_n::rev_list, subst_compose sigma_n sigmas))))
                (Some([sm], sigma)) ((b, c)::d) with None -> None
                    | Some(rev_list, sub_sigmas) -> Some(Proof(List.rev rev_list, judgment), sub_sigmas)));;