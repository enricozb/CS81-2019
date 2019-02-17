open Env
open Syntax
open Type
open Basis
open Error

(* ----------------------------------------------------------------------------
 * Type checking.
 *)

let rec typecheck_expr gamma (l, expr) = match expr with
  | Literal v -> typecheck_literal l gamma v

  | Var v -> (fresh_tyenv (tyenv_find l gamma v), ConTrivial)

  | List exprs -> begin match exprs with
    | [] -> (list_ty (fresh_tyvar ()), ConTrivial)
    | exprs ->
      (* get types and constraints for each element *)
      let (types, constraints) =
        List.split @@
          List.map (typecheck_expr gamma) exprs
      in
      (* equate all of the elements of the list *)
      let con = con_eq_tys types in
      (list_ty (List.nth types 0), con_join (con :: constraints))
    end

  | If (e1, e2, e3) ->
    let (t1, c1) = typecheck_expr gamma e1
    and (t2, c2) = typecheck_expr gamma e2
    and (t3, c3) = typecheck_expr gamma e3
    in
      (t2, con_join [c1; c2; c3; ConEq (t2, t3); ConEq (t1, bool_ty)])

  | Begin [] -> (unit_ty, ConTrivial)
  | Begin exprs ->
    let (ts, c) = typecheck_exprs gamma exprs in
      (List.hd @@ List.rev ts, c)

  | Call (func, args) -> (match (typecheck_exprs gamma (func :: args)) with
    | ([], _) -> Error.syntax_err l "This cannot ever happen!"
    | (funty :: argtys, c) ->
      let retty = fresh_tyvar () in
        (retty, ConAnd (c, ConEq (funty, funtype_of argtys retty))))

  | LetX (LetStar, [], body) -> typecheck_expr gamma body
  | LetX (LetStar, b :: bs, body) ->
    typecheck_expr gamma (l, LetX (Let, [b], (l, LetX (LetStar, bs, body))))

    (* This is almost embarassingly complicated. *)
  | LetX (Let, vars, body) ->
    (* Split let into names and values. *)
    let (xs, bs) = List.split vars in
    (* Typecheck all of the let vars. *)
    let (ts, c) = typecheck_exprs gamma bs in
    (* Solve the constraints for the lets. *)
    let theta = con_solve l c in
    (* Generate new constraints for the let. *)
    let alphas = StringSet.elements @@ StringSet.inter (free_tyvars_tyenv gamma) (subst_dom theta) in
    let c' = con_join (List.map (fun a -> ConEq (TyVar a, subst_var theta a)) alphas) in
    (* Find the free typevars in both gamma and the new constraint. *)
    let tyvars = StringSet.union (free_tyvars_tyenv gamma) (free_tyvars_con c') in
    (* Generalize the types we got from evaluating the let variables wrt the
     * free typevars that we found. *)
    let ss = List.map (fun t -> generalize_ty (subst_ty theta t) tyvars) ts in
    (* Bind each of the new variables into the environment, producing gamma' *)
    let gamma' = List.fold_left2 tyenv_bind gamma xs ss in
    (* Evaluate the body of the let wrt gamma', producing t and c_b. *)
    let (t, cb) = typecheck_expr gamma' body in
    (* We still need to satisfy both c' and c_b, so pass both up. *)
      (t, ConAnd (c', cb))

  | LetX (LetRec, vars, body) ->
    (* Split let into names and values. *)
    let (xs, bs) = List.split vars in
    (* Produce new alphas to "guess" in the letrec, and turn them into typeschemes. *)
    let alphas_fresh = List.map fresh_tyvar xs in
    let forall_alphas_fresh = List.map (fun t -> ForAll ([], t)) alphas_fresh in
    (* Bind these alphas into a gamma that we can evaluate our vars with. *)
    let gamma_fresh = List.fold_left2 tyenv_bind gamma xs forall_alphas_fresh in
    (* Typecheck all of the let vars. *)
    let (ts, c) = typecheck_exprs gamma_fresh bs in
    (* Create constraints so our "guess" alphas_fresh are equal to the real types ts. *)
    let c_fresh = con_join (List.map2 (fun a t -> ConEq (a, t)) alphas_fresh ts) in
    (* Solve the joint constraint between c and c_fresh. *)
    let theta = con_solve l (ConAnd (c, c_fresh)) in
    (* Generate new constraints for the let. *)
    let alphas = StringSet.elements @@ StringSet.inter (free_tyvars_tyenv gamma) (subst_dom theta) in
    let c' = con_join (List.map (fun a -> ConEq (TyVar a, subst_var theta a)) alphas) in
    (* Find the free typevars in both gamma and the new constraint. *)
    let tyvars = StringSet.union (free_tyvars_tyenv gamma) (free_tyvars_con c') in
    (* Generalize the types we got from evaluating the let variables wrt the
     * free typevars that we found. *)
    let ss = List.map (fun t -> generalize_ty (subst_ty theta t) tyvars) ts in
    (* Bind each of the new variables into the environment, producing gamma' *)
    let gamma' = List.fold_left2 tyenv_bind gamma xs ss in
    (* Evaluate the body of the let wrt gamma', producing t and c_b. *)
    let (t, cb) = typecheck_expr gamma' body in
    (* We still need to satisfy both c' and c_b, so pass both up. *)
      (t, ConAnd (c', cb))

  | Lambda (params, body) ->
    (* Produce new alphas to "guess" in the lambda, and turn them into typeschemes. *)
    let alphas_fresh = List.map fresh_tyvar params in
    let forall_alphas_fresh = List.map (fun t -> ForAll ([], t)) alphas_fresh in
    (* Bind these alphas into a new type environment gamma' *)
    let gamma' = List.fold_left2 tyenv_bind gamma params forall_alphas_fresh in
    (* Typecheck the body of the lambda *)
    let (t, c) = typecheck_expr gamma' body in
    (* The type we return is alphas_fresh -> t, and we need to satisfy constraints c still.*)
      (funtype_of alphas_fresh t, c)

and typecheck_exprs gamma exprs = match exprs with
  | [] -> ([], ConTrivial)
  | expr :: exprs ->
    let (ty, c) = typecheck_expr gamma expr
    and (tys, c') = typecheck_exprs gamma exprs
    in
      (ty :: tys, ConAnd (c, c'))

and typecheck_literal l gamma = function
  | Nil -> (list_ty (fresh_tyvar ()), ConTrivial)

  | Bool _ -> (bool_ty, ConTrivial)

  | Num _ -> (num_ty, ConTrivial)

  | Sym _ -> (sym_ty, ConTrivial)

  (* Literal Pair instances are only for list construction, since they only
   * show up as literals in quoted list constructors.
   * Treat p2 and p1 as a list and value, respectively.
   * TODO: Is this okay? Check PL_PBC again. *)
  | Pair (p1, p2) ->
    let (t1, c1) = typecheck_literal l gamma p1
    and (t2, c2) = typecheck_literal l gamma p2
    in
      (t2, con_join [c1; c2; ConEq(list_ty t1, t2)])

  | ListVal vals -> failwith "list literals are only results"

  | Closure ((params, body), _) ->
    let param_tys = List.map fresh_tyvar params
    and (retty, c) = typecheck_expr gamma body in
    let funty = funtype_of param_tys retty
    in
      (funty, c)

  | Primitive _
  | Ref _ -> Error.syntax_err l "Unexpected literal in typechecker" (* TODO: msg *)

and typecheck_def gamma (l, def) = match def with
  | Val (name, expr) ->
    let (t, c) = typecheck_expr gamma expr in
    let theta = con_solve l c in
    let sigma = generalize_ty (subst_ty theta t) (free_tyvars_tyenv gamma)
    in
      (tyenv_bind gamma name sigma, sigma)

  | ValRec (name, expr) ->
    let alpha = fresh_tyvar () in
    let gamma' = tyenv_bind gamma name (ForAll ([], alpha)) in
    let (t, c) = typecheck_expr gamma' expr in
    let theta = con_solve l (ConAnd (c, ConEq (t, alpha))) in
    let sigma = generalize_ty (subst_ty theta alpha) (free_tyvars_tyenv gamma) in
      (tyenv_bind gamma name sigma, sigma)

  | Expr expr -> typecheck_def gamma (l, Val ("_", expr))

  | Define (name, params, body) ->
    typecheck_def gamma (l, ValRec (name, (l, Lambda (params, body))))

  | Use _
  | CheckExpect (_, _)
  | CheckError _
  | CheckType (_, _)
  | CheckPrincipalType (_, _)
  | CheckTypeError _ -> Error.unit_test_err l

(* ----------------------------------------------------------------------------
 * Evaluation.
 *)

let rec eval_expr env (l, expr) = match expr with
  | Literal v -> v

  | Var v -> find l env v

  | List exprs ->
      (* TODO: these should be evaluated in sequence folding `env` with it *)
      ListVal (List.map (eval_expr env) exprs)

  | If (cond, t, f) ->
    if truthy l (eval_expr env cond) then
      eval_expr env t
    else
      eval_expr env f

  | Begin [] -> Nil
  | Begin [x] -> eval_expr env x
  | Begin (x :: xs) ->
    ignore (eval_expr env x);
    eval_expr env (l, Begin xs)

  | Lambda l -> Closure (l, (fun () -> env))

  | Call (func, args) ->
    let args = List.map (eval_expr env) args in
      (match eval_expr env func with
      | Primitive f -> f args l
      | Closure ((params, body), env_fn) ->
        if List.length params <> List.length args then
          Error.call_err l ~expected:(List.length params)
            ~found:(List.length args)
        else
          let env' = bind_list (env_fn ()) params args in
            eval_expr env' body
      | other -> Error.type_err l ~expected:"function" ~found:(render_val other))

  | LetX (Let, bindings, body) ->
    let bindings = List.map (fun (n, e) -> (n, eval_expr env e)) bindings in
    let env' = bind_pairs env bindings in
      eval_expr env' body

  | LetX (LetStar, [], body) -> eval_expr env body
  | LetX (LetStar, (name, expr) :: bindings, body) ->
    let env' = bind env name (eval_expr env expr) in
      eval_expr env' (l, LetX (LetStar, bindings, body))

  | LetX (LetRec, bindings, body) ->
    let rec env' () = bind_pairs env (List.map map_binding bindings)
    and map_binding (name, expr) = match expr with
      | (l, Lambda (params, expr)) ->
        (name, Closure ((params, expr), env'))
      | (l, _) -> Error.rec_err l name (* TODO: Make this error better. *)
    in
     eval_expr (env' ()) body

let rec eval_def env (l, def) = match def with
  | Val (name, expr) ->
    let v = eval_expr env expr in
      (bind env name v, v)

  | ValRec (name, (l, Lambda lb)) ->
    let rec env' () = bind env name closure
    and closure = Closure (lb, env')
    in
      (env' (), closure)
  | ValRec (name, (l, _)) -> Error.rec_err l name

  | Expr expr -> eval_def env (l, Val ("_", expr))

  | Define (name, params, body) ->
    eval_def env (l, ValRec (name, (l, Lambda (params, body))))

  | Use _
  | CheckExpect (_, _)
  | CheckError _
  | CheckType (_, _)
  | CheckPrincipalType (_, _)
  | CheckTypeError _ -> Error.unit_test_err l

(* ----------------------------------------------------------------------------
 * Error checking.
 *)

let rec check_test (gamma, env) (n_passed, n_tests) (l, def) = match def with
  | CheckExpect (e, e') ->
    (try
      let (t, c) = typecheck_expr gamma e in
      let (t', c') = typecheck_expr gamma e' in
      let _ = con_solve l (con_join [c; c'; ConEq (t, t')]) in
      let (v, v') = (eval_expr env e, eval_expr env e') in
        if eq_val v v' then
          (n_passed + 1, n_tests + 1)
        else
          (eprintf "check-expect test failed\nExpected %s, but got %s\n"
            (render_val v') (render_val v);
          Error.print_loc l;
          (n_passed, n_tests + 1))
    with Error.NanoML_err e ->
      eprintf "check-expect test failed\n";
      Error.print_err e;
      (n_passed, n_tests + 1))

  | CheckError e ->
    (try
      let (t, c) = typecheck_expr gamma e in
      let _ = con_solve l c in
      let v = eval_expr env e in
        eprintf "check-error test failed\nGot %s instead\n" (render_val v);
        Error.print_loc l;
        (n_passed, n_tests + 1)
    with Error.NanoML_err e ->
      (n_passed + 1, n_tests + 1))

  | CheckType (e, t') ->
    (try
      let (t, c) = typecheck_expr gamma e in
      let theta = con_solve l c in
      let sigma = generalize_ty (subst_ty theta t) (free_tyvars_tyenv gamma) in
        if ty_as_general_as l sigma t' then
          (n_passed + 1, n_tests + 1)
        else
          (eprintf "check-type test failed\n";
          eprintf "Expected expression to be %s, but it was %s instead\n"
            (render_tyscheme t') (render_tyscheme sigma);
          Error.print_loc l;
          (n_passed, n_tests + 1))
    with Error.NanoML_err e ->
      eprintf "check-type test failed\n";
      Error.print_err e;
      (n_passed, n_tests + 1))

  | CheckPrincipalType (e, t') ->
    (try
      let (t, c) = typecheck_expr gamma e in
      let theta = con_solve l c in
      let sigma = generalize_ty (subst_ty theta t) (free_tyvars_tyenv gamma) in
        if (ty_as_general_as l sigma t') && (ty_as_general_as l t' sigma) then
          (n_passed + 1, n_tests + 1)
        else
          (eprintf "check-principal-type test failed\n";
          eprintf "Expected expression to be %s, but it was %s instead\n"
            (render_tyscheme t') (render_tyscheme sigma);
          Error.print_loc l;
          (n_passed, n_tests + 1))
    with Error.NanoML_err e ->
      eprintf "check-principal-type test failed\n";
      Error.print_err e;
      (n_passed, n_tests + 1))

  | CheckTypeError e ->
    (try
      let (t, c) = typecheck_expr gamma e in
      let theta = con_solve l c in
      let sigma = generalize_ty (subst_ty theta t) (free_tyvars_tyenv gamma) in
        eprintf "check-type-error test failed\nGot %s instead\n" (render_tyscheme sigma);
        Error.print_loc l;
        (n_passed, n_tests + 1)
    with Error.NanoML_err e ->
      (n_passed + 1, n_tests + 1))

  | _ -> raise NanoML_NeverHappen_err

let is_test (_, def) = match def with
  | CheckExpect (_, _)
  | CheckError _
  | CheckType (_, _)
  | CheckPrincipalType (_, _)
  | CheckTypeError _ -> true
  | _ -> false

(* ----------------------------------------------------------------------------
 * File and REPL reading and execution.
 *)

(* Executes a def, first type-checking it, then evaluating it and producing
 * a new variable and type environment. Prints the output of eval if the
 * print argument is true. *)
let rec do_def print (gamma, env) (l, def) = match def with
  | Use filename ->
      begin
      try
        let xs = Repl.parse_file filename in
        use_func (gamma, env) filename xs
      with
        Sys_error msg -> Error.use_err l ~filename ~msg
      end
  | _ ->
    try
      let (gamma', ty) = typecheck_def gamma (l, def) in
      let (env', v) = eval_def env (l, def) in
        (if print then
          Printf.printf "%s : %s\n" (render_val v) (render_tyscheme ty)
        else ()); (gamma', env')
    with Error.NanoML_err e -> Error.print_err e; (gamma, env)

and use_func (gamma, env) filename xs =
  let defs = List.map parse_def xs in
  let (tests, defs) = List.partition is_test defs in
  let (gamma, env) = List.fold_left (do_def false) (gamma, env) defs in
  let (n_passed, n_tests) = List.fold_left (check_test (gamma, env)) (0, 0) tests in
    Printf.printf "Imported %s\n%d of %d tests passed\n" filename n_passed n_tests;
    (gamma, env)

let rec repl_func ast (gamma, env) =
  try
    let def = parse_def ast in
    do_def true (gamma, env) def
  with Error.NanoML_err e -> Error.print_err e; (gamma, env)

