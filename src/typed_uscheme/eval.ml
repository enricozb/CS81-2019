(*
 * The Typed UScheme interpreter.
 *)

open Syntax
open Env

let bind_all = List.fold_left (fun e (k, v) -> bind_local e k v)

let (=|=) = Type.(=|=)

(*
 * The evaluator.
 *)

(* Unit tests. *)
type unit_test =
  | CheckExpectTest    of Loc.loc * expr * expr
  | CheckErrorTest     of Loc.loc * expr
  | CheckTypeTest      of Loc.loc * expr * scheme_type
  | CheckTypeErrorTest of Loc.loc * expr

(* The recursive expression evaluator. *)
let rec eval_expr env expr =
  match expr with
    | Literal (_, i) -> IntVal i

    | Var (l, s) ->
      begin
        match lookup l env s with
          | Unspecified s -> Error.name_err l s
          | other -> other
      end

    | Set (l, s, e) ->
      begin
        set l env s (eval_expr env e);
        UnitVal
      end

    | If (l, cond, t, e) ->
      if truthy l (eval_expr env cond) then
        eval_expr env t
      else
        eval_expr env e

    | While (l, cond, body) ->
      if truthy l (eval_expr env cond) then
        begin
          ignore (eval_expr env body);
          eval_expr env expr
        end
      else
        UnitVal

    | Begin (_, []) -> UnitVal
    | Begin (_, [x]) -> eval_expr env x
    | Begin (l, x :: xs) ->
      begin
        ignore (eval_expr env x);
        eval_expr env (Begin (l, xs))
      end

    | Let (_, bindings, body) ->
      (* First evaluate *all* bindings in the outer environment. *)
      let bindings =
        List.map
          (fun (name, expr) -> (name, eval_expr env expr))
          bindings
      in
      let env' = bind_all env bindings in
        eval_expr env' body

    | LetStar (_, bindings, body) ->
      let folder env (name, expr) =
        bind_local env name (eval_expr env expr)
      in
      let env' = List.fold_left folder env bindings in
        eval_expr env' body

    | Lambda (_, formals, body) ->
      UserFuncVal (List.map fst formals, body, env)

    | Call (l, func, args) ->
      let actuals = List.map (eval_expr env) args in
        begin
          match eval_expr env func with
            | PrimFuncVal f -> f actuals l
            | UserFuncVal (formals, body, closure) ->
              if List.length formals <> List.length actuals then
                Error.call_err l
                  ~expected:(List.length formals)
                  ~found:(List.length actuals)
              else
                let closure = bind_all closure (List.combine formals actuals) in
                  eval_expr closure body
            | other -> Error.type_err l
                         ~expected:"function"
                         ~found:(string_of_val other)
        end

    | Narrow (_, e, tys) -> eval_expr env e
    | TypeLambda (_, tyvars, e) -> eval_expr env e

(* The environment type to use for top-level typechecking and evaluation: a
 * value environment and a type environment. *)
type env = {
  val_env: Env.env;
  type_env: Type.env;
}

(* Possible results from top-level evaluator. *)
type result = UseResult of string
            | ValueResult of value * scheme_type
            | DefineResult of string * value * scheme_type

(* Run a unit test. Return whether the test passed. Prints a message otherwise.
*)
let eval_test env = function
  | CheckExpectTest (l, to_check, result) ->
    ignore (Type.typecheck_expr env.type_env to_check);
    ignore (Type.typecheck_expr env.type_env result);
    let actual = eval_expr env.val_env to_check in
    let expected = eval_expr env.val_env result in
    if actual <> expected then begin
      Error.eprintf "check-expect failed: got %s but expected %s\n"
        (string_of_val actual) (string_of_val expected);
      Error.print_loc l;
      false
    end else
      true
  | CheckErrorTest (l, to_check) ->
    ignore (Type.typecheck_expr env.type_env to_check);
    begin try
        ignore (eval_expr env.val_env to_check);
        Error.eprintf "check-error failed: evaluated with no error\n";
        Error.print_loc l;
        false
      with Error.UScheme_err _ -> true
    end
  | CheckTypeTest (l, to_check, expected) ->
    let actual = Type.typecheck_expr env.type_env to_check in
    if not (actual =|= expected) then begin
      Error.eprintf "check-type failed: found %s; expected %s\n"
        (Type.string_of_type actual) (Type.string_of_type expected);
      Error.print_loc l;
      false
    end else
      true
  | CheckTypeErrorTest (l, to_check) ->
    begin try
      ignore (Type.typecheck_expr env.type_env to_check);
      false
    with
      Error.UScheme_err _ -> true
    end

(* The evaluator for top-level forms. *)
let rec eval_def env def =
  match def with
    | Define (l, name, retty, formals, e) ->
      let functype = FunctionType (List.map snd formals, retty) in
      let type_env = Type.typecheck_define env.type_env l name retty formals e in
      let v = eval_expr env.val_env (Lambda (l, formals, e)) in
        begin
          define env.val_env name v;
          ({env with type_env}, DefineResult (name, v, functype))
        end

    | Expr (_, e) ->
      let ty = Type.typecheck_expr env.type_env e in
      let v = eval_expr env.val_env e in
        begin
          define env.val_env "it" v;
          (env, ValueResult (v, ty))
        end

    | Use (l, filename) ->
      (* Read in and parse the file. *)
      let channel = try
          open_in filename
        with
          Sys_error msg -> Error.use_err l ~filename ~msg
      in
      let lexbuf = Lexing.from_channel channel in
      let (env, count_passed, count_tests) = use_lexbuf lexbuf filename env in
        begin
          Printf.printf "%d of %d unit tests passed\n%!"
            count_passed count_tests;
          (env, UseResult filename)
        end
    | CheckExpect _
    | CheckError _
    | CheckType _
    | CheckTypeError _ ->
      Error.unit_test_err (loc_of_def def)

(* Just like `eval_def`, but additionally keeps track of a list of unit tests
 * and can therefore handle unit test forms. *)
and eval_def_in_use env tests def =
  match def with
    | CheckExpect (l, to_check, result) ->
      let test = CheckExpectTest (l, to_check, result) in
        (env, test :: tests)

    | CheckError (l, to_check) ->
      let test = CheckErrorTest (l, to_check) in
        (env, test :: tests)

    | CheckType (l, to_check, ty) ->
      let test = CheckTypeTest (l, to_check, ty) in
        (env, test :: tests)

    | CheckTypeError (l, to_check) ->
      let test = CheckTypeErrorTest (l, to_check) in
        (env, test :: tests)

    | _ ->
      let env, _ = eval_def env def in
        (env, tests)

(* Parse and run the contents of a lexbuf in the current environment.
 *
 * Returns (num_tests_passed, num_total_tests).
 *)
and use_lexbuf lexbuf filename env =
  (* Parse the file. *)
  let sexprs = Parser.parse_many filename lexbuf in

  (* Helper which folds over the list of definitions in the file,
   * keeping track of modifications to both the list of unit tests and
   * the environment. *)
  let folder (env, unit_tests) expr =
    eval_def_in_use env unit_tests (parse_def expr) in
  (* Finally, actually evaluate the contents of the file. *)
  let env, unit_tests = List.fold_left folder (env, []) sexprs in

  (* Helper which folds over the unit tests, printing a message if one
   * fails and keeping track of how many pass (in `count`). *)
  let test_folder test count =
    if eval_test env test then count + 1 else count
  in

  (* Fold that over unit_tests.  Has to be a right fold because we
   * keep unit_tests in reverse order of definition but we want to
   * run the tests in the order of definition. *)
  let count_passed = List.fold_right test_folder unit_tests 0 in
  (env, count_passed, List.length unit_tests)

(* The REPL, in the form expected by Repl. *)
let rec repl_func env = function
  | [] -> env
  | x :: xs -> try
      let def = parse_def x in

      let (env, result) = eval_def env def in
      begin match result with
        | UseResult fname -> Printf.printf "used file: %s\n" fname
        | ValueResult (v, ty) ->
          if not (ty =|= Type.unit_ty) then
            Printf.printf "%s : %s\n" (string_of_val v) (Type.string_of_type ty)
        | DefineResult (name, v, ty) ->
          Printf.printf "%s : %s = %s\n"
            name (Type.string_of_type ty) (string_of_val v)
      end;
      env
    with Error.UScheme_err e -> Error.print_err e;
      repl_func env xs

let () =
  let env =
    {
      val_env = Basis.basis_vals;
      type_env = Basis.basis_types
    }
  in
  let basis_buf = Lexing.from_string Basis.scheme_basis in
    begin
      ignore (use_lexbuf basis_buf "<initial basis>" env);
      try
        ignore (Repl.make_repl repl_func env)
      with
        End_of_file -> exit 0
    end

