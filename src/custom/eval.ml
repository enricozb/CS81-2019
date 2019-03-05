let rec eval val_env = function
  | Ast.Name (l, id) -> (val_env, Env.lookup l id val_env)

  | Ast.Num (l, i) -> (val_env, Value.Int (Z.of_string i))

  | Ast.List (l, asts) ->
      let (val_envs, values) = List.split @@ List.map (eval val_env) asts in
      (val_env, Value.List values)

  | Ast.Lambda (l, params, body_ast) ->
      (val_env, Value.Lambda ((params, body_ast), (fun () -> val_env)))

  | Ast.If (l, test, suite1, suite2) ->
      let (val_env, Value.Bool test_val) = eval val_env test in
      if test_val then
        let (_, value) = eval val_env (Ast.Suite (l, suite1)) in
        (val_env, value)
      else
        let (_, value) = eval val_env (Ast.Suite (l, suite2)) in
        (val_env, value)

  | Ast.Call (l, fun_ast, param_asts) ->
      let (val_envs, param_values) =
        List.split @@ List.map (eval val_env) param_asts in

      begin
        let (val_env, fun_value) = eval val_env fun_ast in
        match fun_value with
        | Value.Lambda ((params, body_ast), env_fn) ->
            if List.length params <> List.length param_values then
              Error.call_len_error l
                (* TODO : get name of function or something *)
                ~fun_ty: ("function")
                ~expected:(List.length params)
                ~provided:(List.length param_values)
            else
              let val_env' = Env.bind_many params param_values (env_fn ()) in
              let (_, value) = eval val_env' body_ast in
              (val_env, value)
        | Value.Builtin primop ->
           (val_env, primop param_values l)
        | _ -> Error.call_error l (Value.string_of_value fun_value)
      end

  | Ast.Bind (l, id, ast) ->
      let (val_env, value) = eval val_env ast in
      (Env.bind id value val_env, value)

  | Ast.Def (l, name, params, suite) ->
      let rec closure () = Env.bind name lambda val_env
      and lambda = Value.Lambda ((params, Ast.Suite(l, suite)), closure)
      in
      (closure (), lambda)

  | Ast.Suite (l, []) -> failwith "Eval.evel on empty Ast.Suite"
  | Ast.Suite (l, [ast]) -> eval val_env ast
  | Ast.Suite (l, ast :: asts) ->
      let (val_env, _) = eval val_env ast in
      eval val_env (Ast.Suite (l, asts))

  | _ ->
      Printf.printf "Eval.eval: This ast is not yet supported\n";
      (val_env, Value.None)

