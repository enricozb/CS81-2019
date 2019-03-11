type item =
  | Ast of Ast.ast
  | Value of Value.value

let rec eval val_env ast =
  let frames = [] in
  let (v, val_env, _) = eval_item val_env frames (Ast ast) in
  (val_env, v)

and eval_item val_env frames item =
  match item, frames with
  | Ast ast, _ ->
      let (item, val_env, frames) = reduce_ast val_env frames ast in
      eval_item val_env frames item

  | Value v, [] -> (v, val_env, frames)
  | Value v, _ ->
      let (item, val_env, frames) = eval_val val_env frames v in
      eval_item val_env frames item


and reduce_ast val_env frames = function
  | Ast.Name (l, name) -> (Value (Env.lookup l name val_env), val_env, frames)

  | Ast.Num (l, i) -> (Value (Value.Int (Z.of_string i)), val_env, frames)

  | Ast.List (l, []) -> (Value (Value.List []), val_env, frames)
  | Ast.List (l, ast :: asts) ->
      (Ast ast, val_env, Frame.List (l, [], asts) :: frames)

  | Ast.Lambda (l, params, body_ast) ->
      let value = Value.Lambda ((params, body_ast), (fun () -> val_env)) in
      (Value value, val_env, frames)

  | Ast.Call (l, ast, asts) ->
      (Ast ast, val_env, Frame.Apply (l, [], asts) :: frames)

  | Ast.Bind (l, name, ast) ->
      (Ast ast, val_env, Frame.Bind (l, name) :: frames)

  | Ast.Def (l, name, params, suite) ->
      let rec closure () = Env.bind name lambda val_env
      and lambda = Value.Lambda ((params, Ast.Suite(l, suite)), closure) in
      (Value Value.None, closure (), frames)

  | Ast.If (l, test, suite1, suite2) ->
      (Ast test, val_env, Frame.If (l, suite1, suite2) :: frames)

  | Ast.Suite (_, []) ->
      (Value Value.None, val_env, frames)
  | Ast.Suite (l, ast :: asts) ->
      (Ast ast, val_env, Frame.Suite (l, asts) :: frames)

  | Ast.Return (l, ast) ->
      (Ast ast, val_env, Frame.Return l :: frames)

  | ast ->
      let err =
        Printf.sprintf
          "The ast (%s) is not yet supported\n"
          (Ast.string_of_ast ast)
      in
      failwith err


and eval_val val_env frames value =
  match frames with
  | Frame.Bind (l, name) :: frames ->
      (Value value, Env.bind name value val_env, frames)

  | Frame.List (l, values_so_far, []) :: frames ->
      let values = List.rev (value :: values_so_far) in
      (Value (Value.List values), val_env, frames)
  | Frame.List (l, values_so_far, ast :: asts) :: frames ->
      (Ast ast, val_env, (Frame.List (l, value :: values_so_far, asts) :: frames))

  | Frame.If (l, suite1, suite2) :: frames ->
      if Value.truthy l value then
        (Ast (Ast.Suite (l, suite1)), val_env, frames)
      else
        (Ast (Ast.Suite (l, suite2)), val_env, frames)

  | Frame.CallEnv (val_env) :: frames -> (Value value, val_env, frames)

  | Frame.Apply (l, values_so_far, []) :: frames ->
      let values = List.rev (value :: values_so_far) in
      let fun_value = List.hd values in
      let param_values = List.tl values in
      eval_call l fun_value param_values val_env frames
  | Frame.Apply (l, values_so_far, ast :: asts) :: frames ->
      (Ast ast, val_env, (Frame.Apply (l, value :: values_so_far, asts) :: frames))

  | Frame.Suite (l, []) :: frames ->
      (Value Value.None, val_env, frames)
  | Frame.Suite (l, ast :: suite) :: frames ->
      (Ast ast, val_env, (Frame.Suite (l, suite) :: frames))

  | Frame.Return l :: frames ->
      let rec unwind frames = match frames with
        | [] -> Error.return_outside_def l
        | Frame.CallEnv val_env :: frames -> (Value value, val_env, frames)
        | _ :: frames -> unwind frames
      in unwind frames

  | [] -> failwith "Eval.eval_val on empty frames"


and eval_call l fun_value param_values val_env frames =
  match fun_value with
  | Value.Builtin primop ->
     (Value (primop param_values l), val_env, frames)

  | Value.Lambda ((params, body_ast), env_fn) ->
      if List.length params <> List.length param_values then
        Error.call_len_error l
          (* TODO : get name of function or something *)
          ~fun_ty: ("function")
          ~expected:(List.length params)
          ~provided:(List.length param_values)
      else
        let val_env' = Env.bind_many params param_values (env_fn ()) in
        (Ast body_ast, val_env', Frame.CallEnv val_env :: frames)

  | _ -> Error.call_error l (Value.string_of_value fun_value)

