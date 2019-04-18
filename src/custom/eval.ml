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
  | Ast.Name (l, name) ->
      begin match Env.lookup l name val_env with
        | Value.Const value
        | Value.Mut {contents = value} ->
          (Value value, val_env, frames)
      end

  | Ast.Num (l, i) -> (Value (Value.Int (Z.of_string i)), val_env, frames)
  | Ast.String (l, s) -> (Value (Value.String s), val_env, frames)

  | Ast.List (l, []) -> (Value (Value.List []), val_env, frames)
  | Ast.List (l, ast :: asts) ->
      (Ast ast, val_env, Frame.List (l, [], asts) :: frames)

  (* TODO: the usage of the hash-table here is really bad... *)
  | Ast.Record (l, field_ast_map) when Ast.NameMap.is_empty field_ast_map ->
      (Value (Object.base_object ()), val_env, frames)
  | Ast.Record (l, field_ast_map) ->
      let (field, ast) = Ast.NameMap.choose field_ast_map in
      let field_ast_map = Ast.NameMap.remove field field_ast_map in
      (Ast ast,
       val_env,
       Frame.Object (l, field, Object.base_object (), field_ast_map) :: frames)

  | Ast.Field (l, ast, field) ->
      (Ast ast, val_env, Frame.Field (l, field) :: frames)

  | Ast.Lambda (l, params, body_ast) ->
      let params = List.map fst params in
      let lambda = Value.Lambda ((params, body_ast), (fun () -> val_env)) in
      let func_obj = Object.callable_object (lazy lambda) in
      (Value func_obj, val_env, frames)

  | Ast.Call (l, ast, asts) ->
      (Ast ast, val_env, Frame.Apply (l, [], asts) :: frames)

  | Ast.Bind (l, mut, name, ast) ->
      (Ast ast, val_env, Frame.Bind (l, mut, name) :: frames)

  | Ast.Assign (l, name, ast) ->
      (Ast ast, val_env, Frame.Assign (l, name) :: frames)

  | Ast.Def (l, name, params, _, suite) ->
      let params = List.map fst params in
      let rec closure () = Env.bind name (func_obj ()) val_env
      and func_obj = fun () ->
        let lambda = Value.Lambda ((params, Ast.Suite(l, suite)), closure) in
        Value.Const (Object.callable_object ~name:(Some name) (lazy lambda))
      in
      (Value Value.None, closure (), frames)

  | Ast.If (l, test, suite1, suite2) ->
      (Ast test, val_env, Frame.If (l, suite1, suite2) :: frames)

  | Ast.While (l, test, suite) ->
      (Ast test, val_env, Frame.WhileTest (l, test, suite) :: frames)

  | Ast.Break l ->
      let rec unwind frames = match frames with
        | [] -> Error.flow_outside_loop l
        | Frame.WhileBody _ :: frames ->
            (Value Value.None, val_env, frames)
        | _ :: frames -> unwind frames
      in unwind frames

  | Ast.Continue l ->
      let rec unwind frames = match frames with
        | [] -> Error.flow_outside_loop l
        | Frame.WhileBody (l, test, suite) :: frames ->
            (Ast test, val_env, Frame.WhileTest (l, test, suite) :: frames)
        | _ :: frames -> unwind frames
      in unwind frames

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
  | Frame.Assign (l, name) :: frames ->
      begin try match Env.get name val_env with
        | Value.Mut value_ref ->
            value_ref := value;
            (Value value, val_env, frames)
        | Value.Const _ ->
            Error.runtime_error l "Eval: assigning to a non-mutable binding"
      with Not_found ->
        Error.runtime_error l "Eval: assigning to a non-existing binding"
      end

  | Frame.Bind (l, mut, name) :: frames ->
      if mut then
        (Value value, Env.bind name (Value.Mut (ref value)) val_env, frames)
      else
        (Value value, Env.bind name (Value.Const value) val_env, frames)

  | Frame.List (l, values_so_far, []) :: frames ->
      let values = List.rev (value :: values_so_far) in
      (Value (Value.List values), val_env, frames)
  | Frame.List (l, values_so_far, ast :: asts) :: frames ->
      (Ast ast, val_env, (Frame.List (l, value :: values_so_far, asts) :: frames))

  | Frame.Object (l, name, obj, field_ast_map) :: frames
    when Ast.NameMap.is_empty field_ast_map ->
      Object.set_object_field obj name (lazy value);
      (Value obj, val_env, frames)
  | Frame.Object (l, field, obj, field_ast_map) :: frames ->
      (* add just computed field to map *)
      Object.set_object_field obj field (lazy value);
      (* get next field and ast to compute *)
      let (field, ast) = Ast.NameMap.choose field_ast_map in
      let field_ast_map = Ast.NameMap.remove field field_ast_map in
      (Ast ast,
       val_env,
       Frame.Object (l, field, obj, field_ast_map) :: frames)

  | Frame.Field (l, field) :: frames ->
      let value = Object.get_object_field value field in
      (Value value, val_env, frames)

  | Frame.If (l, suite1, suite2) :: frames ->
      if Value.truthy l value then
        (Ast (Ast.Suite (l, suite1)), val_env, frames)
      else
        (Ast (Ast.Suite (l, suite2)), val_env, frames)

  | Frame.WhileTest (l, test, suite) :: frames ->
      if Value.truthy l value then
        let frames = Frame.WhileBody (l, test, suite) :: frames in
        (Ast (Ast.Suite (l, suite)), val_env, frames)
      else
        (Value Value.None, val_env, frames)

  | Frame.WhileBody (l, test, suite) :: frames ->
      (Ast test, val_env, WhileTest (l, test, suite) :: frames)

  | Frame.CallEnv (val_env) :: frames ->
      (Value value, val_env, frames)

  | Frame.Apply (l, values_so_far, []) :: frames ->
      let values = List.rev (value :: values_so_far) in
      let callable_value = List.hd values in
      let param_values = List.tl values in
      eval_call l callable_value param_values val_env frames
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


and eval_call l callable_value param_values val_env frames =
  match Object.get_func_from_callable callable_value with
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
        let val_env' =
          Env.bind_many
            params
            (List.map (fun v -> Value.Const v) param_values)
            (env_fn ())
        in
        (Ast body_ast, val_env', Frame.CallEnv val_env :: frames)
  | _ ->
      Error.call_field_error l (Value.string_of_value callable_value)

