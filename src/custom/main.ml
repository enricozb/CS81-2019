let rec run (ty_env, val_env) ast =
  let (ty_env, ty) = Type.typecheck ty_env ast in
  let (val_env, v) = Eval.eval val_env ast in
  Printf.printf "%s : %s\n" (Value.string_of_value v) (Type.string_of_type ty);
  (ty_env, val_env)


let rec repl_func ast (ty_env, val_env) =
  try
    run (ty_env, val_env) ast
  with
    Error.MythError (l, e) ->
      Error.print_error l e;
      (ty_env, val_env)


let () =
  let (ty_env, val_env) = (Basis.ty_env, Basis.val_env) in
  try
    Repl.repl repl_func (ty_env, val_env)
  with
    | End_of_file -> exit 0

