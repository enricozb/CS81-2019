let passed_tests = ref 0
let total_tests = ref 0
let curr_level = ref 1

let print_tests_stats () =
  Printf.printf "%s\n"
    ("passed " ^ (string_of_int !passed_tests) ^ " of " ^
     (string_of_int !total_tests) ^ " tests.")

(* runs ast then returns type and value *)
let run_ty_val (ty_env, mut_env, val_env) ast =
  let (ty_env, mut_env, ty) = Type.typecheck ~level:!curr_level ty_env mut_env ast in
  let (val_env, v) = Eval.eval val_env ast in
  (ty_env, mut_env, val_env, ty, v)


(* handles non-checks and non-imports, does not return type nor value *)
let run_ast envs ?(quiet=false) ast =
  incr curr_level;
  let (ty_env, mut_env, val_env, ty, value) = run_ty_val envs ast in
  if not quiet then
    Printf.printf "%s : %s\n"
      (Value.string_of_value value) (Type.string_of_type ty);
  (ty_env, mut_env, val_env)


let run_check (ty_env, mut_env, val_env) ast =
  incr total_tests;
  let success = match ast with
  | Ast.CheckExpect (l, ast1, ast2) ->
    let (_, _, _, _, value1) = run_ty_val (ty_env, mut_env, val_env) ast1 in
    let (_, _, _, _, value2) = run_ty_val (ty_env, mut_env, val_env) ast2 in
    value1 = value2

  | Ast.CheckError (l, ast) ->
    let (_, _, _) = Type.typecheck ~level:!curr_level ty_env mut_env ast in
    begin try
      ignore (Eval.eval val_env ast);
      false
    with
      _ -> true
    end

  | Ast.CheckTypeError (l, ast) ->
    begin try
      ignore (Type.typecheck ~level:!curr_level ty_env mut_env ast);
      false
    with
      _ -> true
    end
  | _ -> failwith "Main.run_check called on non check* ast"
  in

  if success then
    incr passed_tests;

  if not success then
    Printf.printf "%s\n" ("failed: " ^ Ast.string_of_ast ast);
  (ty_env, mut_env, val_env)

let rec run ?(quiet=false) (ty_env, mut_env, val_env) ast =
  match ast with
  | Ast.CheckExpect _ | Ast.CheckError _ | Ast.CheckTypeError _ ->
      run_check (ty_env, mut_env, val_env) ast

  | Ast.Import (l, name) ->
    let asts = Repl.parse_file (name ^ ".my") in
    let (ty_env, mut_env, val_env) =
      List.fold_left (run ~quiet:true) (ty_env, mut_env, val_env) asts in
    if not quiet then
      print_tests_stats ();
    (ty_env, mut_env, val_env)

  | Ast.Bind _ | Ast.Def _ | Ast.Assign _ ->
      run_ast (ty_env, mut_env, val_env) ~quiet:quiet ast

  | _ ->
      run_ast (ty_env, mut_env, val_env) ~quiet:quiet
        (Ast.Bind (Ast.loc_of_ast ast, false, "_", ast))


let rec repl_func ast_or_error (ty_env, mut_env, val_env) =
  try
    match ast_or_error with
    | `Ast ast ->
        run (ty_env, mut_env, val_env) ast
    | `ParsingError loc ->
        Error.syntax_error loc ""
  with
    Error.MythError (l, e) ->
      Error.print_error l e;
      (ty_env, mut_env, val_env)


let () =
  let (ty_env, mut_env, val_env) = (Basis.ty_env, Basis.mut_env, Basis.val_env) in
  try
    Repl.repl repl_func (ty_env, mut_env, val_env)
  with
    | End_of_file -> exit 0

