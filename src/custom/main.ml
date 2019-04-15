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
let run_ast envs ast =
  incr curr_level;
  run_ty_val envs ast


let run_check (ty_env, mut_env, val_env) ast =
  incr total_tests;
  let success = match ast with
  | Ast.CheckExpect (l, ast1, ast2) ->
    let (_, _, _, _, v) =
      run_ty_val
        (ty_env, mut_env, val_env)
        (Ast.Call (l, Ast.Name (l, "=="), [ast1; ast2]))
    in
    Value.truthy l v

  | Ast.CheckError (l, ast) ->
    let (_, _, _) = Type.typecheck ~level:!curr_level ty_env mut_env ast in
    begin try
      ignore (Eval.eval val_env ast);
      false
    with
      _ -> true
    end

  | Ast.CheckType (l, ast) ->
    begin
      let (_, _, ty) = Type.typecheck ~level:!curr_level ty_env mut_env ast in
      Printf.printf "%s\n" (Type.string_of_type ty);
      true
    (*with*)
      (*_ -> false*)
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
  | Ast.CheckExpect _
  | Ast.CheckError _
  | Ast.CheckType _
  | Ast.CheckTypeError _ ->
      run_check (ty_env, mut_env, val_env) ast

  | Ast.Import (l, name) ->
    let asts = Repl.parse_file (name ^ ".my") in
    let (ty_env, mut_env, val_env) =
      List.fold_left (run ~quiet:true) (ty_env, mut_env, val_env) asts in
    if not quiet then
      print_tests_stats ();
    (ty_env, mut_env, val_env)

  | _ ->
      if Ast.is_expr ast && not quiet then
        let loc = Ast.loc_of_ast ast in

        (* run and save expression in variable "_" *)
        let (ty_env, mut_env, val_env, ty, v) =
          run_ast (ty_env, mut_env, val_env) (Ast.Bind (loc, false, "_", ast))
        in

        (* if the value is an object get repr of `_` and print it
         * otherwise dispatch the printing to Value.string_of_value *)
        let (ty_env, mut_env, val_env, repr) =
          match v with
          | Value.Object _ ->
            let (ty_env, mut_env, val_env, _, repr) =
              run_ast
                (ty_env, mut_env, val_env)
                (Ast.Call (loc, Ast.Name (loc, "repr"), [Ast.Name (loc, "_")]))
            in
            (* grab raw string out of `repr` *)
            begin match Object.get_object_field repr "val" with
              | Value.String s -> (ty_env, mut_env, val_env, s)
              | _ -> failwith "Main.run: string values is malformed"
            end
          | _ -> (ty_env, mut_env, val_env, Value.string_of_value v)
        in
        Printf.printf "%s : %s\n" repr (Type.string_of_type ty);
        (ty_env, mut_env, val_env)
      else
        let (ty_env, mut_env, val_env, _, _) =
          run_ast (ty_env, mut_env, val_env) ast
        in
        (ty_env, mut_env, val_env)


let run_basis () =
    let asts = Repl.parse_string "<basis>" Basis.basis in
    let envs = List.fold_left (run ~quiet:true) Basis.envs asts in
    print_tests_stats ();
    envs


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
  let envs = run_basis () in
  try
    Repl.repl repl_func envs
  with
    | End_of_file -> exit 0

