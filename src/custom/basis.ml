let unary_fun ty f = Value.Builtin (fun vals loc ->
  match vals with
  | [value] ->
    begin try
      f value
    with Match_failure e ->
      Error.type_mismatch_error loc
        ~expected: (Type.string_of_type ty)
        ~provided: (Value.string_of_value value)
    end

  | _ ->
      Error.call_len_error loc
        ~fun_ty: "builitn"
        ~expected: 1
        ~provided: (List.length vals)
  )

let binary_fun ty1 ty2 f = Value.Builtin (fun vals loc ->
  match vals with
  | [value1; value2] ->
    begin try
      f value1 value2
    with Match_failure e ->
      Error.type_mismatch_error loc
        ~expected: ("{" ^ Type.string_of_type ty1 ^ ", " ^
                          Type.string_of_type ty2 ^ "}")
        ~provided: ("{" ^ Value.string_of_value value1 ^ ", " ^
                         Value.string_of_value value2 ^ "}")
    end

  | _ ->
      Error.call_len_error loc
        ~fun_ty: "builitn"
        ~expected: 2
        ~provided: (List.length vals)
  )

let num_num_to_num f =
  binary_fun
    Type.int_ty Type.int_ty
    (fun (Value.Int a) (Value.Int b) -> Value.Int (f a b))

let num_num_to_bool f =
  binary_fun
    Type.int_ty Type.int_ty
    (fun (Value.Int a) (Value.Int b) -> Value.Bool (f a b))

let len =
  unary_fun
    Type.list_gen_ty
    (fun (List lst) -> Value.Int (Z.of_int (List.length lst)))

let head =
  unary_fun
    Type.list_gen_ty
    (fun (List lst) -> (List.hd lst))

let tail =
  unary_fun
    Type.list_gen_ty
    (fun (List lst) -> Value.List (List.tl lst))

let cons =
  binary_fun
    Type.gen_var_ty Type.list_gen_ty
    (fun x (List lst) -> (Value.List (x :: lst)))

let print =
  unary_fun
    Type.list_gen_ty
    (fun value -> Printf.printf "%s\n" (Value.string_of_value value); Value.None)

let val_env = Env.bind_pairs
  [("len", len);
   ("head", head);
   ("tail", tail);
   ("cons", cons);

   ("+", num_num_to_num Z.( + ));
   ("-", num_num_to_num Z.( - ));
   ("*", num_num_to_num Z.( * ));
   ("/", num_num_to_num Z.( / ));

   ("==", num_num_to_bool Z.equal);
   ("<=", num_num_to_bool Z.leq);
   (">=", num_num_to_bool Z.geq);
   ("!=", num_num_to_bool (fun a b -> not (Z.equal a b)));

   ("false", Value.Bool false);
   ("true", Value.Bool true);
   ("none", Value.None);

   ("print", print);
  ] Env.empty

let ty_env = Env.bind_pairs
  [("len", Type.fun_ty [Type.list_gen_ty] Type.int_ty);
   ("head", Type.fun_ty [Type.list_gen_ty] Type.gen_var_ty);
   ("tail", Type.fun_ty [Type.list_gen_ty] Type.list_gen_ty);
   ("cons", Type.fun_ty [Type.gen_var_ty; Type.list_gen_ty] Type.list_gen_ty);

   ("+", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);
   ("-", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);
   ("/", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);
   ("*", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);

   ("==", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   ("<=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   (">=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   ("!=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);

   ("false", Type.bool_ty);
   ("true", Type.bool_ty);
   ("none", Type.none_ty);

   ("print", Type.fun_ty [Type.gen_var_ty] Type.none_ty);
  ] Env.empty

