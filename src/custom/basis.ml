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
    (fun a b -> match a, b with
      | (Value.Int a, Value.Int b) -> Value.Int (f a b)
      | _ -> failwith "Runtime type error"
    )

let num_int_to_num f =
  binary_fun
    Type.int_ty Type.int_ty
    (fun a b -> match a, b with
      | (Value.Int a, Value.Int b) ->
        begin try
          Value.Int (f a (Z.to_int b))
        with
          Z.Overflow ->
            (* TODO: change loc ASAP *)
            Error.runtime_error
            ({filename = "none";
              start_line = -1;
              start_char = -1;
              end_line   = -1;
              end_char   = -1 })
            "exponent too large"
        end
      | _ -> failwith "Runtime type error"
    )

let num_num_to_bool f =
  binary_fun
    Type.int_ty Type.int_ty
    (fun a b -> match a, b with
      | (Value.Int a, Value.Int b) -> Value.Bool (f a b)
      | _ -> failwith "Runtime type error"
    )

let bool_bool_to_bool f =
  binary_fun
    Type.bool_ty Type.bool_ty
    (fun a b -> match a, b with
      | (Value.Bool a, Value.Bool b) -> Value.Bool (f a b)
      | _ -> failwith "Runtime type error"
    )

let len =
  unary_fun
    Type.list_gen_ty
    (fun lst -> match lst with
      | (List lst) -> Value.Int (Z.of_int (List.length lst))
      | _ -> failwith "Runtime type error"
    )

let head =
  unary_fun
    Type.list_gen_ty
    (fun lst -> match lst with
      | (Value.List lst) -> (List.hd lst)
      | _ -> failwith "Runtime type error"
    )

let tail =
  unary_fun
    Type.list_gen_ty
    (fun lst -> match lst with
      | (Value.List lst) -> Value.List (List.tl lst)
      | _ -> failwith "Runtime type error"
    )

let cons =
  binary_fun
    Type.gen_var_ty Type.list_gen_ty
    (fun x lst -> match x, lst with
      | x, (Value.List lst) -> (Value.List (x :: lst))
      | _ -> failwith "Runtime type error"
    )

let print =
  unary_fun
    Type.gen_var_ty
    (fun value -> Printf.printf "%s\n" (Value.string_of_value value); Value.None)

let val_env = Env.bind_pairs
  (List.map (fun (name, v) -> (name, Value.Const v))
  [("len", len);
   ("head", head);
   ("tail", tail);
   ("cons", cons);

   ("and", bool_bool_to_bool (&&));
   ("or", bool_bool_to_bool (||));

   ("+", num_num_to_num Z.( + ));
   ("-", num_num_to_num Z.( - ));
   ("*", num_num_to_num Z.( * ));
   ("/", num_num_to_num Z.( / ));
   ("^", num_int_to_num Z.( ** ));

   ("==", num_num_to_bool Z.equal);
   ("<=", num_num_to_bool Z.leq);
   (">=", num_num_to_bool Z.geq);
   ("<", num_num_to_bool Z.lt);
   (">", num_num_to_bool Z.gt);
   ("!=", num_num_to_bool (fun a b -> not (Z.equal a b)));

   ("false", Value.Bool false);
   ("true", Value.Bool true);
   ("none", Value.None);

   ("print", print);
  ]) Env.empty

let mut_env = Env.map (fun name -> false) val_env

let ty_env = Env.bind_pairs
  [("len", Type.fun_ty [Type.list_gen_ty] Type.int_ty);
   ("head", Type.fun_ty [Type.list_gen_ty] Type.gen_var_ty);
   ("tail", Type.fun_ty [Type.list_gen_ty] Type.list_gen_ty);
   ("cons", Type.fun_ty [Type.gen_var_ty; Type.list_gen_ty] Type.list_gen_ty);

   ("and", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);
   ("or", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);

   ("+", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);
   ("-", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);
   ("*", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);
   ("/", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);
   ("^", Type.fun_ty [Type.int_ty; Type.int_ty] Type.int_ty);

   ("==", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   ("<=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   (">=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   ("<", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   (">", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);
   ("!=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);

   ("false", Type.bool_ty);
   ("true", Type.bool_ty);
   ("none", Type.none_ty);

   ("print", Type.fun_ty [Type.gen_var_ty] Type.none_ty);
  ] Env.empty

