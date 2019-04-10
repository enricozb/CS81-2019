let fake_loc = Loc.({filename = "none";
                 start_line = -1;
                 start_char = -1;
                 end_line   = -1;
                 end_char   = -1 })

let build_callable f = Value.Object (BatHashtbl.of_list [("__call__", f)])

let get_obj_field obj field =
  match obj with
  | Value.Object fields ->
      begin match BatHashtbl.find_option fields field with
        | Some v -> v
        | None ->
            failwith ("Basis.get_obj_field can't find field '" ^ field ^ "'")
      end
  | _ ->
      failwith "Basis.get_obj_field called on non object"

let call_prim_func f values =
  match f with
  | Value.Builtin primop -> primop values fake_loc
  | _ -> failwith "Basis.call_prim_func called on non-Value.Builtin"

let prim_unary_fun ty f = Value.Builtin (fun vals loc ->
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

let prim_binary_fun ty1 ty2 f = Value.Builtin (fun vals loc ->
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

let unary_fun ty f = build_callable (prim_unary_fun ty f)
let binary_fun ty1 ty2 f = build_callable (prim_binary_fun ty1 ty2 f)

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
            (* TODO: change fake_loc ASAP *)
            Error.runtime_error fake_loc "exponent too large"
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

let callable =
  unary_fun
    Type.gen_var_ty
    (fun value -> match value with
      | Value.Object fields ->
        begin match BatHashtbl.find_option fields "__call__" with
        | Some (Value.Builtin _)
        | Some (Value.Lambda _) -> Value.Bool true
        | _ -> Value.Bool false
        end
      | _ -> Value.Bool false
    )
(* ------------------------------- OPERATORS ------------------------------- *)
let operator_ty field =
  Type.callable_ty
  [(Type.has_field_ty
      field
      (Type.callable_ty [Type.gen_var_ty] Type.gen_var_ty2)
   );
   Type.gen_var_ty]
  Type.gen_var_ty2

let operator_func field =
  binary_fun
  (Type.has_field_ty
    field
    (Type.callable_ty [Type.gen_var_ty] Type.gen_var_ty2)
  )
  Type.gen_var_ty
  (fun obj b ->
      let f = get_obj_field (get_obj_field obj field) "__call__" in
      call_prim_func f [b])

let operator field = (operator_ty field, operator_func field)

let add_ty, add = operator "__add__"
let sub_ty, sub = operator "__sub__"
let mul_ty, mul = operator "__mul__"
let div_ty, div = operator "__div__"
let pow_ty, pow = operator "__pow__"


(* -------------------------------- CLASSES -------------------------------- *)
(* binds obj to the first parameter of func. Used when creating an instance
 * of a class. `func` and `obj` are of type Value.value
 * func needs to be a callable with `__call__` being a Value.Builtin.
 * returns a callable that has it's first argument bound.
 *)
let bind_self callable obj =
  let func = get_obj_field callable "__call__" in
  let newfunc = match func with
    | Value.Builtin primop ->
        Value.Builtin (fun vals loc ->
          let vals = obj :: vals
          in primop vals loc)

    | _ -> failwith "Basis.bind_self on non Value.Builtin"
  in
  build_callable newfunc


(* ---------------------------------- Int ---------------------------------- *)
let int_object_ty =
  (* needs to be lazy to appease OCaml's restriction on let rec values *)
  let rec inner_record = lazy (Type.bare_record_ty
    [("val", Type.prim_int_ty);
     ("__add__", Type.fun_ty
              [TyFold (Some "Int", inner_record)]
              (TyFold (Some "Int", inner_record)));
     ("__sub__", Type.fun_ty
              [TyFold (Some "Int", inner_record)]
              (TyFold (Some "Int", inner_record)));
     ("__mul__", Type.fun_ty
              [TyFold (Some "Int", inner_record)]
              (TyFold (Some "Int", inner_record)));
     ("__div__", Type.fun_ty
              [TyFold (Some "Int", inner_record)]
              (TyFold (Some "Int", inner_record)));
     ("__pow__", Type.fun_ty
              [TyFold (Some "Int", inner_record)]
              (TyFold (Some "Int", inner_record)));
    ])
  in
  Type.TyFold (Some "Int", inner_record)

let int_class_ty =
  Type.folded_record_ty None
    [("__call__", Type.prim_fun_ty [Type.prim_int_ty] int_object_ty);
     ("__add__", Type.fun_ty [int_object_ty; int_object_ty] int_object_ty);
     ("__sub__", Type.fun_ty [int_object_ty; int_object_ty] int_object_ty);
     ("__mul__", Type.fun_ty [int_object_ty; int_object_ty] int_object_ty);
     ("__div__", Type.fun_ty [int_object_ty; int_object_ty] int_object_ty);
     ("__pow__", Type.fun_ty [int_object_ty; int_object_ty] int_object_ty)]

let int_class =
  let rec int_uninitialized_object () =
    let obj = BatHashtbl.of_list [("val", Value.Int Z.zero)] in
    BatHashtbl.add obj "__add__" (bind_self (int_add ()) (Value.Object obj));
    BatHashtbl.add obj "__sub__" (bind_self (int_sub ()) (Value.Object obj));
    BatHashtbl.add obj "__mul__" (bind_self (int_mul ()) (Value.Object obj));
    BatHashtbl.add obj "__div__" (bind_self (int_div ()) (Value.Object obj));
    BatHashtbl.add obj "__pow__" (bind_self (int_pow ()) (Value.Object obj));
    obj
  and int_call () =
    prim_unary_fun
      Type.prim_int_ty
      (fun v -> match v with
        | (Value.Int _) ->
            let obj = int_uninitialized_object () in
            BatHashtbl.replace obj "val" v;
            Value.Object obj
        | _ -> failwith "Runtime type error"
      )
  and int_op f () =
    binary_fun
      int_object_ty int_object_ty
      (fun a b -> match (a, b) with
        | (Value.Object self, Value.Object other) ->
            let self_v = BatHashtbl.find self "val" in
            let other_v = BatHashtbl.find other "val" in
            begin match self_v, other_v with
              | (Value.Int x, Value.Int y) ->
                let obj = int_uninitialized_object () in
                BatHashtbl.replace obj "val" (Value.Int (f x y));
                Value.Object obj
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and int_add () = int_op Z.( + ) ()
  and int_sub () = int_op Z.( - ) ()
  and int_mul () = int_op Z.( * ) ()
  and int_div () = int_op Z.( / ) ()
  and int_pow () = int_op (fun a b -> Z.(a ** to_int b)) ()
  in
  Value.build_object
    [("__call__", int_call ());
     ("__add__", int_add ());
     ("__sub__", int_sub ());
     ("__mul__", int_mul ());
     ("__div__", int_div ());
     ("__pow__", int_pow ());
    ]

(* -------------------------------- String -------------------------------- *)

(* -------------------------- Basis Environments -------------------------- *)
let val_env = Env.bind_pairs
  (List.map (fun (name, v) -> (name, Value.Const v))
  [("len", len);
   ("head", head);
   ("tail", tail);
   ("cons", cons);

   ("and", bool_bool_to_bool (&&));
   ("or", bool_bool_to_bool (||));

   ("+", add);
   ("-", sub); 
   ("*", mul);
   ("/", div);
   ("^", pow);

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

   ("callable", callable);

   ("Int", int_class);
  ]) Env.empty

let mut_env = Env.map (fun name -> false) val_env

let ty_env = Env.bind_pairs
  [("len", Type.fun_ty [Type.list_gen_ty] Type.int_ty);
   ("head", Type.fun_ty [Type.list_gen_ty] Type.gen_var_ty);
   ("tail", Type.fun_ty [Type.list_gen_ty] Type.list_gen_ty);
   ("cons", Type.fun_ty [Type.gen_var_ty; Type.list_gen_ty] Type.list_gen_ty);

   ("and", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);
   ("or", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);

   ("+", add_ty);
   ("-", sub_ty);
   ("*", mul_ty);
   ("/", div_ty);
   ("^", pow_ty);

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

   ("callable", Type.fun_ty [Type.gen_var_ty] Type.bool_ty);

   ("Int", int_class_ty);
  ] Env.empty

