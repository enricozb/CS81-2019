let fake_loc = Loc.({filename = "none";
                 start_line = -1;
                 start_char = -1;
                 end_line   = -1;
                 end_char   = -1 })

let build_callable f = Value.Object (BatHashtbl.of_list [("__call__", f)])

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

(* ------------------------------- OPERATORS ------------------------------- *)
let operator_ty field =
  Type.callable_ty ~generic:true
  [(Type.has_field_ty ~generic:true
      field
      (Type.callable_ty ~generic:true [Type.gen_var_ty] Type.gen_var_ty2)
   );
   Type.gen_var_ty]
  Type.gen_var_ty2

let operator_func field =
  binary_fun
  (Type.has_field_ty ~generic:true
    field
    (Type.callable_ty ~generic:true [Type.gen_var_ty] Type.gen_var_ty2)
  )
  Type.gen_var_ty
  (fun obj b ->
      let f =
        Value.get_object_field
          (Value.get_object_field obj field)
          "__call__"
      in
      call_prim_func f [b])

let operator field = (operator_ty field, operator_func field)

let add_ty, add = operator "__add__"
let sub_ty, sub = operator "__sub__"
let mul_ty, mul = operator "__mul__"
let div_ty, div = operator "__div__"
let pow_ty, pow = operator "__pow__"

let eq_ty, eq = operator "__eq__"


(* -------------------------------- CLASSES -------------------------------- *)
(* binds obj to the first parameter of func. Used when creating an instance
 * of a class. `func` and `obj` are of type Value.value
 * func needs to be a callable with `__call__` being a Value.Builtin.
 * returns a callable that has it's first argument bound.
 *)
let bind_self callable obj =
  let func = Value.get_object_field callable "__call__" in
  let newfunc = match func with
    | Value.Builtin primop ->
        Value.Builtin (fun vals loc ->
          let vals = obj :: vals
          in primop vals loc)

    | _ -> failwith "Basis.bind_self on non Value.Builtin"
  in
  build_callable newfunc

let rec int_ty =
  (* needs to be lazy to appease OCaml's restriction on let rec values *)
  let rec inner_record = lazy (Type.bare_record_ty
    [("val", Type.prim_int_ty);
     ("__add__", Type.fun_ty [int_ty] (int_ty));
     ("__sub__", Type.fun_ty [int_ty] (int_ty));
     ("__mul__", Type.fun_ty [int_ty] (int_ty));
     ("__div__", Type.fun_ty [int_ty] (int_ty));
     ("__pow__", Type.fun_ty [int_ty] (int_ty));
     ("__eq__",  Type.fun_ty [int_ty] Type.bool_ty);
     ("__repr__", Type.fun_ty [] string_ty);

    ])
  and int_ty = Type.TyFold (Some ("Int", []), inner_record)
  in
  int_ty

and string_ty =
  (* needs to be lazy to appease OCaml's restriction on let rec values *)
  let rec inner_record = lazy (Type.bare_record_ty
    [("val", Type.prim_string_ty);
     ("__add__", Type.fun_ty [string_ty] (string_ty));
     ("__repr__", Type.fun_ty [] string_ty);
    ])
  and string_ty = Type.TyFold (Some ("String", []), inner_record)
  in
  string_ty

let make_int = ref (fun x -> failwith "Basis.make_int called before ready")
let make_string = ref (fun s -> failwith "Basis.make_string called before ready")


(* ---------------------------------- Int ---------------------------------- *)

let int_class_ty =
  Type.folded_record_ty None
    [("__call__", Type.prim_fun_ty [Type.prim_int_ty] int_ty);
     ("__add__", Type.fun_ty [int_ty; int_ty] int_ty);
     ("__sub__", Type.fun_ty [int_ty; int_ty] int_ty);
     ("__mul__", Type.fun_ty [int_ty; int_ty] int_ty);
     ("__div__", Type.fun_ty [int_ty; int_ty] int_ty);
     ("__pow__", Type.fun_ty [int_ty; int_ty] int_ty);
     ("__eq__", Type.fun_ty [int_ty; int_ty] Type.bool_ty);

     ("__repr__", Type.fun_ty [int_ty] string_ty);
    ]

let int_class =
  let rec int_uninitialized_object () =
    let obj = BatHashtbl.of_list [("val", Value.Int Z.zero)] in
    BatHashtbl.replace obj "__add__" (bind_self (int_add ()) (Value.Object obj));
    BatHashtbl.replace obj "__sub__" (bind_self (int_sub ()) (Value.Object obj));
    BatHashtbl.replace obj "__mul__" (bind_self (int_mul ()) (Value.Object obj));
    BatHashtbl.replace obj "__div__" (bind_self (int_div ()) (Value.Object obj));
    BatHashtbl.replace obj "__pow__" (bind_self (int_pow ()) (Value.Object obj));
    BatHashtbl.replace obj "__eq__"  (bind_self (int_eq ()) (Value.Object obj));
    BatHashtbl.replace obj "__repr__" (bind_self (int_repr ()) (Value.Object obj));
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
      int_ty int_ty
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
  and int_comp f () =
    binary_fun
      int_ty int_ty
      (fun a b -> match (a, b) with
        | (Value.Object self, Value.Object other) ->
            let self_v = BatHashtbl.find self "val" in
            let other_v = BatHashtbl.find other "val" in
            begin match self_v, other_v with
              | (Value.Int x, Value.Int y) ->
                Value.Bool (f x y)
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )

  and int_unary f () =
    unary_fun
      int_ty
      (fun a -> match a with
        | (Value.Object self) ->
            let self_v = BatHashtbl.find self "val" in
            begin match self_v with
              | Value.Int x -> f x
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and int_add () = int_op Z.( + ) ()
  and int_sub () = int_op Z.( - ) ()
  and int_mul () = int_op Z.( * ) ()
  and int_div () = int_op Z.( / ) ()
  and int_pow () = int_op (fun a b -> Z.(a ** to_int b)) ()
  and int_eq () = int_comp Z.equal ()
  and int_repr () = int_unary (fun x -> !make_string (Z.to_string x)) ()
  in
  make_int := (fun x ->
    let obj = int_uninitialized_object () in
    BatHashtbl.replace obj "val" (Value.Int x);
    Value.Object obj);

  Value.build_object
    [("__call__", int_call ());
     ("__add__", int_add ());
     ("__sub__", int_sub ());
     ("__mul__", int_mul ());
     ("__div__", int_div ());
     ("__pow__", int_pow ());
     ("__eq__", int_eq ());
     ("__repr__", int_repr ());
    ]

(* -------------------------------- String -------------------------------- *)
let string_class_ty =
  Type.folded_record_ty None
    [("__call__", Type.prim_fun_ty [Type.prim_string_ty] string_ty);
     ("__add__", Type.fun_ty [string_ty; string_ty] string_ty);
     ("__repr__", Type.fun_ty [string_ty] string_ty);
    ]

let string_class =
  let rec string_uninitialized_object () =
    let obj = BatHashtbl.of_list [("val", Value.String "")] in
    BatHashtbl.replace obj "__add__" (bind_self (string_add ()) (Value.Object obj));
    BatHashtbl.replace obj "__repr__" (bind_self (string_repr ()) (Value.Object obj));
    obj
  and string_call () =
    prim_unary_fun
      Type.prim_string_ty
      (fun v -> match v with
        | (Value.String _) ->
            let obj = string_uninitialized_object () in
            BatHashtbl.replace obj "val" v;
            Value.Object obj
        | _ -> failwith "Runtime type error"
      )
  and string_op f () =
    binary_fun
      string_ty string_ty
      (fun a b -> match (a, b) with
        | (Value.Object self, Value.Object other) ->
            let self_v = BatHashtbl.find self "val" in
            let other_v = BatHashtbl.find other "val" in
            begin match self_v, other_v with
              | (Value.String x, Value.String y) ->
                let obj = string_uninitialized_object () in
                BatHashtbl.replace obj "val" (Value.String (f x y));
                Value.Object obj
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and string_unary f () =
    unary_fun
      string_ty
      (fun a -> match a with
        | (Value.Object self) ->
            let self_v = BatHashtbl.find self "val" in
            begin match self_v with
              | (Value.String x) -> f x
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and string_comp f () =
    binary_fun
      string_ty string_ty
      (fun a b -> match (a, b) with
        | (Value.Object self, Value.Object other) ->
            let self_v = BatHashtbl.find self "val" in
            let other_v = BatHashtbl.find other "val" in
            begin match self_v, other_v with
              | (Value.String x, Value.String y) ->
                Value.Bool (f x y)
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and string_add () = string_op ( ^ ) ()
  and string_repr () = string_unary
    (fun x ->
      let x = BatString.replace_chars
        (function | '"' -> "\\\"" | c -> String.make 1 c)
        x
      in
      !make_string ("\"" ^ x ^ "\"")) ()
  in
  make_string := (fun s ->
    let obj = string_uninitialized_object () in
    BatHashtbl.replace obj "val" (Value.String s);
    Value.Object obj);
  Value.build_object
    [("__call__", string_call ());
     ("__add__", string_add ());
     ("__repr__", string_repr ());
    ]


(* --------------------------- Common Functions --------------------------- *)
let __print_string__ =
  unary_fun
    string_ty
    (fun string_obj ->
      let raw_str = Value.get_object_field string_obj "val" in
      match raw_str with
      | Value.String s ->
          Printf.printf "%s\n" s;
          Value.None
      | _ -> failwith "Basis.print_str called on non String"
    )

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

(* -------------------------- Basis Environments -------------------------- *)
let val_env = Env.bind_pairs
  (List.map (fun (name, v) -> (name, Value.Const v))
  [
   (*("len", len);*)
   (*("head", head);*)
   (*("tail", tail);*)
   (*("cons", cons);*)

   (*("and", bool_bool_to_bool (&&));*)
   (*("or", bool_bool_to_bool (||));*)

   ("+", add);
   ("-", sub);
   ("*", mul);
   ("/", div);
   ("^", pow);

   ("==", eq);
   (*("<=", num_num_to_bool Z.leq);*)
   (*(">=", num_num_to_bool Z.geq);*)
   (*("<", num_num_to_bool Z.lt);*)
   (*(">", num_num_to_bool Z.gt);*)
   (*("!=", num_num_to_bool (fun a b -> not (Z.equal a b)));*)

   ("false", Value.Bool false);
   ("true", Value.Bool true);
   ("none", Value.None);

   ("__print_string__", __print_string__);

   ("callable", callable);

   ("Int", int_class);
   ("String", string_class);
  ]) Env.empty

let mut_env = Env.map (fun name -> false) val_env

let ty_env = Env.bind_pairs
  [
   (*("len", Type.fun_ty [Type.list_gen_ty] Type.int_ty);*)
   (*("head", Type.fun_ty [Type.list_gen_ty] Type.gen_var_ty);*)
   (*("tail", Type.fun_ty [Type.list_gen_ty] Type.list_gen_ty);*)
   (*("cons", Type.fun_ty [Type.gen_var_ty; Type.list_gen_ty] Type.list_gen_ty);*)

   (*("and", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);*)
   (*("or", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);*)

   ("+", add_ty);
   ("-", sub_ty);
   ("*", mul_ty);
   ("/", div_ty);
   ("^", pow_ty);

   ("==", eq_ty);
   (*("<=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);*)
   (*(">=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);*)
   (*("<", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);*)
   (*(">", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);*)
   (*("!=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);*)

   ("false", Type.bool_ty);
   ("true", Type.bool_ty);
   ("none", Type.none_ty);

   ("__print_string__", Type.fun_ty [string_ty] Type.none_ty);

   ("callable", Type.fun_ty [Type.gen_var_ty] Type.bool_ty);

   ("Int", int_class_ty);
   ("String", string_class_ty);
  ] Env.empty

let envs = (ty_env, mut_env, val_env)

let basis = "
def repr(x):
  return x.__repr__()

def print(x):
  __print_string__(repr(x))
"
