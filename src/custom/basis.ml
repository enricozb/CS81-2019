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
let le_ty, le = operator "__le__"
let lt_ty, lt = operator "__lt__"
let ge_ty, ge = operator "__ge__"
let gt_ty, gt = operator "__gt__"


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
     ("__add__", Type.fun_ty [int_ty] int_ty);
     ("__sub__", Type.fun_ty [int_ty] int_ty);
     ("__mul__", Type.fun_ty [int_ty] int_ty);
     ("__div__", Type.fun_ty [int_ty] int_ty);
     ("__pow__", Type.fun_ty [int_ty] int_ty);

     ("__eq__", Type.fun_ty [int_ty] Type.bool_ty);
     ("__le__", Type.fun_ty [int_ty] Type.bool_ty);
     ("__lt__", Type.fun_ty [int_ty] Type.bool_ty);
     ("__ge__", Type.fun_ty [int_ty] Type.bool_ty);
     ("__gt__", Type.fun_ty [int_ty] Type.bool_ty);

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

     ("__eq__", Type.fun_ty [string_ty] Type.bool_ty);
     ("__le__", Type.fun_ty [string_ty] Type.bool_ty);
     ("__lt__", Type.fun_ty [string_ty] Type.bool_ty);
     ("__ge__", Type.fun_ty [string_ty] Type.bool_ty);
     ("__gt__", Type.fun_ty [string_ty] Type.bool_ty);

     ("__len__", Type.fun_ty [] int_ty);
     ("__repr__", Type.fun_ty [] string_ty);
    ])
  and string_ty = Type.TyFold (Some ("String", []), inner_record)
  in
  string_ty

and list_ty =
  (* needs to be lazy to appease OCaml's restriction on let rec values *)
  let rec inner_record = lazy (Type.bare_record_ty
    [("val", Type.prim_list_gen_ty);
     ("__add__", Type.fun_ty [list_ty] list_ty);

     ("__len__", Type.fun_ty [] int_ty);

     ("__repr__", Type.fun_ty [] string_ty);
    ])
  and list_ty = Type.TyFold (Some ("List", [Type.gen_var_ty]), inner_record)
  in
  list_ty

let make_int = ref (fun _ -> failwith "Basis.make_int called before ready")
let make_string = ref (fun _ -> failwith "Basis.make_string called before ready")
let make_list = ref (fun _ -> failwith "Basis.make_string called before ready")


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
     ("__le__", Type.fun_ty [int_ty; int_ty] Type.bool_ty);
     ("__lt__", Type.fun_ty [int_ty; int_ty] Type.bool_ty);
     ("__ge__", Type.fun_ty [int_ty; int_ty] Type.bool_ty);
     ("__gt__", Type.fun_ty [int_ty; int_ty] Type.bool_ty);

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
    BatHashtbl.replace obj "__le__"  (bind_self (int_le ()) (Value.Object obj));
    BatHashtbl.replace obj "__lt__"  (bind_self (int_lt ()) (Value.Object obj));
    BatHashtbl.replace obj "__ge__"  (bind_self (int_ge ()) (Value.Object obj));
    BatHashtbl.replace obj "__gt__"  (bind_self (int_gt ()) (Value.Object obj));

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
  and int_le () = int_comp Z.leq ()
  and int_lt () = int_comp Z.lt ()
  and int_ge () = int_comp Z.geq ()
  and int_gt () = int_comp Z.gt ()

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
     ("__le__", int_le ());
     ("__lt__", int_lt ());
     ("__ge__", int_ge ());
     ("__gt__", int_gt ());

     ("__repr__", int_repr ());
    ]

(* -------------------------------- String -------------------------------- *)
let string_class_ty =
  Type.folded_record_ty None
    [("__call__", Type.prim_fun_ty [Type.prim_string_ty] string_ty);
     ("__add__", Type.fun_ty [string_ty; string_ty] string_ty);

     ("__eq__", Type.fun_ty [string_ty; string_ty] Type.bool_ty);
     ("__le__", Type.fun_ty [string_ty; string_ty] Type.bool_ty);
     ("__lt__", Type.fun_ty [string_ty; string_ty] Type.bool_ty);
     ("__ge__", Type.fun_ty [string_ty; string_ty] Type.bool_ty);
     ("__gt__", Type.fun_ty [string_ty; string_ty] Type.bool_ty);

     ("__len__", Type.fun_ty [string_ty] int_ty);
     ("__repr__", Type.fun_ty [string_ty] string_ty);
    ]

let string_class =
  let rec string_uninitialized_object () =
    let obj = BatHashtbl.of_list [("val", Value.String "")] in
    BatHashtbl.replace obj "__add__" (bind_self (string_add ()) (Value.Object obj));
    BatHashtbl.replace obj "__len__" (bind_self (string_len ()) (Value.Object obj));
    BatHashtbl.replace obj "__eq__" (bind_self (string_eq ()) (Value.Object obj));
    BatHashtbl.replace obj "__le__" (bind_self (string_le ()) (Value.Object obj));
    BatHashtbl.replace obj "__lt__" (bind_self (string_lt ()) (Value.Object obj));
    BatHashtbl.replace obj "__ge__" (bind_self (string_ge ()) (Value.Object obj));
    BatHashtbl.replace obj "__gt__" (bind_self (string_gt ()) (Value.Object obj));
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

  and string_eq () = string_comp (=) ()
  and string_le () = string_comp (<=) ()
  and string_lt () = string_comp (<) ()
  and string_ge () = string_comp (>=) ()
  and string_gt () = string_comp (>) ()

  and string_len () =
    string_unary
      (fun s -> !make_int (Z.of_int (String.length s))) ()
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

     ("__eq__", string_eq ());
     ("__le__", string_le ());
     ("__lt__", string_lt ());
     ("__ge__", string_ge ());
     ("__gt__", string_gt ());

     ("__len__", string_len ());

     ("__repr__", string_repr ());
    ]


(* -------------------------------- List[a] -------------------------------- *)
let list_class_ty =
  Type.folded_record_ty None
    [("__call__", Type.prim_fun_ty [Type.prim_list_gen_ty] list_ty);
     ("__add__", Type.fun_ty [list_ty; list_ty] list_ty);
     ("__len__", Type.fun_ty [list_ty] int_ty);
     ("__repr__", Type.fun_ty [list_ty] string_ty);
    ]

let list_class =
  let rec list_uninitialized_object () =
    let obj = BatHashtbl.of_list [("val", Value.List [])] in
    BatHashtbl.replace obj "__add__" (bind_self (list_add ()) (Value.Object obj));
    BatHashtbl.replace obj "__len__" (bind_self (list_len ()) (Value.Object obj));
    BatHashtbl.replace obj "__repr__" (bind_self (list_repr ()) (Value.Object obj));
    obj
  and list_call () =
    prim_unary_fun
      Type.prim_list_gen_ty
      (fun l -> match l with
        | (Value.List _) ->
            let obj = list_uninitialized_object () in
            BatHashtbl.replace obj "val" l;
            Value.Object obj
        | _ -> failwith "Runtime type error"
      )
  and list_op f () =
    binary_fun
      list_ty list_ty
      (fun a b -> match (a, b) with
        | (Value.Object self, Value.Object other) ->
            let self_v = BatHashtbl.find self "val" in
            let other_v = BatHashtbl.find other "val" in
            begin match self_v, other_v with
              | (Value.List x, Value.List y) ->
                let obj = list_uninitialized_object () in
                BatHashtbl.replace obj "val" (Value.List (f x y));
                Value.Object obj
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and list_unary f () =
    unary_fun
      list_ty
      (fun a -> match a with
        | (Value.Object self) ->
            let self_v = BatHashtbl.find self "val" in
            begin match self_v with
              | (Value.List x) -> f x
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and list_comp f () =
    binary_fun
      list_ty list_ty
      (fun a b -> match (a, b) with
        | (Value.Object self, Value.Object other) ->
            let self_v = BatHashtbl.find self "val" in
            let other_v = BatHashtbl.find other "val" in
            begin match self_v, other_v with
              | (Value.List x, Value.List y) ->
                Value.Bool (f x y)
              | _ -> failwith "Runtime type error"
            end
        | _ -> failwith "Runtime type error"
      )
  and list_add () = list_op ( @ ) ()
  and list_len () =
    list_unary
      (fun l -> !make_int (Z.of_int (List.length l))) ()
  and list_repr () = list_unary
    (fun x -> !make_string "[.. list repr not ready ..]") ()
  in
  make_list := (fun l ->
    let obj = list_uninitialized_object () in
    BatHashtbl.replace obj "val" (Value.List l);
    Value.Object obj);
  Value.build_object
    [("__call__", list_call ());
     ("__add__", list_add ());
     ("__repr__", list_repr ());
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
   (*("and", bool_bool_to_bool (&&));*)
   (*("or", bool_bool_to_bool (||));*)

   ("+", add);
   ("-", sub);
   ("*", mul);
   ("/", div);
   ("^", pow);

   ("==", eq);
   ("<=", le);
   ("<",  lt);
   (">=", ge);
   (">",  gt);
   (*("!=", num_num_to_bool (fun a b -> not (Z.equal a b)));*)

   ("false", Value.Bool false);
   ("true", Value.Bool true);
   ("none", Value.None);

   ("__print_string__", __print_string__);

   ("callable", callable);

   ("Int", int_class);
   ("String", string_class);
   ("List", list_class);
  ]) Env.empty

let mut_env = Env.map (fun name -> false) val_env

let ty_env = Env.bind_pairs
  [
   (*("and", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);*)
   (*("or", Type.fun_ty [Type.bool_ty; Type.bool_ty] Type.bool_ty);*)

   ("+", add_ty);
   ("-", sub_ty);
   ("*", mul_ty);
   ("/", div_ty);
   ("^", pow_ty);

   ("==", eq_ty);
   ("<=", le_ty);
   ("<",  lt_ty);
   (">=", ge_ty);
   (">",  gt_ty);
   (*("!=", Type.fun_ty [Type.int_ty; Type.int_ty] Type.bool_ty);*)

   ("false", Type.bool_ty);
   ("true", Type.bool_ty);
   ("none", Type.none_ty);

   ("__print_string__", Type.fun_ty [string_ty] Type.none_ty);

   ("callable", Type.fun_ty [Type.gen_var_ty] Type.bool_ty);

   ("Int", int_class_ty);
   ("String", string_class_ty);
   ("List", list_class_ty);
  ] Env.empty

let envs = (ty_env, mut_env, val_env)

let basis = "
def repr(x):
  return x.__repr__()

def len(x):
  return x.__len__()

def print(x):
  __print_string__(repr(x))
"
