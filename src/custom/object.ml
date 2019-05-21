(* used here for callable construction *)
let function_class = ref (fun () -> failwith "Object.function_class uninitialized")

(* -------------------- Convenience Object Constructors -------------------- *)
(* these are initialized in basis.ml *)
let make_int = ref (fun _ -> failwith "Object.make_int uninitialized")
let make_string = ref (fun _ -> failwith "Object.make_string uninitialized")
let make_list = ref (fun _ -> failwith "Object.make_string uninitialized")

(* ------------------------ Accessing Object Fields ------------------------ *)

let set_object_field obj field value =
  match obj with
  | Value.Object fields ->
    BatHashtbl.replace fields field value
  | _ ->
      failwith "Object.set_object_field called on non object"

let get_object_field obj field =
  match obj with
  | Value.Object fields ->
      begin match BatHashtbl.find_option fields field with
        | Some v -> Lazy.force v
        | None ->
            failwith ("Object.get_object_field can't find field '" ^ field ^ "'")
      end
  | _ ->
      failwith ("Object.get_object_field called on non object (" ^ field ^ ")")

let get_object_field_option obj field =
  match obj with
  | Value.Object fields ->
      begin match BatHashtbl.find_option fields field with
        | Some v -> Some (Lazy.force v)
        | None -> None
      end
  | _ -> None

let get_fields obj =
  match obj with
  | Value.Object fields -> List.map fst (BatHashtbl.to_list fields)
  | _ -> failwith "Value.get_fields called on non-object"

let get_func_from_callable callable =
  get_object_field (get_object_field callable "__call__") "~~call~~"

let is_callable obj =
  match get_object_field_option obj "__call__" with
  | None -> false
  | Some obj ->
    begin match get_object_field_option obj "~~call~~" with
    | None -> false
    | Some (Value.Lambda _)
    | Some (Value.Builtin _) -> true
    | _ ->
        failwith "Object has ~~call~~ but isn't Value.Lambda nor Value.Builtin"
    end

(* ------------------------- Constructing Objects ------------------------- *)

let rec base_object () = Value.Object (BatHashtbl.create 0)

(* TODO: depend on base_object *)
and build_object fields = Value.Object (BatHashtbl.of_list fields)

and callable_object ?(name=None) prim_func =
  let rec obj = base_object () in
  set_object_field obj "__call__" (lazy obj);
  set_object_field obj "~~call~~" prim_func;
  begin match name with
    | None -> set_object_field obj "__name__" (lazy (!make_string ""))
    | Some name -> set_object_field obj "__name__" (lazy (!make_string name))
  end;
  set_object_field obj "__class__" (lazy (!function_class ()));
  obj

and zero_ary_fun f = callable_object (lazy (
  Value.Builtin (fun vals loc ->
    match vals with
    | [] -> f ()
    | _ ->
        Error.call_len_error loc
          ~fun_ty: "builitn"
          ~expected: 0
          ~provided: (List.length vals)
  )
))
