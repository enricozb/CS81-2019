type value =
  | None
  | Bool of bool
  | Int of Z.t
  | String of string
  | List of value list
  | Object of name_value_map
  | Lambda of lambda * closure
  | Builtin of primop

and name_value_map = (string, value) BatHashtbl.t

and env_value =
  | Const of value
  | Mut of value ref

and lambda = (string list) * Ast.ast
and closure = unit -> env_value Env.env
and primop = value list -> Loc.loc -> value

let rec string_of_value = function
  | None -> "none"
  | Bool b -> string_of_bool b
  | Int i -> Z.to_string i
  | String s -> "\"" ^ s ^ "\""
  | List values -> "[" ^ string_of_list values ^ "]"
  | Object field_value_map ->
      let obj_str =
        String.concat ", " @@
          List.map
            (fun (field, value) ->
              field ^ ": " ^ (string_of_value value))
            (BatHashtbl.bindings field_value_map)
      in
      "{" ^ obj_str ^ "}"

  (* TODO *)
  | Lambda (lambda, closure) -> "<lambda>"
  | Builtin primop -> "<builtin>"

and string_of_list values =
  String.concat ", " (List.map string_of_value values)


let truthy l = function
  | Bool b -> b
  | value ->
      Error.type_mismatch_error l
                          ~expected: "Bool"
                          ~provided: (string_of_value value)


(* -------------------------- Constructing Values -------------------------- *)
let prim_zero_arity_fun f = Builtin (fun vals loc ->
  match vals with
  | [] -> f ()
  | _ ->
      Error.call_len_error loc
        ~fun_ty: "builitn"
        ~expected: 0
        ~provided: (List.length vals)
  )

(*let callable_object prim_func =*)
  (*let rec callable = Object*)
    (*(BatHashtbl.of_list [*)
      (*("__call__", callable);*)
      (*("~~call~~", prim_func)*)
    (*])*)

(*let base_object () = Object*)
  (*(BatHashtbl.of_list [*)
    (*("__repr__", callable_object (prim_zero_arity_fun (fun () -> "<object>")))*)
  (*])*)

let build_object fields = Object (BatHashtbl.of_list fields)

let get_object_field obj field =
  match obj with
  | Object fields ->
      begin match BatHashtbl.find_option fields field with
        | Some v -> v
        | None ->
            failwith ("Value.get_object_field can't find field '" ^ field ^ "'")
      end
  | _ ->
      failwith "Value.get_object_field called on non object"


