module FieldMap = Map.Make(String)

type value =
  | None
  | Bool of bool
  | Int of Z.t
  | List of value list
  | Object of value FieldMap.t
  | Lambda of lambda * closure
  | Builtin of primop

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
  | List values -> "[" ^ string_of_list values ^ "]"
  | Object field_value_map ->
      let obj_str =
        String.concat ", " @@
          List.map
            (fun (field, value) ->
              field ^ ": " ^ (string_of_value value))
            (FieldMap.bindings field_value_map)
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

