type value =
  | None
  | Bool of bool
  | Int of Z.t
  | List of value list
  | Lambda of lambda * closure
  | Builtin of primop

and lambda = (string list) * Ast.ast
and closure = unit -> value Env.env
and primop = value list -> Loc.loc -> value

let rec string_of_list = function
  | [] -> ""
  | [value] ->
      string_of_value value
  | value :: values ->
      (string_of_value value) ^ ", " ^ (string_of_list values)

and string_of_value = function
  | None -> "none"
  | Bool b -> string_of_bool b
  | Int i -> Z.to_string i
  | List values -> "[" ^ string_of_list values ^ "]"
  (* TODO *)
  | Lambda (lambda, closure) -> "<lambda>"
  | Builtin primop -> "<builtin>"

let truthy l = function
  | Bool b -> b
  | value ->
      Error.type_mismatch_error l
                          ~expected: "Bool"
                          ~provided: (string_of_value value)
