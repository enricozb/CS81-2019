type value =
  | Int of int
  | List of value list
  | Lambda of lambda * closure

and lambda = (string list) * Ast.ast
and closure = unit -> value Env.env

let rec string_of_list = function
  | [] -> ""
  | [value] ->
      string_of_value value
  | value :: values ->
      (string_of_value value) ^ ", " ^ (string_of_list values)

and string_of_value = function
  | Int i -> string_of_int i
  | List values -> "[" ^ string_of_list values ^ "]"
  (* TODO *)
  | Lambda (lambda, closure) -> "<lambda>"

