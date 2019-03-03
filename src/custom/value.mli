type value =
  | Int of int
  | List of value list
  | Lambda of lambda * closure

and lambda = (string list) * Ast.ast
and closure = unit -> value Env.env

val string_of_value : value -> string

