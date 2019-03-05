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

val string_of_value : value -> string

