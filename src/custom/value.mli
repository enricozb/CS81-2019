type value =
  | None
  | Bool of bool
  | Int of Z.t
  | String of string
  | List of value list
  | Object of name_value_map
  | Lambda of lambda * closure
  | Builtin of primop

and name_value_map = (string, value Lazy.t) BatHashtbl.t

and env_value =
  | Const of value
  | Mut of value ref

and lambda = (string list) * Ast.ast
and closure = unit -> env_value Env.env
and primop = value list -> Loc.loc -> value

val string_of_value : value -> string

val truthy : Loc.loc -> value -> bool

val build_object : (string * value Lazy.t) list -> value
val get_object_field : value -> string -> value
val get_object_field_option : value -> string -> value option
val set_object_field : value -> string -> value Lazy.t -> unit
val get_func_from_callable : value -> value

val get_fields : value -> string list

val base_object : unit -> value
val callable_object : value Lazy.t -> value


