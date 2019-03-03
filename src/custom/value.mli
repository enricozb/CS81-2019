type value =
  | Int of int
  | List of value list
  | Lambda of lambda

and lambda = (value list) -> value

val string_of_value : value -> string

