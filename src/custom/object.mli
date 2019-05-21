val build_object : (string * Value.value Lazy.t) list -> Value.value
val get_object_field : Value.value -> string -> Value.value
val get_object_field_option : Value.value -> string -> Value.value option
val set_object_field : Value.value -> string -> Value.value Lazy.t -> unit
val get_func_from_callable : Value.value -> Value.value

val get_fields : Value.value -> string list

val base_object : unit -> Value.value
val callable_object : ?name:(string option) -> Value.value Lazy.t -> Value.value
val is_callable : Value.value -> bool

(* initialized in basis.ml *)
val function_class : (unit -> Value.value) ref
val make_int : (Z.t -> Value.value) ref
val make_string : (string -> Value.value) ref
val make_list : (Value.value list -> Value.value) ref
