type error =
  | RuntimeError of string
  | NameError of string
  | BindError of string
  | TypeError of string
  | SyntaxError of string

exception MythError of Loc.loc * error

val print_error : Loc.loc -> error -> unit

val error : Loc.loc -> error -> 'a
val runtime_error : Loc.loc -> string -> 'a
val name_error : Loc.loc -> string -> 'a
val bind_error : Loc.loc -> string -> 'a
val type_error : Loc.loc -> string -> 'a
val syntax_error : Loc.loc -> string -> 'a


val type_mismatch_error : Loc.loc ->
                          expected: string ->
                          provided: string -> 'a
val unify_error : Loc.loc -> string -> string -> 'a
val call_len_error : Loc.loc ->
                     fun_ty : string ->
                     expected : int ->
                     provided : int -> 'a
val call_error : Loc.loc -> string -> 'a


val unreachable_code_error : Loc.loc -> 'a
val return_outside_def : Loc.loc -> 'a

