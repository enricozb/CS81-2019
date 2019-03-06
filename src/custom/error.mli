type error =
  | RuntimeError of string
  | NameError of string
  | TypeError of string

exception MythError of Loc.loc * error

val error : Loc.loc -> error -> 'a
val runtime_error : Loc.loc -> string -> 'a
val name_error : Loc.loc -> string -> 'a
val type_error : Loc.loc -> string -> 'a
val type_mismatch_error : Loc.loc ->
                          expected: string ->
                          provided: string -> 'a
val unify_error : Loc.loc -> string -> string -> 'a
val call_len_error : Loc.loc ->
                     fun_ty : string ->
                     expected : int ->
                     provided : int -> 'a

val call_error : Loc.loc -> string -> 'a

val print_error : Loc.loc -> error -> unit

