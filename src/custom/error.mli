type error =
  | NameError of string
  | TypeError of string

exception MythError of Loc.loc * error

val error : Loc.loc -> error -> 'a
val name_error : Loc.loc -> string -> 'a
val type_error : Loc.loc -> string -> 'a
val call_len_error : Loc.loc ->
                     fun_ty : string ->
                     expected : int ->
                     provided : int -> 'a

val call_error : Loc.loc -> string -> 'a

val print_error : Loc.loc -> error -> unit

