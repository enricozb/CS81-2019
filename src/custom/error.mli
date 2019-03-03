type error =
  | NameError of string

exception MythError of Loc.loc * error

val error : Loc.loc -> error -> 'a
val name_error : Loc.loc -> string -> 'a

val print_error : Loc.loc -> error -> unit

