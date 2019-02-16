(* error.mli : error handling *)

type nanoml_error_tag =
    SyntaxError of string
  | CallError of int * int
  | RecError of string
  | NameError of string
  | TypeError of string * string
  | UseError of string * string
  | UnitTestError
  | ExecutionError of string

type nanoml_error_info = Loc.loc * nanoml_error_tag

exception NanoML_err of nanoml_error_info
exception NanoML_NeverHappen_err

val eprintf : ('a, out_channel, unit) format -> 'a
val print_loc : Loc.loc -> unit
val print_err : Loc.loc * nanoml_error_tag -> unit
val nanoml_err : Loc.loc -> nanoml_error_tag -> 'a
val name_err : Loc.loc -> string -> 'a
val call_err : Loc.loc -> expected:int -> found:int -> 'a
val rec_err : Loc.loc -> string -> 'a
val syntax_err : Loc.loc -> string -> 'a
val use_err : Loc.loc -> filename:string -> msg:string -> 'a
val unit_test_err : Loc.loc -> 'a
val type_err : Loc.loc -> expected:string -> found:string -> 'a
val execution_err : Loc.loc -> string -> 'a

