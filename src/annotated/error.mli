(*
 * Interface for propagating and handling UScheme user errors.
 *)

type uscheme_error_info

exception UScheme_err of uscheme_error_info

(*
 * Printing error info to stderr.
 *)

val eprintf: ('a, out_channel, unit) format -> 'a

val print_loc: Loc.loc -> unit

val print_err: uscheme_error_info -> unit

(*
 * Functions for throwing UScheme_errs.
 *)

(* The given name is not bound. *)
val name_err: Loc.loc -> string -> 'a

(* A function call had the wrong number of arguments. *)
val call_err: Loc.loc -> expected: int -> found: int -> 'a

(* Syntax analysis failed for the given reason. *)
val syntax_err: Loc.loc -> string -> 'a

(* A `use` could not open the given file for the given reason. *)
val use_err: Loc.loc -> filename: string -> msg: string -> 'a

(* A unit test was entered interactively. *)
val unit_test_err: Loc.loc -> 'a

(* Expected something like `expected`; got something of type `found`. *)
val type_err: Loc.loc -> expected: string -> found: string -> 'a

(* Unsafe operations at runtime--e.g. division by zero. *)
val runtime_err: string -> 'a
