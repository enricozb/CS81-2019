type state =
  | REPL_DOUBLE_NEWLINE
  | RECENT_NEWLINE
  | CODE

type mode =
  | REPL
  | FILE

val state : state ref

(* TODO : take tokens out of parser *)

val string_of_token : Parser.token -> string
val print_token : Parser.token -> unit

(* Lexing *)
exception SyntaxError of string * Loc.loc

val error : string -> Loc.loc -> 'a

val token_cache : string -> Lexing.lexbuf -> Parser.token
val newline : string -> Lexing.lexbuf -> Parser.token

val reset_state : mode -> unit

