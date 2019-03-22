
type state =
  | RECENT_NEWLINE
  | CODE

val state : state ref

(* Tokens (TODO) put in parser *)

val string_of_token : Parser.token -> string
val print_token : Parser.token -> unit

(* Lexing *)
exception SyntaxError of Loc.loc

val token_cache : string -> Lexing.lexbuf -> Parser.token
val newline : string -> Lexing.lexbuf -> Parser.token

val reset_state : unit -> unit

