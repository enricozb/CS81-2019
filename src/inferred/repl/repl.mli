val parse_file : string -> Ast.ast list
val parse_lexbuf : string -> Lexing.lexbuf -> Ast.ast list

val repl : (Ast.ast -> 'a -> 'a) -> 'a -> unit

