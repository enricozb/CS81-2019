val parse_file : string -> Ast.ast list
val parse_string : string -> string -> Ast.ast list

val repl : ([> `Ast of Ast.ast | `ParsingError of string * Loc.loc] -> 'a -> 'a)
            -> 'a
            -> unit

