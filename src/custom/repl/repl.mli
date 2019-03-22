val parse_file : string -> Ast.ast list

val repl : ([> `Ast of Ast.ast | `ParsingError of Loc.loc] -> 'a -> 'a)
            -> 'a
            -> unit

