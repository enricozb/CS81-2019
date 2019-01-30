exception Parse_error of Loc.loc * string
exception Parse_incomplete

(** Parse a single S-expression.  The first argument is the filename. 
    Return None on EOF. *)
val parse : string -> Lexing.lexbuf -> Sexpr.expr option

(** Parse multiple S-expressions, separated by whitespace.  
    The first argument is the filename. *)
val parse_many : string -> Lexing.lexbuf -> Sexpr.expr list

(** Parse S-expressions from a file. 
    The first argument is the filename. *)
val parse_file : string -> Sexpr.expr list

