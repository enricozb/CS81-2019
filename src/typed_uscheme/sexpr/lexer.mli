type token =
  | TOK_EOF
  | TOK_WHITE
  | TOK_LPAREN   of Loc.loc
  | TOK_RPAREN   of Loc.loc
  | TOK_LBRACKET of Loc.loc
  | TOK_RBRACKET of Loc.loc
  | TOK_INT      of Loc.loc * int
  | TOK_ID       of Loc.loc * string
  | TOK_QUOTE    of Loc.loc

exception Lexer_error of Loc.loc * string

val string_of_token : token -> string 
val string_of_token_loc : token -> string 
val make_loc : string -> Lexing.lexbuf -> Loc.loc 
val loc_of_token : token -> Loc.loc
val lex : string -> Lexing.lexbuf -> token

