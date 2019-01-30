(* lexer.mll *)

{

open Lexing
open Loc

type token =
  | TOK_EOF
  | TOK_WHITE
  | TOK_LPAREN of loc
  | TOK_RPAREN of loc
  | TOK_LBRACKET of loc
  | TOK_RBRACKET of loc
  | TOK_INT of loc * int
  | TOK_ID of loc * string
  | TOK_QUOTE of loc

exception Lexer_error of loc * string

let string_of_token = function
  | TOK_EOF         -> "TOK_EOF"
  | TOK_WHITE       -> "TOK_WHITE"
  | TOK_LPAREN _    -> "TOK_LPAREN"
  | TOK_RPAREN _    -> "TOK_RPAREN"
  | TOK_LBRACKET _  -> "TOK_LBRACKET"
  | TOK_RBRACKET _  -> "TOK_RBRACKET"
  | TOK_INT (_, i)  -> "TOK_INT " ^ string_of_int i
  | TOK_ID (_, id)  -> "TOK_ID " ^ id
  | TOK_QUOTE _     -> "TOK_QUOTE"

let string_of_token_loc tok =
  let sol = string_of_loc_short in
    match tok with
      | TOK_EOF         -> "TOK_EOF"
      | TOK_WHITE       -> "TOK_WHITE"
      | TOK_LPAREN l    -> "TOK_LPAREN   (" ^ sol l ^ ")"
      | TOK_RPAREN l    -> "TOK_RPAREN   (" ^ sol l ^ ")"
      | TOK_LBRACKET l  -> "TOK_LBRACKET   (" ^ sol l ^ ")"
      | TOK_RBRACKET l  -> "TOK_RBRACKET   (" ^ sol l ^ ")"
      | TOK_INT (l, i)  -> Printf.sprintf "TOK_INT %d   (%s)" i (sol l)
      | TOK_ID (l, id)  -> Printf.sprintf "TOK_ID %s   (%s)" id (sol l)
      | TOK_QUOTE l     -> "TOK_QUOTE    (" ^ sol l ^ ")"

let make_loc filename lexbuf =
  get_loc filename (lexeme_start_p lexbuf) (lexeme lexbuf)

let loc_of_token = function
  | TOK_EOF         -> raise (Failure "eof token has no location")
  | TOK_WHITE       -> raise (Failure "whitespace token has no location")
  | TOK_LPAREN l    -> l
  | TOK_RPAREN l    -> l
  | TOK_LBRACKET l  -> l
  | TOK_RBRACKET l  -> l
  | TOK_INT (l, _)  -> l
  | TOK_ID (l, _)   -> l
  | TOK_QUOTE l     -> l

}

let whitespace = [' ' '\t']
let integer    = ['+' '-']? ['0' - '9']+

(* Allow all characters to be identifier characters except for:
 * - whitespace
 * - double quote
 * - single quote
 * - delimiters: ( ) [ ] { }
 * - semicolon
 * - backslash
 *
 * Also, identifiers can't start with a digit.
 *)
let id_chars   = ['A' - 'Z' 'a' - 'z'
                  '!' '#' '$' '%' '&' '*' '+' ',' '-' '.' '/'
                  ':' '<' '=' '>' '?' '@' '^' '_' '`' '|' '~']

let id_chars_follow = (id_chars | ['0' - '9'])

rule lex filename = parse
  | eof              { TOK_EOF      }
  | ';'[^'\n']*'\n'  { new_line lexbuf; TOK_WHITE }  (* single-line comments *)
  | whitespace+      { TOK_WHITE    }
  | '\n'             { new_line lexbuf; TOK_WHITE }
  | '('              { TOK_LPAREN   (make_loc filename lexbuf) }
  | ')'              { TOK_RPAREN   (make_loc filename lexbuf) }
  | '['              { TOK_LBRACKET (make_loc filename lexbuf) }
  | ']'              { TOK_RBRACKET (make_loc filename lexbuf) }
  | '\''             { TOK_QUOTE    (make_loc filename lexbuf) }

  (* integers *)
  | integer as lxm   { TOK_INT (make_loc filename lexbuf, int_of_string lxm) }

  (* identifiers *)
  | id_chars id_chars_follow* as lxm { TOK_ID (make_loc filename lexbuf, lxm) }

  | _ {
      raise (Lexer_error (make_loc filename lexbuf, "unrecognized token"))
    }

{
(* Nothing. *)
}
