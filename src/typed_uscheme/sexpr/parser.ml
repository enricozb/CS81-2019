open Lexer
open Loc
open Sexpr

exception Parse_error of Loc.loc * string
exception Parse_incomplete

(*
 * Significant whitespace algorithm:
 *
 * Any atom (integer, identifier) or any open delimiter must be preceded by
 * one of:
 *   -- whitespace
 *   -- an open delimiter (left paren, left square bracket)
 *)

(* Parse a single S-expression.
 * The 'prev_ok' argument is true if the previous token is whitespace, a quote,
 * or an open delimiter (left paren, left bracket). *)
let parse filename lexbuf =
  let rec parse1 prev_ok =
    match lex filename lexbuf with
      | TOK_EOF   -> `EOF
      | TOK_WHITE -> parse1 true  (* skip whitespace *)
      | TOK_INT (loc, i) ->
        if prev_ok
        then `Expr (Int (loc, i))
        else raise
            (Parse_error
               (loc, "missing whitespace before integer"))
      | TOK_ID (loc, id) ->
        if prev_ok
        then `Expr (Id (loc, id))
        else raise
            (Parse_error
               (loc, "missing whitespace before identifier"))
      | TOK_RPAREN   loc -> `RParen loc
      | TOK_RBRACKET loc -> `RBracket loc
      | TOK_LPAREN   loc ->
        if prev_ok
        then `Expr (parse_paren_list loc true [])
        else raise
            (Parse_error
               (loc, "missing whitespace before left parenthesis"))
      | TOK_LBRACKET loc ->
        if prev_ok
        then `Expr (parse_bracket_list loc true [])
        else raise
            (Parse_error
               (loc, "missing whitespace before left square bracket"))
      | TOK_QUOTE loc ->
        if prev_ok
        then `Expr (parse_quote loc)
        else raise
            (Parse_error
               (loc, "missing whitespace before quote"))
  and parse_paren_list start_loc prev_ok results =
    match parse1 prev_ok with
      | `EOF          -> raise Parse_incomplete
      | `RParen loc   -> List (span start_loc loc, List.rev results)
      | `RBracket loc -> raise
                           (Parse_error
                              (loc, "left parenthesis matched with right square bracket"))
      | `Expr e       -> parse_paren_list start_loc false (e :: results)
  and parse_bracket_list start_loc prev_ok results =
    match parse1 prev_ok with
      | `EOF          -> raise Parse_incomplete
      | `RBracket loc -> List (span start_loc loc, List.rev results)
      | `RParen loc   -> raise
                           (Parse_error
                              (loc, "left square bracket matched with right parenthesis"))
      | `Expr e       -> parse_bracket_list start_loc false (e :: results)
  and parse_quote start_loc =
    match parse1 true with
      | `EOF          -> raise Parse_incomplete
      | `RParen loc   -> raise
                           (Parse_error (loc, "unexpected right parenthesis"))
      | `RBracket loc -> raise
                           (Parse_error (loc, "unexpected right square bracket"))
      | `Expr e       ->
        let quote_loc = span start_loc (loc_of_expr e) in
          List (quote_loc, [Id (start_loc, "quote"); e])
  in
    match parse1 true with
      | `Expr e       -> Some e
      | `EOF          -> None
      | `RParen loc   -> raise
                           (Parse_error (loc, "unexpected right parenthesis"))
      | `RBracket loc -> raise
                           (Parse_error (loc, "unexpected right square bracket"))

(* Parse multiple S-expressions, separated by whitespace. *)
let parse_many filename lexbuf =
  let note = "\nNOTE: S-expressions must be separated by " ^
             "at least one whitespace character.\n"
  in
  let rec iter results =
    match parse filename lexbuf with
      | None   -> List.rev results
      | Some s -> iter2 (s :: results)
  and iter2 results =
   match lex filename lexbuf with
     | TOK_EOF   -> List.rev results
     | TOK_WHITE -> iter results
     | TOK_QUOTE l -> raise
         (Parse_error
            (l, "unexpected extra single quote character " ^
                "after S-expression;" ^ note))
     | TOK_LPAREN l -> raise
         (Parse_error
            (l, "unexpected extra '(' character " ^
                "after S-expression;" ^ note))
     | TOK_RPAREN l -> raise
         (Parse_error
            (l, "unexpected extra ')' character " ^
                "after S-expression"))
     | TOK_LBRACKET l -> raise
         (Parse_error
            (l, "unexpected extra '[' character " ^
                "after S-expression;" ^ note))
     | TOK_RBRACKET l -> raise
         (Parse_error
            (l, "unexpected extra ']' character " ^
                "after S-expression"))
     | t -> raise
         (Parse_error
            (loc_of_token t,
             "non-whitespace character after S-expression;" ^ note))
  in
    iter []

(* Parse an entire file of S-expressions. *)
let parse_file filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
    parse_many filename lexbuf

