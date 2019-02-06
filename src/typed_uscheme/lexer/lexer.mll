{
  open Lexing
  open Parser

  type state = RECENT_NEWLINE | CODE
  let state = ref CODE

  let string_of_token = function
    | TYPE (_, t) -> t
    | NAME (_, s) -> s
    | NUMBER (_, i) -> string_of_int i
    | IF _ -> "if"
    | DEF _ -> "def"
    | COLON _ -> ":"
    | COMMA _ -> ","
    | EQUALS _ -> "="
    | MAPSTO _ -> "->"
    | LANGLE _ -> "<" | RANGLE _ -> ">"
    | LBRACE _ -> "{" | RBRACE _ -> "}"
    | LPAREN _ -> "(" | RPAREN _ -> ")"
    | LBRACK _ -> "[" | RBRACK _ -> "]"
    | OPERATOR (_, op) -> op
    | INDENT _ -> "INDENT"
    | DEDENT _ -> "DEDENT"
    | NEWLINE _ -> "NEWLINE"
    | DEINDENT (_, _) -> failwith "string_of_token on deindent"
    | EOF -> "EOF"

  let make_loc filename lexbuf =
    Loc.get_loc filename (lexeme_start_p lexbuf) (lexeme lexbuf)

  let name loc = function
    | "if" -> IF loc
    | "def" -> DEF loc
    | id -> NAME (loc, id)

  let print_token t = Printf.printf "%s\n" (string_of_token t)

  exception Exit
  exception Eof
  exception SyntaxError of string

  let line_number = ref 1

  let space_stack = Stack.create ()
  let _ = Stack.push 0 space_stack

  (* outputs INDENT, DEDENT, or DEINDENT tokens *)
  let count_indent loc count =
    if Stack.top space_stack = count then
      `Skip
    else if Stack.top space_stack < count then
      begin
        Stack.push count space_stack;
        `Token (INDENT loc)
      end
    else
      (* Pop from the stack until we get an equal indent *)
      let dedent_count = ref 0 in
      try
        while true do
          if Stack.top space_stack = count then
            raise Exit
          else
            incr dedent_count;
            ignore (Stack.pop space_stack);
        done;
        raise (SyntaxError "invalid deindent")
      with Exit -> `Token (DEINDENT (loc, !dedent_count))
}

let operators = ['.' '+' '-' '*' '/' '%' '<' '>' '=']

rule token filename = parse
  | ['\n'] {
    incr line_number;
    state := RECENT_NEWLINE;
    NEWLINE (make_loc filename lexbuf)
  }
  | [' ']+ { token filename lexbuf }
  | ':' { COLON (make_loc filename lexbuf) }
  | ',' { COMMA (make_loc filename lexbuf) }
  | ['A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
    TYPE ((make_loc filename lexbuf), id)
  }
  | ['a'-'z' '_']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
    name (make_loc filename lexbuf) id
  }
  | ['0'-'9']+ as num {
    NUMBER ((make_loc filename lexbuf), (int_of_string num))
  }
  | ['('] { LPAREN (make_loc filename lexbuf) }
  | [')'] { RPAREN (make_loc filename lexbuf) }
  | ['['] { LBRACK (make_loc filename lexbuf) }
  | [']'] { RBRACK (make_loc filename lexbuf) }
  | operators+ as op {
    match op with
    | "<" -> LANGLE (make_loc filename lexbuf)
    | ">" -> RANGLE (make_loc filename lexbuf)
    | "=" -> EQUALS (make_loc filename lexbuf)
    | "->" -> MAPSTO (make_loc filename lexbuf)
    | op -> OPERATOR ((make_loc filename lexbuf), op)
  }
  | _ as t { raise (SyntaxError ("unexpected token " ^ Char.escaped t)) }
  | eof { EOF }

and newline filename = parse
  | [' ']* as spaces {
    state := CODE;
    match count_indent (make_loc filename lexbuf) (String.length spaces) with
    | `Skip -> token filename lexbuf
    | `Token t -> t
  }

