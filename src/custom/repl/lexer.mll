{
  open Lexing
  open Parser

  type state = RECENT_NEWLINE | CODE
  let state = ref CODE

  let string_of_token = function
    | NAME (_, s) -> s
    | NUMBER (_, i) -> i
    | CHECKEXPECT _ -> "check_expect"
    | CHECKERROR _ -> "check_error"
    | CHECKTYPEERROR _ -> "check_type_error"
    | IMPORT _ -> "import"
    | IF _ -> "if"
    | ELSE _ -> "else"
    | WHILE _ -> "while"
    | DEF _ -> "def"
    | RETURN _ -> "return"
    | DOT _ -> "."
    | COLON _ -> ":"
    | QUOTE _ -> "'"
    | COMMA _ -> ","
    | EQUALS _ -> "="
    | MAPSTO _ -> "->"
    | LANGLE _ -> "<" | RANGLE _ -> ">"
    | LBRACE _ -> "{" | RBRACE _ -> "}"
    | LPAREN _ -> "(" | RPAREN _ -> ")"
    | LBRACK _ -> "[" | RBRACK _ -> "]"
    | OPERATOR (_, op) -> op
    | ASSIGNOPERATOR (_, op) -> op
    | INDENT _ -> "INDENT"
    | DEDENT _ -> "DEDENT"
    | NEWLINE _ -> "NEWLINE"
    | DEINDENT (_, _) -> failwith "string_of_token on deindent"
    | EOF -> "EOF"

  let name loc = function
    | "check_expect" -> CHECKEXPECT loc
    | "check_error" -> CHECKERROR loc
    | "check_type_error" -> CHECKTYPEERROR loc
    | "import" -> IMPORT loc
    | "if" -> IF loc
    | "else" -> ELSE loc
    | "while" -> WHILE loc
    | "def" -> DEF loc
    | "return" -> RETURN loc
    | id -> NAME (loc, id)

  let print_token t = Printf.printf "%s\n" (string_of_token t)

  exception Exit
  exception Eof
  exception SyntaxError of string

  let line_number = ref 1
  let paren_count = ref 0

  let next_line lexbuf =
    incr line_number;
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let space_stack = Stack.create ()
  let _ = Stack.push 0 space_stack

  let reset_state () =
    state := CODE;
    paren_count := 0;
    Stack.clear space_stack;
    Stack.push 0 space_stack

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

  let make_loc filename lexbuf =
    Loc.get_loc filename (lexeme_start_p lexbuf) (lexeme lexbuf)
}

let operators = ['.' '+' '-' '*' '/' '%' '<' '>' '=' '^']

rule token filename = parse
  | ['#'] [^'\n']* ['\n'] {
    next_line lexbuf;
    token filename lexbuf
  }
  | ['\n'] {
    next_line lexbuf;
    if !paren_count = 0 then begin
      state := RECENT_NEWLINE;
      NEWLINE (make_loc filename lexbuf)
    end else
      token filename lexbuf
  }
  | [' ']+ { token filename lexbuf }
  | ':' { COLON (make_loc filename lexbuf) }
  | '.' { DOT (make_loc filename lexbuf) }
  | ',' { COMMA (make_loc filename lexbuf) }
  | ''' { QUOTE (make_loc filename lexbuf) }
  | ['a'-'z' '_']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
    name (make_loc filename lexbuf) id
  }
  | ['-']?['0'-'9']+ as num {
    NUMBER ((make_loc filename lexbuf), num)
  }
  | ['('] {
    incr paren_count;
    LPAREN (make_loc filename lexbuf)
  }
  | [')'] {
    decr paren_count;
    RPAREN (make_loc filename lexbuf)
  }
  | ['['] { LBRACK (make_loc filename lexbuf) }
  | [']'] { RBRACK (make_loc filename lexbuf) }
  | operators+ as op {
    match op with
    (* These are to prevent <= and >= from becoming assignment operators *)
    | "<=" -> OPERATOR ((make_loc filename lexbuf), "<=")
    | ">=" -> OPERATOR ((make_loc filename lexbuf), ">=")
    | "==" -> OPERATOR ((make_loc filename lexbuf), "==")
    | "=" -> EQUALS (make_loc filename lexbuf)
    | "->" -> MAPSTO (make_loc filename lexbuf)
    | op ->
        if String.get op (String.length op - 1) = '=' then
          ASSIGNOPERATOR ((make_loc filename lexbuf),
            String.sub op 0 (String.length op - 1))
        else
          OPERATOR ((make_loc filename lexbuf), op)
  }
  | _ as t { raise (SyntaxError ("unexpected token " ^ Char.escaped t)) }
  | eof { EOF }

and newline filename = parse
  (* to allow empty and incomplete blocks in REPL *)
  | [' ']* eof { EOF }
  | [' ']* as spaces {
    state := CODE;
    match count_indent (make_loc filename lexbuf) (String.length spaces) with
    | `Skip -> token filename lexbuf
    | `Token t -> t
  }

