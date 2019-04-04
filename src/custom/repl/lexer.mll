{
  open Lexing
  open Parser

  type state =
    | REPL_DOUBLE_NEWLINE
    | RECENT_NEWLINE
    | CODE

  type mode =
    | REPL
    | FILE

  let state = ref CODE
  let mode = ref REPL

  let string_of_token = function
    | NAME (_, s) -> s
    | NUMBER (_, i) -> i
    | CHECKEXPECT _ -> "check_expect"
    | CHECKERROR _ -> "check_error"
    | CHECKTYPEERROR _ -> "check_type_error"
    | IMPORT _ -> "import"
    | LET _ -> "let"
    | MUT _ -> "mut"
    | IF _ -> "if"
    | ELSE _ -> "else"
    | WHILE _ -> "while"
    | BREAK _ -> "break"
    | CONTINUE _ -> "continue"
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
    | "let" -> LET loc
    | "mut" -> MUT loc
    | "if" -> IF loc
    | "else" -> ELSE loc
    | "while" -> WHILE loc
    | "break" -> BREAK loc
    | "continue" -> CONTINUE loc
    | "def" -> DEF loc
    | "return" -> RETURN loc
    | "and" -> OPERATOR (loc, "and")
    | "or" -> OPERATOR (loc, "or")
    | id -> NAME (loc, id)

  let print_token t = Printf.printf "%s\n" (string_of_token t)

  exception Exit
  exception Eof
  exception SyntaxError of Loc.loc

  let line_number = ref 1
  let paren_count = ref 0

  let next_line num_lines lexbuf =
    incr line_number;
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + num_lines
      }

  let rec count_newlines s =
    if String.length s = 0 then
      0
    else if s.[0] = '\n' then
      1 + count_newlines (String.sub s 1 (String.length s - 1))
    else
      count_newlines (String.sub s 1 (String.length s - 1))

  let space_stack = Stack.create ()
  let _ = Stack.push 0 space_stack

  let reset_state new_mode =
    mode := new_mode;
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
        raise (SyntaxError loc)
      with Exit -> `Token (DEINDENT (loc, !dedent_count))

  let make_loc filename lexbuf =
    Loc.get_loc filename (lexeme_start_p lexbuf) (lexeme lexbuf)
}

let operators = ['.' '+' '-' '*' '/' '%' '<' '>' '=' '^']

rule token filename = parse
  | ['#'] [^'\n']* ['\n'] {
    next_line 1 lexbuf;
    token filename lexbuf
  }
  | ['\n'] [' ' '\n']* ['\n'] | ['\n'] as newlines {
    next_line (count_newlines newlines) lexbuf;
    if !paren_count = 0 then begin
      if String.length newlines = 1 || !mode = FILE then
        state := RECENT_NEWLINE
      else
        state := REPL_DOUBLE_NEWLINE;

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
  | ['['] {
    incr paren_count;
    LBRACK (make_loc filename lexbuf)
  }
  | [']'] {
    decr paren_count;
    RBRACK (make_loc filename lexbuf)
  }
  | ['{'] {
    incr paren_count;
    LBRACE (make_loc filename lexbuf)
  }
  | ['}'] {
    decr paren_count;
    RBRACE (make_loc filename lexbuf)
  }
  | "!=" | operators+ as op {
    match op with
    (* These are to prevent <= and >= from becoming assignment operators *)
    | "!=" -> OPERATOR ((make_loc filename lexbuf), op)
    | "<=" -> OPERATOR ((make_loc filename lexbuf), op)
    | ">=" -> OPERATOR ((make_loc filename lexbuf), op)
    | "==" -> OPERATOR ((make_loc filename lexbuf), op)
    | "=" -> EQUALS (make_loc filename lexbuf)
    | "->" -> MAPSTO (make_loc filename lexbuf)
    | op ->
        if String.get op (String.length op - 1) = '=' then
          ASSIGNOPERATOR ((make_loc filename lexbuf),
            String.sub op 0 (String.length op - 1))
        else
          OPERATOR ((make_loc filename lexbuf), op)
  }
  | _ { raise (SyntaxError (make_loc filename lexbuf)) }
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

{

  let replicate (n : int) (el : 'a) =
    let rec iter curr = function
      | 0 -> curr
      | n -> iter (el :: curr) (n - 1)
    in iter [] n

  let token_cache (filename : string) =
    let cache = ref [] in
    fun lexbuf ->
      match !cache with
      | x::xs -> cache := xs; x
      | [] ->
          match !state with
          | CODE -> token filename lexbuf
          | RECENT_NEWLINE ->
              begin match newline filename lexbuf with
                | DEINDENT (loc, n) -> begin
                    cache := (replicate (n - 1) (DEDENT loc));
                    (DEDENT loc)
                  end
                | EOF ->
                  state := CODE;
                  begin match count_indent (make_loc filename lexbuf) 0 with
                  | `Skip -> EOF
                  | `Token (DEINDENT (loc, n)) ->
                    cache := (replicate (n - 1) (DEDENT loc)) @ [EOF];
                    (DEDENT loc)
                  | `Token t ->
                    cache := [EOF];
                    t
                  end
                | token -> token
              end
          (* this disgusting logic is so in a REPL, two successive NEWLINEs
           * will terminate a `compount_stmt`. But in files, we want things
           * that match ['\n'] [' ' '\n']* ['\n'] to be counted as a single
           * newline *)
          | REPL_DOUBLE_NEWLINE ->
              (* ask for count_indent on no spaces, push those DEDENTs out *)
              state := CODE;
              let token = match count_indent (make_loc filename lexbuf) 0 with
                | `Skip -> token filename lexbuf
                | `Token t -> t
              in
              begin match token with
                | DEINDENT (loc, n) ->
                  begin
                    cache := (replicate (n - 1) (DEDENT loc)) @ [(NEWLINE loc)];
                    (DEDENT loc)
                  end
                | token -> token
              end

}

