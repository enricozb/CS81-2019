{
  open Parser

  type state = RECENT_NEWLINE | CODE
  let state = ref CODE

  let string_of_token = function
    | TYPE t -> t
    | IDENTIFIER s -> s
    | NUMBER i -> string_of_int i
    | IF -> "if"
    | DEF -> "def"
    | COLON -> ":"
    | COMMA -> ","
    | EQUALS -> "="
    | MAPSTO -> "->"
    | LANGLE -> "<" | RANGLE -> ">"
    | LBRACE -> "{" | RBRACE -> "}"
    | LPAREN -> "(" | RPAREN -> ")"
    | LBRACK -> "[" | RBRACK -> "]"
    | OPERATOR op -> op
    | INDENT -> "INDENT"
    | DEDENT -> "DEDENT"
    | DEINDENT n -> failwith "string_of_token on deindent"
    | NEWLINE -> "NEWLINE"
    | EOF -> "EOF"

  let keyword_table = Hashtbl.create 2
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [("if", IF);
               ("def", DEF);]

  let print_token t = Printf.printf "%s\n" (string_of_token t)

  exception Exit
  exception Eof
  exception SyntaxError of string

  let line_number = ref 1

  let space_stack = Stack.create ()
  let _ = Stack.push 0 space_stack

  (* outputs INDENT, DEDENT, or DEINDENT tokens *)
  let count_indent count =
    if Stack.top space_stack = count then
      `Skip
    else if Stack.top space_stack < count then
      begin
        Stack.push count space_stack;
        `Token INDENT
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
      with Exit -> `Token (DEINDENT !dedent_count)
}

let operators = ['.' '+' '-' '*' '/' '%' '<' '>' '=']

rule token = parse
  | ['\n'] {
    incr line_number;
    state := RECENT_NEWLINE;
    NEWLINE
  }
  | [' ']+ { token lexbuf }
  | ':' { COLON }
  | ',' { COMMA }
  | ['A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
    TYPE id
  }
  | ['a'-'z' '_']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
    try
      Hashtbl.find keyword_table id
    with Not_found ->
      NAME id
  }
  | ['"'] [^'"']* ['"'] as str {
    STRING (String.sub str 1 ((String.length str) - 2))
  }
  | ['0'-'9']+ as num { NUMBER (int_of_string num) }
  | ['('] { LPAREN } | [')'] { RPAREN }
  | ['['] { LBRACK } | [']'] { RBRACK }
  | operators+ as op {
    match op with
    | "<" -> LANGLE
    | ">" -> RANGLE
    | "=" -> EQUALS
    | "->" -> MAPSTO
    | op -> OPERATOR op
  }
  | _ as t { raise (SyntaxError ("unexpected token " ^ Char.escaped t)) }
  | eof { EOF }

and newline = parse
  | [' ']* as spaces {
    state := CODE;
    match count_indent (String.length spaces) with
    | `Skip -> token lexbuf
    | `Token t -> t
  }
