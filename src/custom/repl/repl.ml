open Lexing
open Parser

module I =
  Parser.MenhirInterpreter

let replicate (n : int) (el : 'a) =
  let rec iter curr = function
    | 0 -> curr
    | n -> iter (el :: curr) (n - 1)
  in iter [] n

let lexfun_cache (filename : string) =
  let cache = ref [] in
  fun lexbuf ->
    match !cache with
    | x::xs -> cache := xs; x
    | [] ->
        match !Lexer.state with
        | Lexer.CODE -> Lexer.token filename lexbuf
        | Lexer.RECENT_NEWLINE -> begin
            match Lexer.newline filename lexbuf with
            | DEINDENT (loc, n) -> begin
                cache := (replicate (n - 1) (DEDENT loc));
                (DEDENT loc)
            end
            | token -> token
        end

(* Prints tokens as they are produced *)
let print_wrap lexfun =
  fun lexbuf ->
    let token = lexfun lexbuf in
      Lexer.print_token token;
      token

(* Taken from https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-incremental/calc.ml *)
exception ParseIncomplete
exception ParsingError of Loc.loc

let rec loop
        lexer_rule
        lexbuf
        (checkpoint : Ast.ast I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      (* TODO super weird use of EOF token... *)
      let token = lexer_rule lexbuf in
      begin
        match token with
        | EOF -> raise ParseIncomplete
        | _ ->
          let startp = lexbuf.lex_start_p
          and endp = lexbuf.lex_curr_p in
          let checkpoint = I.offer checkpoint (token, startp, endp) in
          loop lexer_rule lexbuf checkpoint
      end
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop lexer_rule lexbuf checkpoint
  | I.HandlingError _env ->
      let loc = Loc.get_loc "__repl__" (lexeme_start_p lexbuf) (lexeme lexbuf) in
      raise (ParsingError loc)
  | I.Accepted ast ->
      ast
  | I.Rejected ->
      (* The parser rejects this input. This cannot happen, here, because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false

let print_prompt str =
  Printf.printf "%s" str;
  flush stdout

let read_line_with_prompt (prompt : string) =
  print_prompt prompt;
  (read_line ()) ^ "\n"

let parse_repl () =
  let filename = "__main__" in
  let rec loop_until_parse str =
    Lexer.reset_state ();
    let lexbuf = Lexing.from_string str in
      try
        loop
          (lexfun_cache filename)
          (*(print_wrap @@ lexfun_cache filename)*)
          lexbuf
          (Parser.Incremental.single_input lexbuf.lex_curr_p)
      with ParseIncomplete -> begin
        loop_until_parse (str ^ (read_line_with_prompt "... "))
      end
  in begin
    loop_until_parse (read_line_with_prompt ">>> ")
  end

let parse_file (filename : string) =
  Lexer.reset_state ();
  let lexbuf = Lexing.from_channel (open_in filename) in
    try
      Parser.file_input (lexfun_cache filename) lexbuf
    with _ ->
      let loc = Loc.get_loc filename (lexeme_start_p lexbuf) (lexeme lexbuf) in
      raise (ParsingError loc)


let parse_lexbuf (name : string) lexbuf =
  Lexer.reset_state ();
    try
      Parser.file_input (lexfun_cache name) lexbuf
    with _ ->
      let loc = Loc.get_loc name (lexeme_start_p lexbuf) (lexeme lexbuf) in
      raise (ParsingError loc)

(* takes in a function that takes in the (`Ast || `ParsingError) read in and
 * any auxilliary data. Feeds in `aux` into f repeatedly after each parsing. *)
let repl f (aux : 'a) =
  let aux = ref aux in
  while true do
    try
        aux := f (`Ast (parse_repl ())) (!aux)
    with
    | ParsingError loc ->
        aux := f (`ParsingError loc) (!aux)
    | End_of_file ->
        exit 0
  done

