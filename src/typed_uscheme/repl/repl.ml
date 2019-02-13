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
        | Lexer.RECENT_NEWLINE ->
          (* Indents or dedents need to be preceded by newlines *)
          match Lexer.newline filename lexbuf with
          | DEINDENT (loc, n) ->
              cache := (replicate (n - 1) (DEDENT loc)); (DEDENT loc)
          | token -> token

(* Prints tokens as they are produced *)
let print_wrap lexfun =
  fun lexbuf ->
    let token = lexfun lexbuf in
      Lexer.print_token token;
      token

(* Taken from https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-incremental/calc.ml *)
exception ParseIncomplete
exception SyntaxError of int

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
      raise (SyntaxError (Lexing.lexeme_start lexbuf))
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

(* TODO use `filename` usefully. Right now this always parses from stdin *)
let parse (filename : string) =
  let rec loop_until_parse str =
    Lexer.reset_state ();
    let lexbuf = Lexing.from_string str in
      try
        loop
          (lexfun_cache filename)
          (* (print_wrap @@ lexfun_cache filename) *)
          lexbuf
          (Parser.Incremental.main lexbuf.lex_curr_p)
      with ParseIncomplete -> begin
        loop_until_parse (str ^ (read_line_with_prompt "... "))
      end
  in begin
    loop_until_parse (read_line_with_prompt ">>> ")
  end

(* takes in a function that takes in the Ast read in and any auxilliary
 * data. Feeds in `aux` into f repeatedly after each parsing. *)
let repl (f : Ast.ast -> 'a -> 'a) (aux : 'a) =
	try
    let aux = ref aux in
    while true do
      aux := f (parse "__main__") (!aux)
    done
  with
  | Lexer.SyntaxError msg ->
      Printf.fprintf stderr "%s\n" msg
  | End_of_file ->
      exit 0

