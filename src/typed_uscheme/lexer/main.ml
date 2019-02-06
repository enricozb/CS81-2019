open Lexing
open Parser

module I =
  Parser.MenhirInterpreter

let replicate n el =
  let rec iter curr = function
    | 0 -> curr
    | n -> iter (el :: curr) (n - 1)
  in iter [] n

let lexfun_cache filename =
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

let print_wrap lexfun =
  fun lexbuf ->
    let token = lexfun lexbuf in
      Lexer.print_token token;
      token

(* Taken from https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-incremental/calc.ml *)
exception ParseIncomplete

let rec loop lexer_rule lexbuf (checkpoint : Ast.ast I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
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
      Printf.fprintf stderr
        "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf)
  | I.Accepted ast ->
    Printf.printf "%s\n" (Ast.string_of_ast ast)
  | I.Rejected ->
      (* The parser rejects this input. This cannot happen, here, because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false

let print_prompt str =
  Printf.printf "%s" str;
  flush stdout;
  ()

let readline () =
  (read_line ()) ^ "\n"

let parse filename =
  let rec loop_until_parse str =
    let lexbuf = Lexing.from_string str in
      try
        loop (lexfun_cache filename) lexbuf (Parser.Incremental.main lexbuf.lex_curr_p)
      with ParseIncomplete -> begin
            print_prompt "... ";
            loop_until_parse (str ^ (readline ()))
          end
  in begin
    print_prompt ">>> ";
    loop_until_parse (readline ())
  end

let _ =
  let filename = "__main__" in
	try
    let rec iter _ =
      parse filename;
      iter ()
    in iter ()
    with
    | Lexer.Eof ->
        exit 0
    | Lexer.SyntaxError msg ->
        Printf.fprintf stderr "%s\n" msg


