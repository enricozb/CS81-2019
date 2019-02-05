open Parser

let filename = "__main__"

let replicate n el =
  let rec iter curr = function
    | 0 -> curr
    | n -> iter (el :: curr) (n - 1)
  in iter [] n

let lexfun_cache =
  let l = ref [] in
  fun lexbuf ->
    match !l with
    | x::xs -> l := xs; x
    | [] ->
        match !Lexer.state with
        | Lexer.CODE -> Lexer.token filename lexbuf
        | Lexer.RECENT_NEWLINE ->
          (* Indents or dedents need to be preceded by newlines *)
          match Lexer.newline filename lexbuf with
          | DEINDENT (loc, n) ->
              l := (replicate (n - 1) (DEDENT loc)); (DEDENT loc)
          | token -> token

let print_wrap lexfun =
  fun lexbuf ->
    let token = lexfun lexbuf in
      Lexer.print_token token;
      token


let _ =
	try
    flush stdout;
		let lexbuf = Lexing.from_channel stdin in
      let rec iter _ =
        Printf.printf ">>> ";
        flush stdout;
        let ast = Parser.main (lexfun_cache) lexbuf in
          Printf.printf "%s\n" (Ast.string_of_ast ast);
          flush stdout;
      in iter ()
		with Lexer.Eof -> exit 0

