open Lexer

let test_lex filename lexbuf =
  let rec iter () =
    try
      let tok = lex filename lexbuf in
        begin
          Printf.printf "%s\n" (string_of_token_loc tok);
          if tok <> TOK_EOF
            then iter ()
        end
    with Lexer_error (l, s) ->
      Printf.printf "LEXER ERROR: %s (%s)\n" s (Loc.string_of_loc_short l)
  in 
    iter ()

let _ =
  let args = Sys.argv in
    match args with
      | [| _; filename |] -> 
        let file = open_in filename in
        let lexbuf = Lexing.from_channel file in
          begin
            test_lex filename lexbuf ;
            close_in file
          end
      | _ ->
        let usagestr = Printf.sprintf "usage: %s filename\n" args.(0) in
          print_string usagestr

