open Lexer
open Parser
open Sexpr

let prompt  = ">>> "
let prompt2 = "...   "

let print_prompt s = 
  print_string (if s = "" then prompt else prompt2)

let make_repl eval_print_fn env =
  let rec loop env s = 
    begin
      print_prompt s;
      let s' = s ^ read_line () ^ "\n" in
        try
          let lexbuf = Lexing.from_string s' in
          let es = parse_many "<repl>" lexbuf in
          let env' = eval_print_fn env es in
            loop env' ""
        with
          | Lexer_error (l, s) -> 
            begin
              Printf.printf "LEXER ERROR: %s (%s)\n\n%!" 
                s (Loc.string_of_loc_short l);
              loop env ""
            end
          | Parse_error (l, s) -> 
            begin
              Printf.printf "PARSE ERROR: %s (%s)\n\n%!" 
                s (Loc.string_of_loc_short l);
              loop env ""
            end
          | Parse_incomplete -> loop env s'
    end
  in 
    loop env ""

