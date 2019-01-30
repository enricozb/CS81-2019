open Lexer
open Parser
open Sexpr

let test_parse1 filename lexbuf =
  try
    match parse filename lexbuf with
      | None -> false
      | Some e ->
        begin
          Printf.printf "%s\n" (string_of_expr_loc e);
          true
        end
  with
    | Lexer_error (l, s) ->
      begin
        Printf.printf "LEXER ERROR: %s (%s)\n\n"
          s (Loc.string_of_loc_short l);
        false
      end
    | Parse_error (l, s) ->
      begin
        Printf.printf "PARSE ERROR: %s (%s)\n\n"
          s (Loc.string_of_loc_short l);
        false
      end
    | Parse_incomplete ->
      begin
        Printf.printf "INCOMPLETE PARSE\n\n";
        false
      end

let test_parser filename lexbuf =
  let rec iter () =
    if test_parse1 filename lexbuf
    then iter ()
    else ()
  in
    iter ()

let _ =
  let args = Sys.argv in
    match args with
      | [| _; filename |] ->
        let file = open_in filename in
        let lexbuf = Lexing.from_channel file in
          begin
            test_parser filename lexbuf;
            close_in file
          end
      | _ ->
        let usagestr = Printf.sprintf "usage: %s filename\n" args.(0) in
          print_string usagestr

