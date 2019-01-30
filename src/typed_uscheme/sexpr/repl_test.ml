open Lexer
open Parser
open Sexpr
open Repl

let repl () = 
  let f () lst = 
    List.iter 
      (fun e -> Printf.printf "\n%s\n" (string_of_expr e))
      lst
  in
    make_repl f ()

let _ = 
  try 
    repl () 
  with 
      End_of_file -> Printf.printf "  \n"

