open Env
open Syntax
open Type
open Eval
open Basis

let () =
  let (prim_gamma, prim_env) = (Basis.prim_gamma, Basis.prim_env) in
  (*let basis_buf = Lexing.from_string Basis.myth_basis in*)
  (*let basis_xs = Repl.parse_lexbuf "<basis>" basis_buf in*)
  (*let (gamma, env) = use_func (prim_gamma, prim_env) "<basis>" basis_xs in*)
  let (gamma, env) = (prim_gamma, prim_env) in
  try
    Repl.repl repl_func (gamma, env)
  with Error.NanoML_err e -> Error.print_err e; exit 0
     | End_of_file -> exit 0

