open Env
open Syntax
open Type
open Eval
open Basis

let () =
  let (prim_gamma, prim_env) = (Basis.prim_gamma, Basis.prim_env) in
  let basis_buf = Lexing.from_string Basis.nanoml_basis in
  let basis_xs = Parser.parse_many "<basis>" basis_buf in
  try
    let (gamma, env) = use_func (prim_gamma, prim_env) "<basis>" basis_xs in
      ignore (Repl.make_repl repl_func (gamma, env))
  with Error.NanoML_err e -> Error.print_err e; exit 0
     | End_of_file -> exit 0
