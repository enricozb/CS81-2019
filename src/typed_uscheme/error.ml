(*
 * Exceptions used in the UScheme interpreter.
 *)

(* Error tags.  They correspond to the possible errors documented in error.mli.
*)
type uscheme_error_tag =
  | SyntaxError of string
  | CallError of int * int
  | NameError of string
  | TypeError of string * string
  | UseError of string * string
  | RuntimeError of string
  | UnitTestError

type uscheme_error_info = Loc.loc * uscheme_error_tag

exception UScheme_err of uscheme_error_info

(* Printf to stderr and then flush. *)
let eprintf s =
  let result = Printf.fprintf stderr s in
  flush stderr;
  result

(* Pretty-print a location to stderr. *)
let print_loc l = eprintf "    at %s\n" (Loc.string_of_loc_short l)

(* Print a uscheme error to stderr. *)
let print_err (loc, tag) = begin
  match tag with
  | NameError v ->
    eprintf "Unknown name: %s\n" v
  | CallError (expected, found) ->
    eprintf "Wrong number of arguments: ";
    eprintf "expected %d; found %d\n" expected found
  | SyntaxError msg ->
    eprintf "Syntax error: %s\n" msg;
  | UseError (filename, msg) ->
    eprintf "Could not open file %s (%s).\n" filename msg
  | UnitTestError ->
    eprintf "Cannot run unit test in the REPL.\n";
  | TypeError (expected, found) ->
    eprintf "Type error: expected %s; got %s.\n" expected found
  | RuntimeError msg ->
    eprintf "Runtime error: %s\n" msg
end;
  print_loc loc;
  flush stderr

(* Ugly hack... TODO: clean up by refactoring error handling. *)
let runtime_loc = { Loc.filename = "<basis>";
                    Loc.start_line = -1;
                    Loc.start_char = -1;
                    Loc.end_line = -1;
                    Loc.end_char = -1 }

let uscheme_err l i = raise (UScheme_err (l, i))
let name_err l v = uscheme_err l (NameError v)
let call_err l ~expected ~found = uscheme_err l (CallError (expected, found))
let syntax_err l msg = uscheme_err l (SyntaxError msg)
let use_err l ~filename ~msg = uscheme_err l (UseError (filename, msg))
let unit_test_err l = uscheme_err l UnitTestError
let type_err l ~expected ~found = uscheme_err l (TypeError (expected, found))
let runtime_err msg = uscheme_err runtime_loc (RuntimeError msg)

