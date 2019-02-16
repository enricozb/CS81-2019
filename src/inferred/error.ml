(*
 * Exceptions used in the NanoML interpreter.
 *)

(* Error tags.  They correspond to the possible errors documented in error.mli.
*)
type nanoml_error_tag =
  | SyntaxError of string
  | CallError of int * int
  | RecError of string
  | NameError of string
  | TypeError of string * string
  | UseError of string * string
  | UnitTestError
  | ExecutionError of string

type nanoml_error_info = Loc.loc * nanoml_error_tag

exception NanoML_err of nanoml_error_info
exception NanoML_NeverHappen_err

(* Printf to stderr and then flush. *)
let eprintf s =
  let result = Printf.fprintf stderr s in
  flush stderr;
  result

(* Pretty-print a location to stderr. *)
let print_loc l = eprintf "    at %s\n" (Loc.string_of_loc_short l)

(* Print a nanoml error to stderr. *)
let print_err (loc, tag) = begin
  match tag with
  | NameError v ->
    eprintf "Unknown name: %s\n" v
  | CallError (expected, found) ->
    eprintf "Wrong number of arguments: ";
    eprintf "expected %d; found %d\n" expected found
  | RecError arg ->
    eprintf "Recursives cannot be bound on non-closure expressions: %s" arg
  | SyntaxError msg ->
    eprintf "Syntax error: %s\n" msg
  | UseError (filename, msg) ->
    eprintf "Could not open file %s (%s).\n" filename msg
  | UnitTestError ->
    eprintf "Cannot run unit test in the REPL.\n";
  | TypeError (expected, found) ->
    eprintf "Type error: cannot equate %s and %s.\n" expected found
  | ExecutionError msg ->
    eprintf "Error called: %s\n" msg
end;
  print_loc loc;
  flush stderr

let nanoml_err l i = raise (NanoML_err (l, i))
let name_err l v = nanoml_err l (NameError v)
let call_err l ~expected ~found = nanoml_err l (CallError (expected, found))
let rec_err l arg = nanoml_err l (RecError arg)
let syntax_err l msg = nanoml_err l (SyntaxError msg)
let use_err l ~filename ~msg = nanoml_err l (UseError (filename, msg))
let unit_test_err l = nanoml_err l UnitTestError
let type_err l ~expected ~found = nanoml_err l (TypeError (expected, found))
let execution_err l msg = nanoml_err l (ExecutionError msg)
