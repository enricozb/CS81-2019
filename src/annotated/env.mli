(*
 * Values and environments.
 *)

(* An environment, recording both the global and local scope.
 *
 * Explicitly describing environment transformations was a bit painful in our
 * Imp interpreter, and it would be much more so here.  Instead we're going to
 * describe environment transformations implicitly using mutable environments.
 *
 * Note the curious semantics of Scheme environments: locations in the local
 * environment are mutable, but the local environment itself is not: changes in
 * one local scope can never be observed in other scopes.  The global scope,
 * meanwhile, is itself mutable: a change to the global environment can be
 * observed anywhere in the program.
 *)
type env

type value =
  | PrimFuncVal of (value list -> Loc.loc -> value)
  (* Note that user-defined functions capture an environment. *)
  | UserFuncVal of Syntax.id list * Syntax.expr * env
  | IntVal  of int
  | BoolVal of bool
  | PairVal of value * value
  (* Empty list. *)
  | NilVal
  (* Unit value--value of e.g. `set` and `while`. *)
  | UnitVal
  (* For temporarily putting in environments to allow recursion.
   * Any attempt to actually evaluate this will be an error. *)
  | Unspecified of string

(*
 * Operations on values.
 *)

(* Pretty-print the given value. *)
val string_of_val: value -> string

(* Determine whether a value counts as "true". *)
val truthy: Loc.loc -> value -> bool

(*
 * Operations on environments.
 *)

(* Lookup the given name in the environment, throwing an Error.UScheme_err if
 * not found. *)
val lookup: Loc.loc -> env -> Syntax.id -> value

(* Modify the value of the given variable in-place, throwing an
 * Error.UScheme_err if not found.  Modified in the local scope if it is
 * found there, otherwise in the global scope. *)
val set: Loc.loc -> env -> Syntax.id -> value -> unit

(* (Re-)bind the given variable in-place in the global scope. *)
val define: env -> Syntax.id -> value -> unit

(* Add a new local binding of the given variable to the environment. *)
val bind_local: env -> Syntax.id -> value -> env

(* Make a new environment with the given values in the global scope. *)
val make_env: (Syntax.id * value) list -> env

